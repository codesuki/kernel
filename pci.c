#include "pci.h"
#include "io.h"
#include "memory.h"
#include "print.h"
#include "rtl8139.h"

// PCI vendor
// - device
// 0x8086 Intel
// - 0x1237 440FX - 82441FX PMC [Natoma]
// - 0x7000 82371SB PIIX3 ISA [Natoma/Triton II]
// 0x1234 Bochs
// - 0x1111 Bochs Graphic Adapter
// 0x10ec Realtek
// - 0x8139 RTL-8100/8101L/8139 PCI Fast Ethernet Adapter

// PCI class
// - subclass
// 0x2 Network controller
// - 0x0 Ethernet controller
// 0x3 Display controller
// - 0x0 VGA compatible
// 0x6 Bridge
// - 0x0 Host Bridge
// - 0x1 ISA Bridge
//

// PCI has a configuration space. This space can be read to learn about all PCI
// devices in the computer. A system can have up to 256 busses. Each bus can
// have up to 32 devices. Each device can have up to 8 functions. Each function
// has 256 bytes of space available for configuration. The first 64 bytes are
// standardized.
//
// The OS or BIOS need to configure each functions Base Address Register (BAR)
// to be able to talk to it.
//
// If a read of function 0 returns all bits enabled (0xFFFFFFFF) then the device
// does not exist.
//
// Writing all ones to a BAR will cause the device to respond with the size it
// requires for it's configuration. Each device can have up to 6 BARs that are
// between 16 bytes and 2gb, e.g. a framebuffer. The addresses are often 32 bit
// so need to be under 4gb.
//
// Maybe busses can only be found through bridges that are connected to other
// busses. So if bus 0 does not have a bridge there is just one bus. This is my
// assumption.

u8* pci_vendor_name(u32 vendor) {
  switch (vendor) {
    case 0x8086:
      return "Intel";
  }
  return "unknown";
}

u8* pci_device_name(u32 vendor, u32 device) {
  switch (vendor) {
    case 0x8086:
      switch (device) {
	case 0x7010:
	  return "PIIX3";
      }
  }
  return "unknown";
}

u8* pci_base_class(u32 class) {
  switch (class) {
    case 0x1:
      return "Mass storage device";
  }
  return "unknown";
}

u8* pci_sub_class(u32 class) {
  switch (class) {
    case 0x1:
      return "IDE controller";
  }
  return "unknown";
}

#define ATA_PORT_COMMAND_PRIMARY 0x01f0
#define ATA_PORT_COMMAND_PRIMARY_DATA (ATA_PORT_COMMAND_PRIMARY + 0)
#define ATA_PORT_COMMAND_PRIMARY_ERROR (ATA_PORT_COMMAND_PRIMARY + 1)
#define ATA_PORT_COMMAND_PRIMARY_SECTOR_COUNT (ATA_PORT_COMMAND_PRIMARY + 2)
#define ATA_PORT_COMMAND_PRIMARY_SECTOR (ATA_PORT_COMMAND_PRIMARY + 3)
#define ATA_PORT_COMMAND_PRIMARY_CYLINDER_LOW (ATA_PORT_COMMAND_PRIMARY + 4)
#define ATA_PORT_COMMAND_PRIMARY_CYLINDER_HIGH (ATA_PORT_COMMAND_PRIMARY + 5)
#define ATA_PORT_COMMAND_PRIMARY_DRIVE (ATA_PORT_COMMAND_PRIMARY + 6)
#define ATA_PORT_COMMAND_PRIMARY_COMMAND (ATA_PORT_COMMAND_PRIMARY + 7)
#define ATA_PORT_COMMAND_PRIMARY_STATUS (ATA_PORT_COMMAND_PRIMARY + 7)
#define ATA_PORT_CONTROL_PRIMARY 0x03f4
#define ATA_PORT_COMMAND_SECONDARY 0x0170
#define ATA_PORT_CONTROL_SECONDARY 0x0374

// ATA Command and Control registers
// ref: 3.5.1 Table 13
//
// Each is a byte it seems. Except data which is 16 bit.
// Reading the status register clears the interrupt.
//
// Command
// Offset         Function (R/W)
// 0               Data
// 1               Error/Features
// 2               Sector Count
// 3               Sector Number
// 4               Cylinder Low
// 5               Cylinder High
// 6               Drive/Head
// 7               Status/Command
//
// Status register
// Bit      Name
// 0        ERR Error
// 1        IDX Index
// 2        CORR Corrected data
// 3        DRQ Data request
// 4        DSC Drive seek complete
// 5        DWF Drive write fault
// 6        DRDY Drive ready
// 7        BSY Busy
//
// Control
// Offset         Function
// 0               Reserved
// 1               Reserved
// 2               Alt Status/Device control
// 3               Forward to ISA (Floppy)
//
//
// Protocol
//
// Before each command one has to check that BSY=0 and DRDY=1.
// Configure the command.
// Send the command.
// The device sends 1 sector, i.e. 512kb, at a time.
// Before each sector read one has to wait for BSY=0 and DRQ=1.
// Actually the device sends an interrupt, but we clear it by
// polling the status register.

#define ATA_COMMAND_IDENTIFY 0xec
#define ATA_COMMAND_READ_BUFFER 0xe4
#define ATA_COMMAND_READ_SECTOR_RETRY 0x20
// Response in error register
#define ATA_COMMAND_SELF_TEST 0x90

void hdd_wait_drdy() {
  while (true) {
    u8 s = inb(ATA_PORT_COMMAND_PRIMARY_STATUS);
    printf("pci: status=%b\n", s);
    if (s & 0b01000000) {
      break;
    }
  }
}

void hdd_wait_drq() {
  while (true) {
    u8 s = inb(ATA_PORT_COMMAND_PRIMARY_STATUS);
    printf("pci: status=%b\n", s);
    if (s & 0b00001000) {
      printf("pci: data ready\n");
      break;
    }
  }
}

void hdd_read(u16* buffer) {
  for (u32 i = 0; i < 256; i++) {
    buffer[i] = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
    u8 e = inb(ATA_PORT_COMMAND_PRIMARY_ERROR);
    if (e != 0x0) {
      printf("pci: error=%b\n", e);
    }
  }
}

void hdd_print_boot_record(fat32_boot_record* br) {
  u8* buf = (u8*)br;
  printf("pci: boot record %x %x %x\n", buf[0], buf[1], buf[2]);
  printf("pci: oem_identifier=%c%c%c%c%c%c%c%c\n", buf[3], buf[4], buf[5],
	 buf[6], buf[7], buf[8], buf[9], buf[10]);

  printf("fat32: bootable_partition_signature=%x\n",
	 br->fat32_extended_boot_record.bootable_partition_signature);
  printf("fat32: reserved_sectors=%d\n",
	 br->fat32_bios_parameter_block.num_reserved_sectors);
  printf("fat32: hidden_sectors=%d\n",
	 br->fat32_bios_parameter_block.num_hidden_sectors);
  printf("fat32: backup_boot_sector=%d\n",
	 br->fat32_extended_boot_record.backup_boot_sector);

  printf("fat32: fat_id=%x\n",
	 br->fat32_bios_parameter_block.media_descriptor_type);
  printf("fat32: num_fats=%d\n", br->fat32_bios_parameter_block.num_fats);
  printf("fat32: sectors_per_fat=%d\n",
	 br->fat32_extended_boot_record.num_sectors_per_fat);
  printf("fat32: fat_version=%x\n", br->fat32_extended_boot_record.fat_version);
  printf("fat32: num_bytes_per_sector=%d\n",
	 br->fat32_bios_parameter_block.num_bytes_per_sector);
  printf("fat32: num_sectors_per_cluster=%d\n",
	 br->fat32_bios_parameter_block.num_sectors_per_cluster);
  printf("fat32: root_cluster=%d\n",
	 br->fat32_extended_boot_record.root_cluster);
  printf("fat32: fsinfo_sector=%d\n",
	 br->fat32_extended_boot_record.fsinfo_sector);
  printf("fat32: volume_label=%.*s\n", 11,
	 br->fat32_extended_boot_record.volume_label);
  printf("fat32: system_identifier=%.*s\n", 8,
	 br->fat32_extended_boot_record.system_identifier);
}

void hdd() {
  // Set drive=0
  u8 d = inb(ATA_PORT_COMMAND_PRIMARY_DRIVE);
  printf("pci: drive=%b\n", d);
  d &= ~0b00010000;
  outb(ATA_PORT_COMMAND_PRIMARY_DRIVE, d);
  d = inb(ATA_PORT_COMMAND_PRIMARY_DRIVE);
  printf("pci: drive=%b\n", d);

  // Enable LBA
  d = inb(ATA_PORT_COMMAND_PRIMARY_DRIVE);
  printf("pci: drive=%b\n", d);
  d |= 0b01000000;
  outb(ATA_PORT_COMMAND_PRIMARY_DRIVE, d);
  d = inb(ATA_PORT_COMMAND_PRIMARY_DRIVE);
  printf("pci: drive=%b\n", d);

  // Set head
  d &= ~0b1111;
  outb(ATA_PORT_COMMAND_PRIMARY_DRIVE, d);
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR_COUNT, 1);
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR, 0);
  outb(ATA_PORT_COMMAND_PRIMARY_CYLINDER_LOW, 0);
  outb(ATA_PORT_COMMAND_PRIMARY_CYLINDER_HIGH, 0);

  hdd_wait_drdy();

  outb(ATA_PORT_COMMAND_PRIMARY_COMMAND, ATA_COMMAND_READ_SECTOR_RETRY);

  u8 e = inb(ATA_PORT_COMMAND_PRIMARY_ERROR);
  printf("pci: error=%b\n", e);

  hdd_wait_drq();

  u16 buf1[512] = {0};
  hdd_read(buf1);
  hdd_print_boot_record((fat32_boot_record*)buf1);

  // Read FSInfo
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR_COUNT, 1);
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR, 1);
  hdd_wait_drdy();
  outb(ATA_PORT_COMMAND_PRIMARY_COMMAND, ATA_COMMAND_READ_SECTOR_RETRY);

  e = inb(ATA_PORT_COMMAND_PRIMARY_ERROR);
  printf("pci: error=%b\n", e);

  hdd_wait_drq();

  u16 buf2[512] = {0};
  hdd_read(buf2);
  fat32_fsinfo* fsinfo = (fat32_fsinfo*)buf2;
  if (fsinfo->signature == 0x41615252 && fsinfo->signature_3 == 0xaa550000) {
    printf("fat32: found fsinfo\n");
    printf("fat32: %x %x %x\n", fsinfo->signature, fsinfo->signature_2,
	   fsinfo->signature_3);
  }

  // Read FAT
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR_COUNT, 1);
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR, 32);
  e = inb(ATA_PORT_COMMAND_PRIMARY_ERROR);
  printf("pci: error=%b\n", e);

  hdd_wait_drdy();
  outb(ATA_PORT_COMMAND_PRIMARY_COMMAND, ATA_COMMAND_READ_SECTOR_RETRY);

  hdd_wait_drq();

  u16 buf3[512] = {0};
  hdd_read(buf3);
  u32* fatp = (u32*)buf3;
  printf("fat32: cluster0=%x (fat_id) cluster1=%x (end of chain marker)\n",
	 fatp[0], fatp[1]);

  for (u32 i = 0; i < 256; i++) {
    if ((fatp[i] & 0x0fffffff) == 0x0) {
      //  printf("empty\n");
      continue;
    }

    printf("fat32: cluster %d=%x ", i, fatp[i]);
    if ((fatp[i] & 0x0fffffff) == 0x1) {
      printf("reserved");
    } else if ((fatp[i] & 0x0fffffff) >= 0x2 &&
	       (fatp[i] & 0x0fffffff) <= 0x0fffffef) {
      printf("next cluster");
    } else if ((fatp[i] & 0x0fffffff) == 0x0ffffff7) {
      printf("bad sector");
    } else if ((fatp[i] & 0x0fffffff) >= 0x0ffffff8) {
      printf("last cluster in file");
    }
    printf("\n");
  }

  // 32 + 788 * 2 (num_fats) = 0x648
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR_COUNT, 1);
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR, 0x48);
  outb(ATA_PORT_COMMAND_PRIMARY_CYLINDER_LOW, 0x6);
  hdd_wait_drdy();
  outb(ATA_PORT_COMMAND_PRIMARY_COMMAND, ATA_COMMAND_READ_SECTOR_RETRY);

  hdd_wait_drq();

  u16 buf4[512] = {0};
  hdd_read(buf4);
  fat32_directory_entry* de = (fat32_directory_entry*)buf4;
  printf("fat32: filename=%.*s\n", 11, de->file_name);
  printf("fat32: attributes=%b\n", de->attributes);
  printf("fat32: first_cluster_high=%x\n", de->first_cluster_high);
  printf("fat32: first_cluster_low=%x\n", de->first_cluster_low);
  printf("fat32: size_in_bytes=%d\n", de->size_in_bytes);

  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR_COUNT, 1);
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR, 0x49);
  outb(ATA_PORT_COMMAND_PRIMARY_CYLINDER_LOW, 0x6);
  hdd_wait_drdy();
  outb(ATA_PORT_COMMAND_PRIMARY_COMMAND, ATA_COMMAND_READ_SECTOR_RETRY);

  hdd_wait_drq();

  u16 buf5[512] = {0};
  hdd_read(buf5);
  printf("fat32: file contents=\n");
  printf("%.*s", de->size_in_bytes, buf5);

  // Identify
  // Wait for DRDY
  hdd_wait_drdy();
  outb(ATA_PORT_COMMAND_PRIMARY_COMMAND, ATA_COMMAND_IDENTIFY);

  hdd_wait_drq();

  u16 info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  printf("pci: info=%b\n", info);
  if (info & 0x40) {  // 0b0100.0000
    printf("pci: fixed drive\n");
  }
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  printf("pci: cylinders=%d\n", info);

  // read reserved
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  printf("pci: heads=%d\n", info);

  // stuff
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // vendor
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // serial #
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  printf("pci: buffer_type=%b\n", info);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  printf("pci: buffer_size=%x\n", info);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  printf("pci: ecc_bytes=%b\n", info);

  // Firmware revision (8 ascii)
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // Model number (40 ascii)
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // Capabilities
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  printf("pci: capabilities=%b\n", info);

  // Reserved
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // # of current cylinders
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // # of current heads
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // # of current sectors per track
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // current capacity in sectors
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  //
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // Total number of user addressable sectors (LBA-only)
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  info = inw(ATA_PORT_COMMAND_PRIMARY_DATA);

  // rest is reserved or vendor specific
}

void pci_enumerate() {
  pci_config_address a = {0};
  a.bits.enabled = 1;
  a.bits.bus = 0;
  a.bits.device = 0;
  a.bits.function = 0;
  a.bits.offset = 0;

  // Root bus is apparently always 0 so we could scan from there.
  for (int bus = 0; bus < 256; bus++) {
    if (bus > 1) {
      break;
    }
    // printf("pci: scanning bus %d\n", bus);
    a.bits.bus = bus;
    for (int device = 0; device < 32; device++) {
      a.bits.device = device;
      for (int function = 0; function < 8; function++) {
	a.bits.function = function;

	a.bits.offset = 0;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_0 h0;
	h0.raw = inl(PCI_CONFIG_DATA);

	if (h0.fields.vendor_id == 0xffff) {
	  continue;
	}
	printf("pci: bus=%d device=%d function=%d\n", bus, device, function);
	printf("pci: vendor=%x device=%x\n", h0.fields.vendor_id,
	       h0.fields.device_id);
	printf("pci: name=%s\n",
	       pci_device_name(h0.fields.vendor_id, h0.fields.device_id));

	a.bits.offset = 0x8;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_2 h2;
	h2.raw = inl(PCI_CONFIG_DATA);
	printf("pci: class=%x subclass=%x progif=%b\n", h2.fields.class,
	       h2.fields.subclass, h2.fields.prog_if);

	a.bits.offset = 0xc;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_3 h3;
	h3.raw = inl(PCI_CONFIG_DATA);
	printf("pci: header_type=%x\n", h3.fields.header_type);

	if (h0.fields.vendor_id == PCI_VENDOR_REALTEK &&
	    h0.fields.device_id == 0x8139) {
	  // TODO: should be registered somewhere. pci should not know this
	  // specific thing.
	  rtl8139_configure(a);
	} else if (h0.fields.vendor_id == PCI_VENDOR_INTEL &&
		   h0.fields.device_id == 0x7010) {
	  printf("pci: found ide controller\n");
	  // IDE
	  // read command and status
	  a.bits.offset = 0x4;
	  outl(PCI_CONFIG_ADDRESS, a.raw);
	  pci_config_register_1 h1;
	  h1.raw = inl(PCI_CONFIG_DATA);
	  printf("pci: command=%b status=%b\n", h1.fields.command,
		 h1.fields.status);

	  h1.fields.command_bits.bus_master = 1;
	  printf("pci: writing command=%b\n", h1.fields.command);
	  outl(PCI_CONFIG_ADDRESS, a.raw);
	  outl(PCI_CONFIG_DATA, a.raw);

	  a.bits.offset = 0x40;
	  outl(PCI_CONFIG_ADDRESS, a.raw);
	  u16 ide_timing_register = inw(PCI_CONFIG_DATA);
	  printf("pci: timing_register=%b\n", ide_timing_register);
	  // IDE Decode Enable (IDE) was 1

	  if (h3.fields.header_type == 0x0) {
	    // BAR header

	    // Loop over all bars, write 0xffffffff and read response.
	    // 0x0 response means bar not supported.
	    for (u8 bar = 0; bar < 5; bar++) {
	      a.bits.offset = (4 + bar) * 4;
	      outl(PCI_CONFIG_ADDRESS, a.raw);
	      pci_config_register_4 h4;
	      h4.raw = inl(PCI_CONFIG_DATA);

	      if (h4.io_space.is_io_space) {
		// According to 'info pci' 0xc100 is the right address.
		printf("pci: offset=%x bar=%d uses ports address=%x\n",
		       a.bits.offset, bar, h4.io_space.address << 2);

		// Note: everything in the configuration space is little endian
		u16 io_base = h4.io_space.address << 2;

		// This one actually uses ports
		// Offset     Register
		// 00h        Bus Master IDE Command (primary)
		// 01h        Reserved
		// 02h        Bus Master IDE Status (primary)
		// 03h        Reserved
		// 04-07h    Bus Master IDE Descriptor Table Pointer
		// (primary) 08h        Bus Master IDE Command (secondary)
		// 09h        Reserved
		// 0Ah        Bus Master IDE Status (secondary)
		// 0Bh        Reserved
		// 0C-0Fh    Bus Master IDE Descriptor Table Pointer
		// (secondary)

		// We need to enable bus mastering (bit 2) and io space
		// enable (bit 0) in the command byte

		// 2.7. PCI BUS Master IDE Registers
		// Page 99 describes the operation of the BUS Master
		// ref:
		// https://web.archive.org/web/20170115041945/http://download.intel.com/design/intarch/datashts/29055002.pdf

		// u8 a = inb(ATA_PORT_COMMAND_PRIMARY);
		// printf("pci: ata_command=%b\n", a);

		// u8 b = inb(ATA_PORT_CONTROL_PRIMARY);
		// // TODO: b is u6 but printf reads u64 from stack. Seems
		// // problematic.
		// printf("pci: ata_control=%b\n", b);

		hdd();

	      } else {
		printf("pci: bar=%d uses mmio address=%x\n", bar,
		       h4.memory_space.address);
		// // write FFFFFFFFF.... read back for size
		// outl(PCI_CONFIG_ADDRESS, a.raw);
		// outl(PCI_CONFIG_DATA, 0xffffffff);

		// outl(PCI_CONFIG_ADDRESS, a.raw);
		// pci_config_register_4 answer;
		// answer.raw = inl(PCI_CONFIG_DATA);
		// if (answer.raw == 0x0) {
		//   // This BAR is not supported
		//   continue;
		// }
		// printf("pci: bar=%d size=%d\n", bar, answer.raw);
		// // Now we need to allocate physical memory of the size
		// specified
		// // and write the address to the BAR.
		// // Needs to be in 32 bit range.
		// memory* bar_memory = memory_remove();
		// printf("pci: memory=%x\n", bar_memory->address);

		// outl(PCI_CONFIG_ADDRESS, a.raw);
		// outl(PCI_CONFIG_DATA, (u32)bar_memory->address);

		// outl(PCI_CONFIG_ADDRESS, a.raw);
		// answer.raw = inl(PCI_CONFIG_DATA);
		// printf("pci: bar=%d address=%x\n", bar, answer.raw);
	      }
	    }
	  }
	} else if (h0.fields.vendor_id == PCI_VENDOR_INTEL &&
		   h0.fields.device_id == 0x2922) {
	  // TODO: bochs does not support this so I put it on hold

	  // ahci sata controller
	  // ref:
	  // https://www.intel.com/content/dam/doc/datasheet/io-controller-hub-9-datasheet.pdf
	  // 14.1.30 SATA mode select
	  // 14.2 IDE bus master
	  // 14.4.1.2 GHC—Global ICH9 Control Registe

	  if (h3.fields.header_type == 0x0) {
	    // Before reading the BAR, disable port and mmio in the command
	    // byte.
	    a.bits.offset = 4 * 4;
	    outl(PCI_CONFIG_ADDRESS, a.raw);
	    pci_config_register_4 h4;
	    h4.raw = inl(PCI_CONFIG_DATA);

	    if (h4.io_space.is_io_space) {
	      printf("pci: bar0 uses ports address=%x\n", h4.io_space.address);
	    } else {
	      printf("pci: bar0 uses mmio address=%x\n",
		     h4.memory_space.address);
	      // write FFFFFFFFF.... read back for size
	      outl(PCI_CONFIG_ADDRESS, a.raw);
	      outl(PCI_CONFIG_DATA, 0xffffffff);

	      outl(PCI_CONFIG_ADDRESS, a.raw);
	      pci_config_register_4 answer;
	      answer.raw = inl(PCI_CONFIG_DATA);
	      printf("pci: bar0 size=%d", answer.raw);
	    }
	  }
	}
      }
    }
  }
}
