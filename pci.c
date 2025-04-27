#include "pci.h"
#include "ata.h"
#include "io.h"
#include "lib.h"
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

		ata_initialize();

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
