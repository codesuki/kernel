#include "ata.h"
#include "fat32.h"
#include "io.h"
#include "lib.h"
#include "print.h"

void ata_wait_drive_ready() {
  while (true) {
    ata_status_register r;
    r.raw = inb(ATA_PORT_COMMAND_PRIMARY_STATUS);
    printf("pci: status=%b\n", r.raw);
    if (r.bits.drive_ready) {
      break;
    }
  }
}

void ata_wait_data_request() {
  while (true) {
    ata_status_register r;
    r.raw = inb(ATA_PORT_COMMAND_PRIMARY_STATUS);
    printf("pci: status=%b\n", r.raw);
    if (r.bits.data_request) {
      break;
    }
  }
}

void ata_enable_lba() {
  ata_drive_register r;
  r.raw = inb(ATA_PORT_COMMAND_PRIMARY_DRIVE);
  r.bits.lba = 1;
  outb(ATA_PORT_COMMAND_PRIMARY_DRIVE, r.raw);
}

void ata_select_drive(u8 drive) {
  ata_drive_register r;
  r.raw = inb(ATA_PORT_COMMAND_PRIMARY_DRIVE);
  if (drive == 0) {
    r.bits.drive = 0;
  } else {
    r.bits.drive = 1;
  }
  outb(ATA_PORT_COMMAND_PRIMARY_DRIVE, r.raw);
}

void ata_select_sectors(u32 start_lba, u32 count) {
  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR, start_lba & 0xff);
  outb(ATA_PORT_COMMAND_PRIMARY_CYLINDER_LOW, (start_lba >> 8) & 0xff);
  outb(ATA_PORT_COMMAND_PRIMARY_CYLINDER_HIGH, (start_lba >> 16) & 0xff);
  ata_drive_register r;
  r.raw = inb(ATA_PORT_COMMAND_PRIMARY_DRIVE);
  r.bits.head_or_lba_high = (start_lba >> 24) & 0xff;
  outb(ATA_PORT_COMMAND_PRIMARY_DRIVE, r.raw);

  outb(ATA_PORT_COMMAND_PRIMARY_SECTOR_COUNT, 1);
}

void ata_read_sectors(u8 drive, u32 lba, u32 num_sectors, u16* buffer) {
  ata_select_drive(drive);
  ata_select_sectors(lba, num_sectors);
  ata_wait_drive_ready();
  outb(ATA_PORT_COMMAND_PRIMARY_COMMAND, ATA_COMMAND_READ_SECTOR_RETRY);
  ata_wait_data_request();
  for (u32 i = 0; i < 256 * num_sectors; i++) {
    buffer[i] = inw(ATA_PORT_COMMAND_PRIMARY_DATA);
    u8 e = inb(ATA_PORT_COMMAND_PRIMARY_ERROR);
    if (e != 0x0) {
      printf("pci: error=%b\n", e);
      __asm__("hlt");
    }
  }
}

void ata_read_sector(u8 drive, u32 lba, u16* buffer) {
  ata_read_sectors(drive, lba, 1, buffer);
}

void ata_initialize() {
  ata_enable_lba();
  ata_select_drive(0);
  ata_wait_drive_ready();

  // Read boot record
  u16 boot_sector[512] = {0};
  ata_read_sector(0, 0, (u16*)&boot_sector);

  // Identify format
  // TODO: make this less brittle.
  // TODO: don't depend on fat32 here.
  u8 fat32_jump[3] = {0xEB, 0x58, 0x90};
  if (memcmp(boot_sector, fat32_jump, 3)) {
    //
    fat32_initialize((fat32_boot_record*)boot_sector);
  } else {
    printf("ata: unknown file system\n");
    __asm__("hlt");
  }

  // Identify
  // Wait for DRDY
  ata_wait_drive_ready();
  outb(ATA_PORT_COMMAND_PRIMARY_COMMAND, ATA_COMMAND_IDENTIFY);

  ata_wait_data_request();

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
