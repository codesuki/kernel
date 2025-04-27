#pragma once

#include "types.h"

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
// Status Register
//
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
// Drive/Head Register
//
// Bit      Name
// 0        HS0   Head address
// 1        HS1
// 2        HS2
// 3        HS3
// 4        DRV   Drive 0/1
// 5        1
// 6        L      Address mode LBA=1
// 7        1
//
// Error Register
//
// Bit      Name    Desc
// 0        AMNF    Address mark not found
// 1        TK0NF   Track 0 not found
// 2        ABRT    Aborted command
// 3        MCR     Media change requested
// 4        IDNF    ID not found
// 5        MC      Media Changed
// 6        UNC     Uncorrectale data error
// 7        BBK     Bad block detect
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

#define ATA_DATA_REQUEST (1 << 3)
#define ATA_DRIVE_READY (1 << 6)
#define ATA_BUSY (1 << 7)

#define ATA_LBA (1 << 6)

#define ATA_DRIVE_1 (1 << 4)

typedef union {
  u8 raw;
  struct {
    u8 error : 1;
    u8 index : 1;
    u8 corrected_data : 1;
    u8 data_request : 1;
    u8 drive_seek_complete : 1;
    u8 drive_write_fault : 1;
    u8 drive_ready : 1;
    u8 busy : 1;
  } __attribute__((packed)) bits;
} ata_status_register;

typedef union {
  u8 raw;
  struct {
    u8 address_mark_not_found : 1;
    u8 track_0_not_found : 1;
    u8 aborted_command : 1;
    u8 media_change_requested : 1;
    u8 id_not_found : 1;
    u8 media_changed : 1;
    u8 uncorrectable_data_error : 1;
    u8 bad_block_detected : 1;
  } __attribute__((packed)) bits;
} ata_error_register;

typedef union {
  u8 raw;
  struct {
    u8 head_or_lba_high : 4;
    u8 drive : 1;
    u8 : 1;  // 1
    u8 lba : 1;
    u8 : 1;  // 1
  } __attribute__((packed)) bits;
} ata_drive_register;

void ata_read_sector(u8 drive, u32 lba, u16* buffer);
void ata_read_sectors(u8 drive, u32 lba, u32 num_sectors, u16* buffer);

void ata_initialize();
