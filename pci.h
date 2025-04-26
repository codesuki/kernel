#pragma once

#include "types.h"

#define PCI_CONFIG_ADDRESS 0xcf8
#define PCI_CONFIG_DATA 0xcfc

#define PCI_VENDOR_INTEL 0x8086
#define PCI_VENDOR_BOCHS 0x1234
#define PCI_VENDOR_REALTEK 0x10ec

#define PCI_DEVICE_RTL8139 0x8139

typedef union {
  u32 raw;
  struct {
    u32 offset : 8;
    u32 function : 3;
    u32 device : 5;
    u32 bus : 8;
    u32 reserved : 7;
    u32 enabled : 1;
  } bits;
} pci_config_address;

typedef union {
  u32 raw;
  struct {
    u16 vendor_id;
    u16 device_id;
  } __attribute__((packed)) fields;
} pci_config_register_0;

typedef union {
  u32 raw;
  struct {
    union {
      u16 command;
      struct {
	u16 io_space : 1;
	u16 memory_space : 1;
	u16 bus_master : 1;
	u16 reserved : 13;
      } command_bits;
    };
    union {
      u16 status;
      struct {
	u16 reserved : 16;
      } status_bits;
    };
  } __attribute__((packed)) fields;
} pci_config_register_1;

typedef union {
  u32 raw;
  struct {
    u8 revision_id;
    u8 prog_if;
    u8 subclass;
    u8 class;
  } __attribute__((packed)) fields;
} pci_config_register_2;

typedef union {
  u32 raw;
  struct {
    u8 cache_line_size;
    u8 latency_timer;
    u8 header_type;
    u8 bist;
  } __attribute__((packed)) fields;
} pci_config_register_3;

typedef union {
  u32 raw;
  struct {
    u32 is_io_space : 1;
    u32 type : 2;  // 0 = 32 bit, 1 = reserved, 2 = 64 bit (uses two BARs)
    u32 prefetchable : 1;
    u32 address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    u32 is_io_space : 1;
    u32 reserved : 1;
    u32 address : 30;
  } __attribute__((packed)) io_space;
} pci_config_register_4;

typedef union {
  u32 raw;
  struct {
    u32 is_io_space : 1;
    u32 type : 2;
    u32 prefetchable : 1;
    u32 address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    u32 is_io_space : 1;
    u32 reserved : 1;
    u32 io_size : 6;
    u32 address : 24;
  } __attribute__((packed)) io_space;
} pci_config_register_4_rtl8139;

typedef union {
  u32 raw;
  struct {
    u8 interrupt_line;
    u8 interrupt_pin;
    u8 min_grant;
    u8 max_latency;
  } __attribute__((packed)) fields;
} pci_config_register_f;

void pci_enumerate();

typedef struct {
  struct {
    u8 jump[3];
    u8 identifier[8];
    u16 num_bytes_per_sector;
    u8 num_sectors_per_cluster;
    u16 num_reserved_sectors;
    u8 num_fats;
    u16 num_root_dir_entries;
    u16 total_sectors;  // if 0 stored in large sector count
    u8 media_descriptor_type;
    // u16 num_sectors_per_fat;
    u16 reserved;
    u16 num_sectors_per_track;
    u16 num_heads;
    u32 num_hidden_sectors;
    u32 num_large_sectors;
  } __attribute__((packed)) fat32_bios_parameter_block;

  struct {
    u32 num_sectors_per_fat;
    u16 flags;
    u16 fat_version;  // high byte = major version, low byte = minor version
    u32 root_cluster;
    u16 fsinfo_sector;
    u16 backup_boot_sector;
    u8 reserved[12];
    u8 drive_number;
    u8 reserved_2;  // windows NT flags
    u8 signature;   // 0x28 or 0x29
    u32 volume_id;
    u8 volume_label[11];
    u8 system_identifier[8];  // "FAT32   "
    u8 boot_code[420];
    u16 bootable_partition_signature;  // 0xaa55
  } __attribute__((packed)) fat32_extended_boot_record;
} fat32_boot_record;

typedef struct {
  u32 signature;  // 0x41615252
  u8 reserved[480];
  u32 signature_2;          // 0x61417272
  u32 last_free_cluster;    // unknown if 0xffffffff
  u32 last_free_cluster_2;  // TODO: check official spec
  u8 reserved_2[12];
  u32 signature_3;  // 0xaa550000
} __attribute__((packed)) fat32_fsinfo;

#define FAT32_FILE_ATTRIBUTE_READ_ONLY 0x1
#define FAT32_FILE_ATTRIBUTE_HIDDEN 0x2
#define FAT32_FILE_ATTRIBUTE_SYSTEM 0x4
#define FAT32_FILE_ATTRIBUTE_VOLUME_LABEL 0x8
#define FAT32_FILE_ATTRIBUTE_SUBDIRECTORY 0x10
#define FAT32_FILE_ATTRIBUTE_ARCHIVE 0x20

typedef struct {
  // tODO: split
  u8 file_name[11];  // 8 + 3: name + extension
  u8 attributes;
  u8 reserved;
  u8 creation_seconds;  // s/100
  u16 creation_ts;      // hour 5bits minutes 6bits seconds 5bits
  u16 creation_date;    // year 7bits month 4bits day 5bits
  u16 last_accessed_date;
  u16 first_cluster_high;  // high 16 bits
  u16 last_modification_time;
  u16 last_modification_date;
  u16 first_cluster_low;  // low 16 bits
  u32 size_in_bytes;
} __attribute__((packed)) fat32_directory_entry;

typedef struct {
} __attribute__((packed)) fat32_long_file_name;
