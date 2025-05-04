#pragma once

#include "types.h"

typedef struct {
  struct {
    u8 jump[3];
    u8 identifier[8];
    u16 bytes_per_sector;
    u8 sectors_per_cluster;
    u16 num_reserved_sectors;
    u8 num_fats;
    u16 num_root_dir_entries;
    u16 total_sectors;  // if 0 stored in large sector count
    u8 media_descriptor_type;
    // u16 num_sectors_per_fat;
    u16 reserved;
    u16 sectors_per_track;
    u16 num_heads;
    u32 num_hidden_sectors;
    u32 num_large_sectors;
  } __attribute__((packed)) bios_parameter_block;

  struct {
    u32 sectors_per_fat;
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
  } __attribute__((packed)) extended_boot_record;
} __attribute__((packed)) fat32_boot_record;

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
  u8 name[8];
  u8 extension[3];
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

typedef enum {
  unknown,
  empty,
  reserved,
  cluster_pointer,
  bad_pointer,
  last,
} fat32_cluster_type;

typedef u32 fat32_cluster;

typedef struct {
  fat32_boot_record* boot_record;
  fat32_fsinfo* fsinfo;
  fat32_cluster* fat;
  fat32_directory_entry* root;
} fat32_context;

typedef struct {
  u32 size;
  void* data;
} fat32_buffer;

fat32_context* fat32_initialize();
fat32_buffer* fat32_read_file(fat32_context* ctx, u8* name);
