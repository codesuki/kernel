#include "fat32.h"
#include "ata.h"
#include "lib.h"
#include "memory.h"
#include "print.h"

fat32_cluster_type fat32_identify_cluster(u32 cluster) {
  if ((cluster & 0x0fffffff) == 0x0) {
    return empty;
  } else if ((cluster & 0x0fffffff) == 0x1) {
    return reserved;
  } else if ((cluster & 0x0fffffff) >= 0x2 &&
	     (cluster & 0x0fffffff) <= 0x0fffffef) {
    return cluster_pointer;
  } else if ((cluster & 0x0fffffff) == 0x0ffffff7) {
    return bad_pointer;
  } else if ((cluster & 0x0fffffff) >= 0x0ffffff8) {
    return last;
  } else {
    return unknown;
  }
}

fat32_cluster fat32_next_cluster(fat32_cluster cluster) {
  return cluster & 0x0fffffff;
}

u32 fat32_cluster_size(fat32_boot_record* boot_record) {
  // TODO: is byte_per_sector trustworthy? This should be a disk property.
  return boot_record->bios_parameter_block.sectors_per_cluster *
	 boot_record->bios_parameter_block.bytes_per_sector;
}

u32 fat32_cluster_sector(fat32_boot_record* boot_record, u32 cluster) {
  u32 first_cluster_sector =
      boot_record->bios_parameter_block.num_reserved_sectors +
      2 * boot_record->extended_boot_record.sectors_per_fat;

  return first_cluster_sector +
	 (cluster - 2) * boot_record->bios_parameter_block.sectors_per_cluster;
}

void fat32_read_cluster(fat32_context* ctx, u32 cluster, u16* buffer) {
  u32 sector = fat32_cluster_sector(ctx->boot_record, cluster);
  u32 size = fat32_cluster_size(ctx->boot_record);

  ata_read_sectors(0, sector,
		   ctx->boot_record->bios_parameter_block.sectors_per_cluster,
		   buffer);
}

fat32_buffer* fat32_read_file(fat32_context* ctx, u8* name) {
  // Scan to find specific file.
  fat32_directory_entry* e = ctx->root;
  while (e->name[0] != 0x00) {
    printf("fat32: name=%.*s\n", 8, e->name);
    printf("fat32: extension=%.*s\n", 3, e->extension);
    printf("fat32: attributes=%b\n", e->attributes);
    printf("fat32: first_cluster_high=%x\n", e->first_cluster_high);
    printf("fat32: first_cluster_low=%x\n", e->first_cluster_low);
    printf("fat32: size_in_bytes=%d\n", e->size_in_bytes);
    u32 cluster = (e->first_cluster_high << 16) | e->first_cluster_low;
    printf("fat32: cluster=%d\n", cluster);

    if (strncmp(e->name, name, strlen(name))) {
      break;
    }

    e++;
  }

  fat32_buffer* buffer = malloc(sizeof(*buffer));
  buffer->data = malloc(e->size_in_bytes);
  fat32_cluster cluster = (e->first_cluster_high << 16) | e->first_cluster_low;
  u32 total = 0;

  printf("fat32: cluster=%d\n", cluster);
  fat32_read_cluster(ctx, cluster, (u16*)buffer->data);
  total += fat32_cluster_size(ctx->boot_record);

  while (fat32_identify_cluster(ctx->fat[cluster]) != last) {
    cluster = fat32_next_cluster(ctx->fat[cluster]);
    printf("fat32: cluster=%d\n", cluster);
    fat32_read_cluster(ctx, cluster, (u16*)((u8*)buffer->data + total));
    total += fat32_cluster_size(ctx->boot_record);
  }

  printf("fat32: total=%d\n", total);
  buffer->size = total;
  return buffer;
}

void fat32_print_boot_record(fat32_boot_record* br) {
  u8* buf = (u8*)br;
  printf("pci: boot record %x %x %x\n", buf[0], buf[1], buf[2]);
  printf("pci: oem_identifier=%c%c%c%c%c%c%c%c\n", buf[3], buf[4], buf[5],
	 buf[6], buf[7], buf[8], buf[9], buf[10]);

  printf("fat32: bootable_partition_signature=%x\n",
	 br->extended_boot_record.bootable_partition_signature);
  printf("fat32: reserved_sectors=%d\n",
	 br->bios_parameter_block.num_reserved_sectors);
  printf("fat32: hidden_sectors=%d\n",
	 br->bios_parameter_block.num_hidden_sectors);
  printf("fat32: backup_boot_sector=%d\n",
	 br->extended_boot_record.backup_boot_sector);

  printf("fat32: fat_id=%x\n", br->bios_parameter_block.media_descriptor_type);
  printf("fat32: num_fats=%d\n", br->bios_parameter_block.num_fats);
  printf("fat32: sectors_per_fat=%d\n",
	 br->extended_boot_record.sectors_per_fat);
  printf("fat32: fat_version=%x\n", br->extended_boot_record.fat_version);
  printf("fat32: num_bytes_per_sector=%d\n",
	 br->bios_parameter_block.bytes_per_sector);
  printf("fat32: num_sectors_per_cluster=%d\n",
	 br->bios_parameter_block.sectors_per_cluster);
  printf("fat32: root_cluster=%d\n", br->extended_boot_record.root_cluster);
  printf("fat32: fsinfo_sector=%d\n", br->extended_boot_record.fsinfo_sector);
  printf("fat32: volume_label=%.*s\n", 11,
	 br->extended_boot_record.volume_label);
  printf("fat32: system_identifier=%.*s\n", 8,
	 br->extended_boot_record.system_identifier);
}

fat32_context* new_context() {
  printf("fat32: new context\n");
  fat32_context* ctx = malloc(sizeof(*ctx));
  ctx->boot_record = malloc(sizeof(*ctx->boot_record));
  ctx->fsinfo = malloc(sizeof(*ctx->fsinfo));
  return ctx;
}

fat32_context* fat32_initialize() {
  fat32_context* ctx = new_context();

  // TODO: once we have more filesystems this may need to change.
  // Read boot record
  ata_read_sector(0, 0, (u16*)ctx->boot_record);
  fat32_print_boot_record(ctx->boot_record);

  // TODO: below comment is from ata.c
  // Identify format
  // TODO: make this less brittle.
  // TODO: don't depend on fat32 here.
  // How to do it?
  // 1. We don't detect FS here
  // We only create ATA and make it available
  // FS code detects FS by reading it.
  // 2. We have a registry of formats and we pass them the boot sector.
  //
  // In 1 the ATA code only knows about itself.
  // In 2 the ATA code knows about the registry, for no good reason.
  // It is decided.
  // u8 fat32_jump[3] = {0xEB, 0x58, 0x90};
  // if (memcmp(boot_sector, fat32_jump, 3)) {
  //   //
  //   fat32_initialize((fat32_boot_record*)boot_sector);
  // } else {
  //   printf("ata: unknown file system\n");
  //   __asm__("hlt");
  // }

  // Read FSInfo
  ata_read_sector(0, 1, (u16*)ctx->fsinfo);
  if (ctx->fsinfo->signature == 0x41615252 &&
      ctx->fsinfo->signature_3 == 0xaa550000) {
    printf("fat32: found fsinfo\n");
    printf("fat32: %x %x %x\n", ctx->fsinfo->signature,
	   ctx->fsinfo->signature_2, ctx->fsinfo->signature_3);
  }

  // Read FAT
  //
  // cluster[2] points to the root directory entry.
  // It's actually the first cluster, because 0 and 1 are fixed.
  // We could check it's state before we do anything else for sanity.
  // We actually have to check it's state because it could be too big for one
  // cluster.
  // What to do:
  // Read FAT into memory.
  u32 fat_size = ctx->boot_record->extended_boot_record.sectors_per_fat *
		 ctx->boot_record->bios_parameter_block.bytes_per_sector;
  ctx->fat = malloc(fat_size);

  ata_read_sector(0,
		  ctx->boot_record->bios_parameter_block.num_reserved_sectors,
		  (u16*)ctx->fat);
  printf("fat32: cluster0=%x (fat_id) cluster1=%x (end of chain marker)\n",
	 ctx->fat[0], ctx->fat[1]);

  // Read whole index into memory.
  // For now I only support 1 cluster.
  if (fat32_identify_cluster(ctx->fat[2]) != last) {
    printf("fat32: root directory larger than 1 cluster");
    __asm__("hlt");
  }

  u32 size = fat32_cluster_size(ctx->boot_record);
  ctx->root = malloc(size);
  fat32_read_cluster(ctx, 2, (u16*)ctx->root);

  return ctx;
}
