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

void fat32_read_cluster(fat32_boot_record* boot_record,
			u32 cluster,
			u16* buffer) {
  u32 sector = fat32_cluster_sector(boot_record, cluster);
  u32 size = fat32_cluster_size(boot_record);

  ata_read_sectors(
      0, sector, boot_record->bios_parameter_block.sectors_per_cluster, buffer);
}

void hdd_print_boot_record(fat32_boot_record* br) {
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

void fat32_initialize(fat32_boot_record* boot_record) {
  // Read FSInfo
  fat32_fsinfo fsinfo = {0};
  ata_read_sector(0, 1, (u16*)&fsinfo);
  if (fsinfo.signature == 0x41615252 && fsinfo.signature_3 == 0xaa550000) {
    printf("fat32: found fsinfo\n");
    printf("fat32: %x %x %x\n", fsinfo.signature, fsinfo.signature_2,
	   fsinfo.signature_3);
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
  u32 fat_size = boot_record->extended_boot_record.sectors_per_fat *
		 boot_record->bios_parameter_block.bytes_per_sector;
  u32* fat = malloc(fat_size);

  ata_read_sector(0, boot_record->bios_parameter_block.num_reserved_sectors,
		  (u16*)fat);
  printf("fat32: cluster0=%x (fat_id) cluster1=%x (end of chain marker)\n",
	 fat[0], fat[1]);

  // Read whole index into memory.
  // For now I only support 1 cluster.
  if (fat32_identify_cluster(fat[2]) != last) {
    printf("fat32: root directory larger than 1 cluster");
    __asm__("hlt");
  }

  u32 size = fat32_cluster_size(boot_record);
  fat32_directory_entry* index = malloc(size);
  fat32_read_cluster(boot_record, 2, (u16*)index);

  // Scan to find specific file.
  fat32_directory_entry* e = index;
  while (e->name[0] != 0x00) {
    printf("fat32: name=%.*s\n", 8, e->name);
    printf("fat32: extension=%.*s\n", 3, e->extension);
    printf("fat32: attributes=%b\n", e->attributes);
    printf("fat32: first_cluster_high=%x\n", e->first_cluster_high);
    printf("fat32: first_cluster_low=%x\n", e->first_cluster_low);
    printf("fat32: size_in_bytes=%d\n", e->size_in_bytes);
    u32 cluster = (e->first_cluster_high << 16) | e->first_cluster_low;
    printf("fat32: cluster=%d\n", cluster);

    if (strncmp(e->name, "README", 6)) {
      break;
    }

    e++;
  }

  u8* file_contents = malloc(e->size_in_bytes);
  u32 cluster = (e->first_cluster_high << 16) | e->first_cluster_low;
  if (fat32_identify_cluster(fat[cluster]) != last) {
    printf("fat32: file larger than 1 cluster");
    __asm__("hlt");
  }

  fat32_read_cluster(boot_record, cluster, (u16*)file_contents);
  printf("fat32: file contents=\n");
  printf("%.*s", e->size_in_bytes, file_contents);
}
