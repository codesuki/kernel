#pragma once

#include "types.h"

typedef struct {
  u8 magic[4];  // 0x7F followed by ELF(45 4c 46)
  u8 class;     // 1=32 bit or 2=64 bit
  u8 data;      // 1=little endian or 2=big endian
  u8 version;   // 1
  u8 os_abi;    // 0x03 Linux,
  u8 abi_version;
  u8 padding[7];
  u16 type;  // 0x0=Unknown, 0x1=Relocatable, 0x2=Executable, 0x3=Shared object,
  // 0x4=Core file
  u16 machine;
  u32 version_2;  // 1
  u64 entry;      // entry point
  u64 program_header_offset;
  u64 section_header_offset;
  u32 flags;
  u16 size;                       // size of this header
  u16 program_header_entry_size;  // 0x20 for 32 bit or 0x38 for 64 bit
  u16 program_header_entries;     // number of entries
  u16 section_header_entry_size;  // 0x28 for 32 bit or 0x40 for 64 bit
  u16 section_header_entries;     // number of entries
  u16 section_header_names;       // index of names
} __attribute__((packed)) elf_header;

typedef struct {
} __attribute__((packed)) elf_header_32;

typedef struct {
} __attribute__((packed)) elf_header_64;

#define ELF_SEGMENT_TYPE_LOAD 0x1

typedef struct {
  u32 type;  // 0x0=NULL, 0x1=LOAD, 0x2=DYNAMIC, 0x3=INTERP, 0x4=NOTE,
  // 0x5=SHLIB, 0x6=PHDR, 0x7=TLS
  u32 flags;   // 0x1=X, 0x2=W, 0x3=R
  u64 offset;  // offset into the file for this segment
  u64 vaddr;   // virtual address
  u64 paddr;   // physical address
  u64 file_size;
  u64 mem_size;
  u64 align;  // 0|1=no alignment or positive power of 2. vaddr = offset % align
} __attribute__((packed)) elf_program_header;

typedef struct {
  u32 name;  // offset into .shstrtab that has the name
  u32 type;  // 0x0=NULL, 0x1=PROGBITS, 0x2=SYMTAB, 0x3=STRTAB, 0x4=RELA,
  // 0x5=HASH, 0x6=DYNAMIC, 0x7=NOTE, ...
  u64 flags;  // 0x1=WRITE, 0x2=ALLOC, 0x4=EXECINSTR, ...
  u64 vaddr;
  u64 offset;  // offset into file for this section
  u64 size;
  u32 link;
  u32 info;
  u64 align;
  u64 entry_size;  // for sections that contain fixed size entries, otherwise 0
} __attribute__((packed)) elf_section_header;
