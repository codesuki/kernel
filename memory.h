#pragma once
#include "types.h"

// Memory start
// Defined in linker script.
// Need to take the address, because the address is the value in this case.
// ref: https://sourceware.org/binutils/docs/ld/Source-Code-Reference.html
extern u64 _kernel_start;
extern u64 _kernel_end;

typedef struct memory memory;
struct memory {
  u64 address;
  u64 size;  // 4kb, 2mb, etc.
  memory* next;
  memory* prev;
};

memory* memory_remove();
void* memory_4kb_remove();

void memory_init(u64 base_address, u64 length);
void* malloc(u64 size);
void free(void* memory);

// Note: I found using bitfields strongly discouraged, but I will still try to
// use them.
// ref: https://news.ycombinator.com/item?id=17056301 and many more.

// Example
// ref: https://news.ycombinator.com/item?id=17057367
// typedef union some_reg {
//   uint32_t raw;
//   struct {
//     uint32_t bits1 : 4;
//     uint32_t bits2 : 3;
//     uint32_t rsvd : 5;
//     uint32_t bits2 : 20;
//   }
// } some_reg_t;

// Then use it either like:
// some_reg_t reg;
// reg->raw = READ_REG(reg_addr);
// reg->bits1 = 4;
// WRITE_REG(reg_addr, reg->raw);
// Or:
// volatile some_reg_t* reg = (some_reg_t*)reg_addr;
// reg->bits1 = 4;

// Paging table entry is 64 bits / 8 byte on a 64 bit system.
typedef union pml4_entry {
  u64 raw;
  struct {
    u64 p : 1;    // present
    u64 rw : 1;   // read/write
    u64 us : 1;   // user/supervisor
    u64 pwt : 1;  // page-level write-through
    u64 pcd : 1;  // page-level cache disable
    u64 a : 1;    // accessed
    u64 ignored_1 : 1;
    u64 reserved : 1;
    u64 ignored_2 : 3;
    u64 r : 1;
    u64 pointer_table_ptr : 40;
    u64 ignored_3 : 11;
    u64 xd : 1;
  };
} pml4_entry;

// Entry that points to a 4gb page
// typedef union page_directory_pointer_table_entry {
//   u64 raw;
//   struct {
//     u64 p : 1;
//     u64 rw : 1;
//     u64 us : 1;
//     u64 pwt : 1;
//     u64 pcd : 1;
//     u64 a : 1;
//     u64 d : 1;  // dirty
//     // page size; must be 1 (otherwise, this entry references a page
//     directory) u64 ps : 1;
//     // ignored if this entry references a page directory
//     u64 g : 1;  // global
//     u64 ignored : 2;
//     u64 r : 1;
//     u64 pat : 1;
//     u64 reserved : 17;
//     u64 directory_ptr : 32;
//     u64 ignored_2 : 7;
//     u64 key : 4;  // protection key
//     u64 xd : 1;
//   };
// } page_directory_pointer_table_entry;

// Entry that points to a page directory
typedef union page_directory_pointer_table_entry {
  u64 raw;
  struct {
    u64 p : 1;
    u64 rw : 1;
    u64 us : 1;
    u64 pwt : 1;
    u64 pcd : 1;
    u64 a : 1;
    u64 ignored_1 : 1;  // dirty, ignored if it doesn't point to a 1gb page
    // page size; must be 0 (otherwise, this entry references a 1 gb page)
    u64 ps : 1;
    u64 ignored_2 : 3;
    u64 r : 1;
    u64 directory_ptr : 40;
    u64 ignored_3 : 11;
    u64 xd : 1;
  };
} page_directory_pointer_table_entry;

// Entry that maps a 2mb page
typedef union page_directory_entry {
  u64 raw;
  struct {
    u64 p : 1;
    u64 rw : 1;
    u64 us : 1;
    u64 pwt : 1;
    u64 pcd : 1;
    u64 a : 1;
    u64 d : 1;
    u64 ps : 1;  // must be 1 in case this points to a 2mb page
    u64 g : 1;
    u64 ignored : 2;
    u64 r : 1;
    u64 pat : 1;
    u64 reserved : 8;
    u64 ptr : 30;
    u64 ignored_2 : 6;
    u64 key : 4;
    u64 xd : 1;
  };
} page_directory_entry;

typedef union page_table_entry {
  u64 raw;
  struct {
    u64 p : 1;
    u64 rw : 1;
    u64 us : 1;
    u64 pwt : 1;
    u64 pcd : 1;
    u64 a : 1;
    u64 d : 1;
    u64 pat : 1;
    u64 g : 1;
    u64 ignored : 2;
    u64 r : 1;
    u64 page_table_ptr : 40;
    u64 ignored_2 : 7;
    u64 key : 4;
    u64 xd : 1;
  };
} page_table_entry;

// ref: intel 3 3.4.5
typedef union gdt_entry {
  u64 raw;
  struct {
    u64 limit_start : 16;
    u64 base_start : 16;
    u64 base_middle : 8;
    u64 type : 4;
    u64 s : 1;
    u64 dpl : 2;
    u64 p : 1;  // present
    u64 limit_end : 4;
    u64 avl : 1;
    u64 l : 1;  // long mode
    u64 db : 1;
    u64 g : 1;
    u64 base_end : 8;
  };
} gdt_entry;

struct gdt_ptr {
  u16 limit;
  u64 base;
} __attribute__((packed));
typedef struct gdt_ptr gdt_ptr;

extern void switch_cr3(void* cr3, void* gdt);
extern void switch_gdt(void* gdt);

void* physical2virtual(void* address);
void* virtual2physical(void* address);
void pages_init();
void pages_map_contiguous(pml4_entry* pml4,
			  u64 virtual_address,
			  u64 physical_start,
			  u64 physical_end
			  //	  u64 flags
);
pml4_entry* pages_new_table();
