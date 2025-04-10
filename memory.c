#include "memory.h"
#include "lib.h"    // IWYU pragma: keep
#include "print.h"  // IWYU pragma: keep

const u32 page_size = 0x200000;  // 2mb

const u64 one_gb = 0x400000000;
const u64 lower_half_start = 0x0;
const u64 lower_half_end = 0x00007fffffffffff;
const u64 half_size = lower_half_end;
const u64 higher_half_start = 0xffff800000000000;
const u64 higher_half_end = 0xffffffffffffffff;
const u64 physical_memory_offset = higher_half_start;
const u64 kernel_heap_start = higher_half_start + 200 * one_gb;
const u64 kernel_code_start = 0xffffffff80000000;
// grows down towards kernel_heap_start
const u64 kernel_stack_start = kernel_code_start;
const u64 kernel_physical_start = (u64)&_kernel_start;
const u64 kernel_physical_end = (u64)&_kernel_end;

// Current thought, keep two lists. Free and used memory.
// When we malloc we take from the free list and put it into the used list.
// When we free we search the address in the used list and move it back.
// What other way is there to implement free?
memory* memory_free_first = nullptr;
memory* memory_used_first = nullptr;

// TODO:
// now that the kernel is in the higher half we have to rewrite this.
// what has to change?
// Not much. We initialize the free list in the first free memory available.
// Afterwards we can create a new page table in memory using malloc.
// Set the page table.
// The page stable should map all of physical memory to some upper half address.
// Decide start for kernel heap and stack.
// New discovery: the network card of course needs a physical memory address.
// Currently it's using a global that's mapped to a high address and hence it
// fails. The card only supports 32bit addresses which means its memory has to
// live below 4gb.
// We also need to block PCI and ACPI addresses. PCI seems below 1mb.
//
// memory_init, memory_add, memory_remove
// those are the physical memory management functions.
// the paging code should call these.
//
// continuation from paper notes.
// vmm call pmm: give me 1 page worth of phys memory.
// vmm now needs to configure the page entry
// there is a chance the table there does not exist yet, then we need to
// allocate this too and map it recursively. I think this makes sense. Let's try
// it.

// memory_init creates the first memorys in physical memory, because we cannot
// call malloc.
// This code is very naive. It should do a better job of categorizing memory and
// marking some memory as unusable, e.g. mapped hardware memory, kernel memory,
// etc.
void memory_init(u64 base_address, u64 length) {
  // We don't want to touch the memory block from 0 - 1mb.
  // All BIOS things live there.
  if (base_address == 0) {
    return;
  }
  // This will make our math below go awry.
  if (length == 0) {
    return;
  }

  // Kernel size was off because it was doing address arithmetic. Casting to
  // u64 fixed it. This caused the memorys to be allocated on kernel
  // memory which caused memory corruption.
  u64 base_address_after_kernel =
      base_address + ((u64)&_kernel_end - (u64)&_kernel_start);
  // simpler
  // if base_address < _kernel_end { base_address = kernel_end }

  printf("memory_init: available memory with base %x and length %x\n",
	 base_address, length);

  printf("memory_init: kernel located between %x - %x\n", &_kernel_start,
	 &_kernel_end);

  // TODO: length is too big because of the loader. Make this more resilient.
  // For now I subtract 1.
  u64 page_count = (length / page_size) - 1;
  u64 memory_size = page_count * sizeof(memory);

  // After our memorys
  // Aligned on page_size.
  u64 usable_memory =
      ((base_address_after_kernel + memory_size + (page_size - 1)) &
       ~(page_size - 1));

  printf("memory_init: %d 2mb pages available\n", page_count);
  printf("memory_init: usable memory starts from %x\n", usable_memory);

  memory* current = (memory*)base_address_after_kernel;
  memory* prev = nullptr;

  memory_free_first = current;

  for (u64 i = 0; i < page_count; i++) {
    current->address = usable_memory;
    current->next = nullptr;
    current->prev = prev;
    current->size = page_size;

    if (prev != nullptr) {
      prev->next = current;
    }

    // Nice in Bochs because we only have 30mb there.
    // printf("memory_init: assigned %x\n", m->address);

    usable_memory += page_size;
    prev = current;
    current++;
  }
  for (memory* m = memory_free_first; m != nullptr; m = m->next) {
    printf("memory_init: %x %x %x\n", m->address, m->next, m->prev);
  }
}

void memory_add(u64 address) {
  if (memory_free_first == nullptr) {
    memory_used_first = nullptr;
    // Ouch! We want a new memory. Now do we malloc one? Haha.. Maybe the
    // kernel needs to reserve some memory beforehand for some kind of
    // bootstrap?
    // So when we read available memory, we remove the kernel area.
    // Then we know the real start of the memory.
    // Then we can just do memory * m = start of memory;
    // And so on.
  }
}

memory* memory_remove() {
  if (memory_free_first == nullptr) {
    return nullptr;
  }
  memory* memory = memory_free_first;
  memory_free_first = memory->next;
  memory_free_first->prev = memory->prev;
  return memory;
}

memory* memory_4kb_free_first = nullptr;
// memory* memory_used_first = nullptr;

void memory_4kb_pool_init() {
  const u64 element_size = 0x1000;  // 4kib = 16 * 256 (0x100) bytes
  // get a 2mb physical memory pool
  memory* pool = memory_remove();

  u64 count = (0x200000 / element_size) - 1;
  u64 memory_size = count * sizeof(memory);

  // After our memorys
  // Aligned on element_size.
  u64 usable_memory = ((pool->address + memory_size + (element_size - 1)) &
		       ~(element_size - 1));

  printf("memory_4kb_pool_init: %d 4kb elements available\n", count);
  printf("memory_4kb_pool_init: usable memory starts from %x\n", usable_memory);

  memory* current = (memory*)pool->address;
  memory* prev = nullptr;

  memory_4kb_free_first = current;

  for (u64 i = 0; i < count; i++) {
    current->address = usable_memory;
    current->next = nullptr;
    current->prev = prev;
    current->size = element_size;

    if (prev != nullptr) {
      prev->next = current;
    }

    // Nice in Bochs because we only have 30mb there.
    // printf("memory_init: assigned %x\n", m->address);

    usable_memory += element_size;
    prev = current;
    current++;
  }
  for (memory* m = memory_4kb_free_first; m != nullptr; m = m->next) {
    printf("memory_4kb_pool_init: %x %x %x\n", m->address, m->next, m->prev);
  }
}

void* memory_4kb_remove() {
  if (memory_4kb_free_first == nullptr) {
    return nullptr;
  }

  memory* memory = memory_4kb_free_first;
  memory_4kb_free_first = memory->next;
  memory_4kb_free_first->prev = memory->prev;

  printf("memory_4kb_remote: %x\n", memory->address);

  memset((void*)memory->address, 0, 8 * 512);
  return (void*)memory->address;
}

gdt_ptr* gdt_pointer = {0};
gdt_entry* gdt = {0};

#define SEGMENT_TYPE_CODE 0b1000
#define SEGMENT_TYPE_CODE_READ 0b1100

void init_gdt() {
  gdt = memory_4kb_remove();  // waste
  /// gdt[0] should be empty
  gdt[1].type = SEGMENT_TYPE_CODE;
  gdt[1].l = 1;  // long mode
  gdt[1].p = 1;  // present
  gdt[1].s = 1;  // code / data segment

  gdt_pointer = memory_4kb_remove();  // waste
  gdt_pointer->limit = 2 * 8 - 1;     // for two entries
  gdt_pointer->base = physical_memory_offset + (u64)gdt;

  // f  gdt_pointer = physical2virtual(gdt_pointer);

  // printf("pages_gdt: gdt=%x gdtp=%x\n", gdt, gdt_pointer);
}

// Each paging table has 512 entries of size 8 bytes on a 64bit architecture.
// Section 4.2 in Intel Developer Manual.
// Root paging structure needs to be put into CR3.
// We will do 4 level paging. Section 4.5.
// To enable we have to set
// CR0.PG = 1, CR4.PAE = 1, IA32_EFER.LME = 1, and CR4.LA57 = 0.
// Paging maps linear address space to physical address space.
// PML4[PML4Entries] -> Page Directory Pointer Table[PDPTEntries] -> 1Gb page |
// Page Directory Page Directory -> 2Mb page | Page Table Page Table Entry ->
// 4kb Page
pml4_entry* kernel_pml4;

// page_directory_pointer_table_entry page_directory_pointer_table[512];
// page_directory_entry page_directory[512];
// page_table_entry page_table[512];

// I can just extract the idx from the address like the
// CPU.
// 9 bits index into pml4
// 9 bits index into
// 9 bits index into
// 21 bits offset
// 48 bit total
u64 index_pml4(u64 address) {
  u64 offset = (address >> 39) & 0b111111111;
  printf("index_pml4: %d\n", offset);
  return offset;
}

u64 index_pdpt(u64 address) {
  u64 offset = (address >> 30) & 0b111111111;
  printf("index_pdpt: %d\n", offset);
  return offset;
}

u64 index_pd(u64 address) {
  u64 offset = (address >> 21) & 0b111111111;
  printf("index_pd: %d\n", offset);
  return offset;
}

// For 2mb page
// 21 bits results in offset 0 - 0x1fffff
// 0x200000 is 2mb.
u64 index_offset(u64 address) {
  u64 offset = (address >> 30) & 0b111111111111111111111;  // 21 bits
  return offset;
}

// u64 index_pml4(u64 address) {
//   u64 offset = 0;
//   if (address >= higher_half_start) {
//     address -= higher_half_start;
//     offset = 256;
//   }
//   return offset + address / ((u64)512 * 512 * 512 * 4096);
// }

void pages_test() {
  u16 idx = index_pml4(0);
  if (idx != 0) {
    printf("pages_init: expected index 0 but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_pml4(lower_half_end);
  if (idx != 255) {
    printf("pages_init: expected index 255 but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_pdpt(lower_half_end);
  if (idx != 511) {
    printf("pages_init: index_pdpt: expected index 511 but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_pd(lower_half_end);
  if (idx != 511) {
    printf("pages_init: index_pd: expected index 511 but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_offset(lower_half_end);
  if (idx != 0xffff) {
    printf("pages_init: expected index 0xffff but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_pml4(higher_half_start);
  if (idx != 256) {
    printf("pages_init: expected index 256 but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_pml4(higher_half_start);
  if (idx != 256) {
    printf("pages_init: expected index 256 but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_pdpt(higher_half_start);
  if (idx != 0) {
    printf("pages_init: expected index 0 but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_pd(higher_half_start);
  if (idx != 0) {
    printf("pages_init: expected index 0 but was %d\n", idx);
    __asm__("hlt");
  }
  idx = index_offset(higher_half_start);
  if (idx != 0) {
    printf("pages_init: expected index 0 but was %d\n", idx);
    __asm__("hlt");
  }

  // 0xffffffff800001cd
  idx = index_pml4(0xffffffff800001cd);
  // 511 = last 2gb
  // one slot is 512gb
  if (idx != 511) {
    printf("pages_init: expected index 511 but was %d\n", idx);
    __asm__("hlt");
  }
  // 510 = at the start of the last two gb, i.e. 0xffffffff80000000
  // one slot is 1gb
  idx = index_pdpt(0xffffffff800001cd);
  if (idx != 510) {
    printf("pages_init: expected index 510 but was %d\n", idx);
    __asm__("hlt");
  }
  // 0 - start of the last 2gb
  // one slot is 2mb
  idx = index_pd(0xffffffff800001cd);
  if (idx != 0) {
    printf("pages_init: expected index 0 but was %d\n", idx);
    __asm__("hlt");
  }
  //  idx = index_offset(0xffffffff800001cd);
  //  if (idx != 0x1cd) {
  //    printf("pages_init: expected index 0x1cd but was %d\n", idx);
  //    panic("error");
  //  }
}

// We cannot call malloc here. We have to get physical memory. We can make this
// better by creating a pool allocator. I think what I have is already very
// similar to a pool allocator, just the size is fixed. Let me add this here.
// The pool allocator takes physical memory in page frame size and splits it up
// into aligned 4kb chunks. The physical memory manager could just do it. It's a
// bit tricky because we have to manage memory again in physical space. What do
// I do? Make 4kb pages or write a small memory allocator? memory add/remove
// could be rewritten to take a linked list as input, then they are reusable but
// I don't want to change them now because I might introduce a bug. memory init
// could then also be abstracted to initialize itself with different block size.
// That's the best plan.

// When we allocate this we get a physical memory address. We can use it and
// move it around, but we cannot access it without a page table entry.
// Interestingly we only need to keep a pointer to the pml4 and we can patch it
// once we switch page tables.
pml4_entry* pages_allocate_pml4() {
  // TODO: check for nullptr
  pml4_entry* t = memory_4kb_remove();
  return t;
}

void pages_free_pml4(pml4_entry* table) {
  // loop and free
}

void pages_create_pml4_entry(pml4_entry* e) {
  printf("pages_create_pml4_entry: entry=%x\n", e);
  e->p = 1;
  e->rw = 1;  // kernel code should not be writeable so we need to configure
  // this.

  // next we need to allocate the next level
  // ouch we call malloc here?
  // at this point in time malloc works on the identity mapped memory.
  page_directory_pointer_table_entry* pdpt = memory_4kb_remove();

  e->pointer_table_ptr = (u64)pdpt >> 12;
}

void pages_create_pdpt_entry(page_directory_pointer_table_entry* e) {
  printf("pages_create_pdpt_entry: entry=%x\n", e);

  e->p = 1;
  e->rw = 1;

  page_table_entry* pd = memory_4kb_remove();
  e->directory_ptr = (u64)pd >> 12;
}

void pages_create_pd_entry(page_directory_entry* e, u64 physical) {
  printf("pages_create_pd_entry: to=%x to=%x entry=%x\n", physical,
	 physical >> 21, e);
  e->ptr = physical >> 21;
  e->p = 1;
  e->rw = 1;
  e->ps = 1;
}

u64 pages_virtual_memory_offset = 0;

void* physical2virtual(void* address) {
  return (void*)((u64)address + pages_virtual_memory_offset);
}

pml4_entry* pages_pml4(u64 address) {
  return physical2virtual(&kernel_pml4[index_pml4(address)]);
}

page_directory_pointer_table_entry* pages_pdpt(u64 address) {
  pml4_entry* e = pages_pml4(address);
  page_directory_pointer_table_entry* pdpt =
      (page_directory_pointer_table_entry*)((u64)e->pointer_table_ptr << 12);
  return physical2virtual(&pdpt[index_pdpt(address)]);
}

page_directory_entry* pages_pd(u64 address) {
  page_directory_pointer_table_entry* e = pages_pdpt(address);
  page_directory_entry* pd =
      (page_directory_entry*)((u64)e->directory_ptr << 12);
  return physical2virtual(&pd[index_pd(address)]);
}

void pages_map_one(u64 virtual, u64 physical) {
  printf("pages_map_one: from=%x to=%x\n", virtual, physical);

  pml4_entry* e = pages_pml4(virtual);
  if (e->p == 1) {
    printf("pages_map_one: pml4 entry present\n");
  } else {
    printf("pages_map_one: pml4 missing\n");
    pages_create_pml4_entry(e);
  }

  page_directory_pointer_table_entry* pdpt_e = pages_pdpt(virtual);
  if (pdpt_e->p == 1) {
    printf("pages_map_one: pdpt entry present\n");
  } else {
    printf("pages_map_one: pdpt entry missing\n");
    pages_create_pdpt_entry(pdpt_e);
  }

  page_directory_entry* pd_e = pages_pd(virtual);
  if (pd_e->p == 1) {
    printf("pages_map_one: pd entry present\n");
  } else {
    // overwrite or fail? basically a remap
    printf("pages_map_one: pd entry missing\n");
    pages_create_pd_entry(pd_e, physical);
  }
}

// pages_aligns changes the provided address to the containing page
u64 pages_align(u64 address) {
  return address & ~0x1FFFFF;
}

void pages_align_test() {
  u64 got = pages_align(0x200000);
  if (got != 0x200000) {
    printf("want 0x200000, got %x\n", got);
    panic("1");
  }
  got = pages_align(0x100000);
  if (got != 0x0) {
    printf("want 0x0, got %x\n", got);
    panic("2");
  }
  got = pages_align(0x1fffff);
  if (got != 0x0) {
    printf("want 0x0, got %x\n", got);
    panic("2");
  }
  got = pages_align(0x300000);
  if (got != 0x200000) {
    printf("want 0x200000, got %x\n", got);
    panic("1");
  }
  got = pages_align(0x400000);
  if (got != 0x400000) {
    printf("want 0x400000, got %x\n", got);
    panic("1");
  }
}
// pages_map_contiguous assumes the physical memory is already allocated and
// only maps it.
// TODO: specify whether end is inclusive or not.
void pages_map_contiguous(u64 virtual_address,
			  u64 physical_start,
			  u64 physical_end) {
  pages_align_test();
  // alignment is wrong. from is 1 mb which is in the middle of a page and 2.5
  // is in the middle of the next page.
  // we somehow need to normalize to and length.
  //
  // what do I want? If I get 1mb to 2.5 mb I want
  // page 0, page 1
  // using pages_align I get 0x0 and 0x200000, doing length is exactly page
  // size, dividing would return 1, 1 page too small.
  // what if i were to extend the 'end'. i.e. return 0x0 and 0x400000. Then the
  // result would be 2.

  u64 physical_start_aligned = pages_align(physical_start);
  u64 physical_end_aligned = pages_align(physical_end);

  printf("pages_map_contiguous: physical_start=%x physical_start_aligned=%x\n",
	 physical_start, physical_start_aligned);
  printf("pages_map_contiguous: physical_end=%x physical_end_aligned=%x\n",
	 physical_end, physical_end_aligned);

  u64 count = ((physical_end_aligned - physical_start_aligned) / page_size) +
	      1;  // poor mans ceil

  printf("pages_map_contiguous: mapping %d pages from %x to %x\n", count,
	 virtual_address, physical_start);

  for (u64 i = 0; i < count; i++) {
    pages_map_one(virtual_address + i * page_size,
		  physical_start_aligned + i * page_size);
  }
}

// what are pages used for? mapping things to different places. but also
// creating new physical memory. malloc needs physical memory so it can say 'do
// we still have some physical memory that we put in a page that i can use?'
// does the paging system know everything about physical memory?

void pages_init() {
  printf("pages_init: initializing pages\n");
  pages_test();

  // allocate our physical memory pool for page table entries.
  memory_4kb_pool_init();

  // As long as we know that kernel_pml4 is allocated we can check whether pages
  // exist. It's the entry point.
  kernel_pml4 = pages_allocate_pml4();

  // map
  // all memory
  // u64 size = kernel_physical_end - kernel_physical_start;
  // pages_map_contiguous(kernel_code_start, kernel_physical_start, size);
  // Where do we get the memory map from? We need to know which regions to map.

  // TODO: consider alignment. I think inside map_contiguous we can just align
  // to bigger sizes if necessary.
  pages_map_contiguous(physical_memory_offset, 0x0, 0x9FC00);

  pages_map_contiguous(physical_memory_offset + 0x100000, 0x100000, 0x7EE0000);

  // kernel
  // kernel goes from 0x2 to kernel end and we want to map it to max-2gb
  // compute how many pages it needs
  // kernel end - 0x2 / page_size and round up
  // create n pages at max-2b with contiguous physical space from 0x2
  // decided to map from 1mb to keep the boot structures.

  // does this choose the page size given a size in bytes?
  u64 size = kernel_physical_end - kernel_physical_start;
  // BUG:! kernel physical start contains loader so VMA won't be correct for
  // kernel code.
  pages_map_contiguous(kernel_code_start, 0x200000, kernel_physical_end);

  // 1mb area (included above?)
  // 4gb thing (included above?)

  // switch pml4.
  // any code now needs to use the memory mapped to the upper half
  // for hardware communication. networking code, ioapic (4gb), is ps2 code with
  // outb inb affected?
  //
  // the stack is also affected. should make a new kernel stack.
  // plan is to allocate stack in the location I planned to have it and copy the
  // old stack data there, then set the pointer.
  printf("pages_init: switch_cr3 to %x\n", kernel_pml4);
  init_gdt();
  //  __asm__("cli");
  switch_cr3(kernel_pml4, (void*)(physical_memory_offset + (u64)gdt_pointer));
  //  switch_gdt(gdt_pointer);
  pages_virtual_memory_offset = physical_memory_offset;
  //__asm__("sti");
  // gdt should be aligned on 8byte boundary
  printf("pages_init: new page tables active\n");

  // debugging
  // last entry of initial page table
  // 0x0000000000106023
  // last entry
  // 0x0000000607000003
  // last entry after shift fix
  // 0x0000000000607003
  // observation
  // mapping for physical memory seems to be missing because there's only the
  // 511 entry. There should be one around 256
  // No, it was there: 0x0000000605000003

  // now I expect we cannot access pml4 anymore
  return;
}

// Note about memory management.
// There is physical memory in the system. We get its address ranges from Grub.
// Then there is virtual memory (enforced in 64bit mode).
// Page tables describe a mapping from physical to virtual memory.
// A page can be 1Gb, 2mb, 4kb, ...
// A task has it's own virtual memory. There doesn't exist enough physical
// memory to fill this. Just some parts are mapped in via the page table.
// If a task needs memory it needs to request pages to be mapped to its virtual
// address space. Afterwards it can use this memory for all the objects it
// creates. We see that task memory can only grow by page size increments
// although they may be bigger or smaller than what the task needs. So there
// needs to be some kind of memory management to efficiently utilize the memory
// the task received. This is what malloc does.
//
// What will we do here? Currently we just allocate complete pages which is why
// we immediately run out of memory. Now we want malloc to request and manage
// only as much memory as necessary.
//
// Currently we only have the kernel page table.
//
// If there is no memory allocated to the task, ask the kernel for some memory
// however much is necessary to fullfill the request, in page size increments.
// Malloc then will only utilize the part that the task really needs. I.e. the
// task requests 1 byte but the page size is 2mb. So malloc will reserve 1 byte
// of memory. Malloc could keep a list of memory it gave out with the address as
// key. When we want to free memory we look into this list. This list also needs
// to live somewhere so we need to account for the size of this list when we
// request memory.
//
// Where should this structure live? It can be a list of free / used memory.
// Searching the list seems slow. O(n). Could be a hash map for O(1) lookup.
// Could be a tree for binary search. (What's the O? something log).
//
// When the task requests memory that memory needs to be contiguous.
//
// Interesting thought: Do pages need to be contiguous? I don't think so as long
// as they are mapped contiguously into the virtual address space. This is why
// alignment is so important. TODO: flesh this out.
//
// All too complicated. I will go with a free list again. On free we add blocks
// back and try to merge if possible. Fixed size 2mb heap.
//
// In the beginning we don't have memory again, so similarly to the free pages
// list we create a free list for malloc. The free pages I pre-allocated but
// with malloc that won't be possible.
//
// We can't know how big the list would be up front. We can do a worst case
// analysis. If the task requests 1 byte increments we create the biggest
// overhead because there is one structure allocated for 1 byte. Of course we
// could use as much as we have physical memory.
//
// I'll reuse the headers of the allocated memory that are used for freeing as
// free list structures.

typedef struct memory_header memory_header;
struct memory_header {
  u64 size;
  memory_header* next;
};

memory_header* malloc_free_list = nullptr;

void* malloc(u64 size) {
  // TODO: assert size > 0
  printf("malloc: %d\n", size);
  if (malloc_free_list == nullptr) {
    // Reserve a 2mb page.
    memory* m = memory_remove();
    // TODO: all these printfs should be converted to a debug logger.
    // printf("malloc: received %d bytes from kernel\n", m->size);
    memory_header* h = (memory_header*)(m->address);
    h->size = m->size;
    h->next = nullptr;
    malloc_free_list = h;
  }

  u64 actual_size = size + sizeof(memory_header) + sizeof(memory_header);
  // memory_header is 16 bytes. Times 2 is 32 bytes. If we allocate 1 byte we
  // have 32 bytes overhead.
  // printf("malloc: allocating %d bytes for the requested %d bytes\n",
  //	 actual_size, size);

  // First fit
  memory_header* m = malloc_free_list;
  memory_header* prev = nullptr;
  while (m != nullptr) {
    // Not big enough
    if (m->size < actual_size) {
      prev = m;
      m = m->next;
      continue;
    }

    // Positive cases. Perfect match or too big. In case it's too big we need to
    // chop it up. Maybe we can merge the cases by passing the perfect size to
    // the chop function which results in a no-op.
    if (m->size == actual_size) {
      u64 address = (u64)m;
      // printf("malloc2 perfect: %x\n", (void*)(address));

      // TODO: we don't need to do this anymore. It's the same type.
      memory_header* h = (memory_header*)address;
      h->size = actual_size;

      // TODO: I am not removing this from the free list so I expect it to be
      // used twice. I.e. log will show perfect twice.
      // Yes this happened.
      // Remove it from the free list.
      if (m == malloc_free_list) {
	malloc_free_list = m->next;
      } else {
	// We should have prev
	prev->next = m->next;
      }

      // printf("malloc: current free list\n");
      // for (memory_header* m = malloc_free_list; m != nullptr; m = m->next)
      // {	printf("malloc: %x %d\n", m, m->size);
      // }

      return (void*)(address + sizeof(memory_header));
    } else {
      // We need to chop it up. Because the address is the address of the
      // struct, it needs to move.

      // Naively we would:
      // Move the current header after the memory we allocate.
      // Reduce the size in the header by the size of the allocation
      // (actual_size).
      // Update the free list's previous items next pointer to
      // the new header. Allocate the space with header.

      // IMPORTANT: It was bigger than actual_size, but we don't know if it was
      // big enough to fit another memory_header into it. We can check, if
      // it's not big enough we could merge with the next slot if it's free, but
      // there is no guarantee for that. If it's not free we are out of luck.
      // Maybe best is to check if it's big enough. Worst case we write a header
      // for a 0 byte block. At least we can use it when merging. Alternatively
      // we overallocate and set the size of the actual allocation to the max
      // possible. This way we save one hop when iterating over the free list.

      // For now I went with the 0 size element by changing the continue
      // condition above.

      u64 address = (u64)m;

      // printf("malloc: found %d bytes slot to chop at %x\n", m->size,
      // address);

      memory_header* moved_m = (memory_header*)(address + actual_size);
      moved_m->size = m->size - actual_size;
      moved_m->next = m->next;

      // If we are moving the actual first element, update it.
      if (m == malloc_free_list) {
	malloc_free_list = moved_m;
      } else {
	// We should have prev
	prev->next = moved_m;
      }

      // Allocate the space with a header and return the address after the
      // header.
      memory_header* h = (memory_header*)address;
      h->size = actual_size;

      // printf("malloc: current free list\n");
      // for (memory_header* m = malloc_free_list; m != nullptr; m = m->next)
      // {	printf("malloc: %x %d\n", m, m->size);
      // }

      return (void*)(address + sizeof(memory_header));
    }
  }

  // If we came this far we didn't find free memory.
  // TODO: how can we assert?
  printf("malloc: OOM\n");
  __asm__("hlt");
  return nullptr;
}

void free(void* memory) {
  // All we know currently is the address. We stored a struct that contains the
  // size before the address.
  memory_header* h = (memory_header*)memory - 1;
  printf("free: %d\n", h->size);

  // Put it back into the free list.
  // Sort it by address so that merging is easier.
  // Hunch: I only have to check prev and next to see if they are mergable.

  u64 address = (u64)h;

  memory_header* current = malloc_free_list;
  memory_header* prev = nullptr;
  while (current != nullptr) {
    // TODO: having the address in here is waste because we already know it.
    if ((u64)current < (u64)address) {
      prev = current;
      current = current->next;
      continue;
    }
    break;
  }

  // What is this state?
  if (current == nullptr) {
  } else {
    // Where do I get that object from? The memory. Do I create this again
    // inside the free memory? I could call malloc now, but then I would have
    // some overhead from the header. Can I reuse the header somehow?
    // First thought: but then I can't free the header.
    // Second thought: doesn't matter because it will just become a header again
    // anyway.
    // It will be cheaper if I re-use the headers because otherwise I write down
    // the size of the thing again.

    // I don't even need a pointer in them because I can calculate it. I how how
    // far the next item is away. But I cannot decide between free and used
    // memory this way. I could certainly find the next header but wouldn't know
    // what to do with it.

    // I am unhappy that I skimmed the book because it robbed me of the
    // opportunity to think.

    // Let's do without merging for now.

    // Now current is larger than address.
    // Convert the header to a free list element.
    prev->next = h;
    h->next = current;

    // Set head of list to smallest address. I assume the big free block is
    // always at the end because we cut it from the front.
    if (h < malloc_free_list) {
      malloc_free_list = h;
    }
  }

  // printf("free: current free list\n");
  // for (memory_header* m = malloc_free_list; m != nullptr; m = m->next) {
  //   printf("free: %x %d\n", m, m->size);
  // }
}
// Memory end
