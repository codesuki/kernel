#include "memory.h"
#include "print.h"

// // Memory start
// // Defined in linker script.
// // Need to take the address, because the address is the value in this case.
// // ref: https://sourceware.org/binutils/docs/ld/Source-Code-Reference.html
// extern u64 _kernel_start;
// extern u64 _kernel_end;

const u32 page_size = 0x200000;  // 2mb

typedef struct memory memory;
struct memory {
  u64 address;
  u64 size;  // 4kb, 2mb, etc.
  memory* next;
  memory* prev;
};

// Current thought, keep two lists. Free and used memory.
// When we malloc we take from the free list and put it into the used list.
// When we free we search the address in the used list and move it back.
// What other way is there to implement free?
memory* memory_free_first = nullptr;
memory* memory_used_first = nullptr;

// memory_init creates the first memorys in physical memory, because we cannot
// call malloc.
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

  printf("memory_init: available memory with base %x and length %x\n",
	 base_address, length);

  printf("memory_init: kernel located between %x - %x\n", &_kernel_start,
	 &_kernel_end);

  // TODO: I think this overcounts. I.e. the last page is too big for the
  // available physical memory.
  // Maybe for now we don't track the memorys themselves.
  // How many do we need?
  u64 page_count = length / page_size;
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
  // printf("malloc2: OOM\n");
  return nullptr;
}

void free(void* memory) {
  // All we know currently is the address. We stored a struct that contains the
  // size before the address.
  memory_header* h = (memory_header*)memory - 1;
  // printf("free: %d\n", h->size);

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
