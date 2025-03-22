#pragma once
#include "types.h"


// Memory start
// Defined in linker script.
// Need to take the address, because the address is the value in this case.
// ref: https://sourceware.org/binutils/docs/ld/Source-Code-Reference.html
extern u64 _kernel_start;
extern u64 _kernel_end;

void memory_init(u64 base_address, u64 length);
void* malloc(u64 size);
void free(void* memory);
