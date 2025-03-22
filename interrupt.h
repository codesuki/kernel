#pragma once

#include "types.h"

struct interrupt_registers {
  //  uint32 ds;                                     // data segment selector
  u64 r15, r14, r13, r12, r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx, rax;
  u64 int_no, err_code;
  u64 eip, cs, rflags, rsp, ss;  // pushed by cpu after interrupt
} __attribute__((packed));
typedef struct interrupt_registers interrupt_registers;
