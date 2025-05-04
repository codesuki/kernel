#pragma once

#include "interrupt.h"
#include "memory.h"
#include "message.h"
#include "types.h"

enum task_state { running, blocked, finished };
typedef enum task_state task_state;

typedef struct task task;

// Consider this order then r8 and index 8 match
// //
//       rax = 0x0000000000000202
//        rbx = 0x0000000000c01600
//        rcx = 0x0000000000000f9f
//        rdx = 0xffff832000200010
//        rsi = 0xffff832000200010
//        rdi = 0xffff80000010be10
//        rbp = 0xffff80000010bff0
//        rsp = 0xffff80000010bdf8 -> 0xffffffff8000338e kernel.bin`kmain + 2087
//        at kernel.c:2083:10
//         r8 = 0x000000000000004f
//         r9 = 0x0000000000000034
//        r10 = 0x000000000000004f
//        r11 = 0x0000000000000000
//        r12 = 0x0000000000000000
//        r13 = 0x0000000000000000
//        r14 = 0x0000000000000000
//        r15 = 0x0000000000000000
//        rip = 0xffffffff8000028b kernel.bin`switch_task + 96
//        kernel.bin`switch_task + 96
//     eflags = 0x00000202
//         cs = 0x00000008
//         ss = 0x00000000
//         ds = 0x00000000
//         es = 0x00000000
//         fs = 0x00000000
//         gs = 0x00000000
//    fs_base = 0x0000000000000000
//    gs_base = 0x0000000000000000
//   k_gs_base = 0x0000000000000000
//        cr0 = 0x0000000080000011
//        cr2 = 0x0000000000000000
//        cr3 = 0x0000000000404000
//        cr4 = 0x0000000000000020
//        cr8 = 0x0000000000000000
//       efer = 0x0000000000000501
//
struct task {
  u64 rsp;
  u64 rip;
  u64 rax;
  u64 rbx;
  u64 rcx;
  u64 rdx;
  u64 rsi;
  u64 rdi;
  u64 rbp;
  u64 r8;
  u64 r9;
  u64 r10;
  u64 r11;
  u64 r12;
  u64 r13;
  u64 r14;
  u64 r15;
  u64 rflags;
  u64 cr3;
  u64 cs;
  u64 ss;
  // Don't change the order above. It will impact assembly code in switch_task.s
  task* next;
  task_state state;
  u64 sleep_until;
  // Using a non pointer here means we need to import the header because we need
  // to know the size.
  message_queue queue;
  u64 timeout;
  bool timed_out;
};

extern void switch_task(task* current, task* next);
extern void task_replace(task* task);

// void task_new(u64 entry_point, u64 stack_bottom, u32 stack_size, task* task);
task* task_new_user(pml4_entry* pml4, u64 entry_point);
task* task_new_kernel(u64 entry_point);
task* task_remove(task* task);
void task_mark_finished(task* task);

void task_update_context(task* task, interrupt_registers* regs);
void update_regs_from_task(task* task, interrupt_registers* regs);

void sleep(u64 ms);

void idle_task();

extern task* task_current;
// My guess is that we probably want to 'lose' the initial kernel loader task.
// Which means we create a new stack and switch to a new task that we define
// here. What happens with the initial kernel stack? Can we clean it up somehow?
// It won't be needed anymore because we jump out of kmain.
extern task* task_scheduler;
extern task* task_idle;
extern task* service_mouse;
extern task* service_keyboard;
extern task* service_network;
extern task* service_dhcp;
extern task* service_dns;
