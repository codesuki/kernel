#pragma once

#include "types.h"
#include "interrupt.h"

enum task_state { running, blocked, finished };
typedef enum task_state task_state;

typedef struct message message;

typedef struct task task;
struct task {
  u8 id;  // TODO: remove
  u64 rsp;
  u64 eip;
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
  // Don't change the order above. It will impact assembly code in switch_task.s
  task* next;
  task_state state;
  u64 sleep_until;
  message* queue;
  u64 timeout;
  bool timed_out;
};

extern void switch_task(task* current, task* next);
extern void task_replace(task* task);

// void task_new(u64 entry_point, u64 stack_bottom, u32 stack_size, task* task);
task* task_new_malloc(u64 entry_point);
task* task_remove(task* task);
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
