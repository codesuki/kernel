#include "task.h"
#include "interrupt.h"
#include "print.h"
#include "memory.h"
#include "time.h"

task* task_first = nullptr;
task* task_current = nullptr;
// My guess is that we probably want to 'lose' the initial kernel loader task.
// Which means we create a new stack and switch to a new task that we define
// here. What happens with the initial kernel stack? Can we clean it up somehow?
// It won't be needed anymore because we jump out of kmain.
task* task_scheduler = nullptr;
task* task_idle = nullptr;
task* service_mouse = nullptr;
task* service_keyboard = nullptr;
task* service_network = nullptr;
task* service_dhcp = nullptr;
task* service_dns = nullptr;

void task_mark_finished(task* task) {
  task->state = finished;
}

void trampoline() {
  printf("finished a task: %d\n", task_current->id);
  task_mark_finished(task_current);
  // Before we called hlt here which wastes time, but it also resulted in a
  // crash because after a hardware interrupt we would jump behind the hlt.
  switch_task(task_current, task_scheduler);
}

// TODO: added task prefix for namespacing. Check C best practices.
void task_setup_stack(u64 rsp) {
  // TODO: we could null it, but maybe that's too much work, also, how big is it
  // even?
  // Make space for the pointer by subtracting it. The stack grows down.
  u64* s = (u64*)(rsp - sizeof(u64*));
  s[0] = (u64)&trampoline;
}

// TODO: actually we want to allocate a new stack/task now..
void task_new(u64 entry_point, u64 stack_bottom, u32 stack_size, task* task) {
  printf("New task %x\n", entry_point);
  // memset to 0
  task->state = running;
  task->queue = nullptr;

  task->eip = entry_point;

  // Set rsp to end of stack memory because it grows down.
  // E.g. Stack is from 0x200000 to 0x400000. We set it to 0x400000.
  u64 rsp = stack_bottom + stack_size;
  task_setup_stack(rsp);
  rsp = rsp - sizeof(u64*);
  // We push the trampoline pointer on the stack so we need to update rsp.
  task->rsp = rsp;

  if (task_first == nullptr) {
    // First task
    task_first = task;
    task_first->next = task_first;
  } else {
    // Add to end of list
    struct task* t = task_first;
    for (; t->next != task_first; t = t->next) {
    }
    t->next = task;
    task->next = task_first;
  }
}

#define STACK_SIZE 8192

task* task_new_malloc(u64 entry_point) {
  task* task = (struct task*)malloc(sizeof(*task));
  u64 stack = (u64)malloc(STACK_SIZE);
  task_new(entry_point, stack, STACK_SIZE, task);
  return task;
}

task* task_remove(task* task) {
  printf("Removing task %d\n", task->id);

  // Case 1: It's the first task
  // Assumption: Never happens because it's the scheduler.
  if (task_first == task) {
    task_first = task->next;
  }

  // Case 2: It's in the middle or the last task. Behavior is the same.
  struct task* t = task_first;
  for (; t->next != task_first; t = t->next) {
    if (t->next == task) {
      t->next = task->next;
      return t;
    }
  }
}


void sleep(u64 ms) {
  // how do I find the task that this should apply
  // let's assume it's current
  task_current->sleep_until = get_global_timer_value() + ms * _1ms;
  // We want to reschedule after the sleep because the current task becomes
  // blocking.
  // reschedule()
  // let's call hlt for now
  // printf("sleep: sleeping for %dms\n", ms);
  switch_task(task_current, task_scheduler);
}

void print_task(task* task) {
  printf(
      "task: rsp = %x, eip = %x\nrax = %x, rcx = %x, rdx = %x\nrsi = %x, rdi = "
      "%x, r8 = %x\nr9 = %x, r10 = %x, r11 = %x\n",
      task->rsp, task->eip, task->rax, task->rcx, task->rdx, task->rsi,
      task->rdi, task->r8, task->r9, task->r10, task->r11);
}

void print_regs(interrupt_registers* regs) {
  printf(
      "regs:  rsp = %x, eip = %x\nrax = %x, rcx = %x, rdx = %x\nrsi = %x, rdi "
      "= %x, r8 = %x\nr9 = %x, r10 = %x, r11 = %x\n",
      regs->rsp, regs->eip, regs->rax, regs->rcx, regs->rdx, regs->rsi,
      regs->rdi, regs->r8, regs->r9, regs->r10, regs->r11);
}

void task_update_context(task* task, interrupt_registers* regs) {
  // printf("task before\n");
  // print_task(task);
  // If the layout were the same we could memcpy.
  task->rsp = regs->rsp;
  task->eip = regs->eip;
  task->rax = regs->rax;
  task->rbx = regs->rbx;
  task->rcx = regs->rcx;
  task->rdx = regs->rdx;
  task->rsi = regs->rsi;
  task->rdi = regs->rdi;
  task->rbp = regs->rbp;
  task->r8 = regs->r8;
  task->r9 = regs->r9;
  task->r10 = regs->r10;
  task->r11 = regs->r11;
  task->r12 = regs->r12;
  task->r13 = regs->r13;
  task->r14 = regs->r14;
  task->r15 = regs->r15;
  task->rflags = regs->rflags;

  // printf("task after\n");
  // print_task(task);
}

// I guess this should be normalized and just one function, from -> to.
void update_regs_from_task(task* task, interrupt_registers* regs) {
  // printf("regs before\n");
  // print_regs(regs);
  //  If the layout were the same we could memcpy.
  regs->rsp = task->rsp;
  regs->eip = task->eip;
  regs->rax = task->rax;
  regs->rbx = task->rbx;
  regs->rcx = task->rcx;
  regs->rdx = task->rdx;
  regs->rsi = task->rsi;
  regs->rdi = task->rdi;
  regs->rbp = task->rbp;
  regs->r8 = task->r8;
  regs->r9 = task->r9;
  regs->r10 = task->r10;
  regs->r11 = task->r11;
  regs->r12 = task->r12;
  regs->r13 = task->r13;
  regs->r14 = task->r14;
  regs->r15 = task->r15;
  regs->rflags = task->rflags;

  // printf("regs after\n");
  // print_regs(regs);
}



void idle_task() {
  while (1) {
    // printf("idle_task: going to idle\n");
    __asm__ volatile("hlt");
    // printf("idle_task: woke up\n");
  }
}
// Task end
