#include "elf.h"
#include "fat32.h"
#include "interrupt.h"
#include "io.h"
#include "lib.h"
#include "memory.h"
#include "message.h"
#include "network.h"
#include "print.h"
#include "rtl8139.h"
#include "task.h"
#include "time.h"
#include "types.h"

/* task-state segment */
struct tss_struct {
  u16 link;
  u16 link_r;
  u32 esp0;
  u16 ss0;
  u16 ss0_r;
  u32 esp1;
  u16 ss1;
  u16 ss1_r;
  u32 esp2;
  u16 ss2;
  u16 ss2_r;
  u32 cr3;
  u32 eip;
  u32 eflags;
  u32 eax;
  u32 ecx;
  u32 edx;
  u32 ebx;
  u32 esp;
  u32 ebp;
  u32 esi;
  u32 edi;
  u16 es;
  u16 es_r;
  u16 cs;
  u16 cs_r;
  u16 ss;
  u16 ss_r;
  u16 ds;
  u16 ds_r;
  u16 fs;
  u16 fs_r;
  u16 gs;
  u16 gs_r;
  u16 iopb_r;
  u16 iopb;
} __attribute__((packed));
typedef struct tss_struct tss_t;

// #IFDEF __x86_64__
struct idt_entry {
  u16 offset_start;
  u16 selector;
  u8 zero;
  u8 type_attr;
  u16 offset_mid;
  u32 offset_end;
  u32 reserved;
} __attribute__((packed));
typedef struct idt_entry idt_entry_t;
/* #ELSE */
/* struct idt_entry_struct { */
/*   uint16 offset_start; */
/*   uint16 selector; */
/*   uint8 zero; */
/*   uint8 type_attr; */
/*   uint16 offset_end; */
/* } __attribute__((packed)); */
/* typedef struct idt_entry_struct idt_entry_t; */
/* #ENDIF */

// The base addresses of the IDT should be aligned on an 8-byte boundary to
// maximize performance of cache line fills
struct idt_ptr_struct {
  // TODO: this is supposed to be 32 bit but in 64 bit code this will be 64bit.
  // See 6.10

  // Also: In 64-bit mode, the instruction’s operand size is fixed at 8+2 bytes
  // (an 8-byte base and a 2-byte limit).
  // ref: https://www.felixcloutier.com/x86/lgdt:lidt
  u16 limit;

  u64 base;
} __attribute__((packed));
typedef struct idt_ptr_struct idt_ptr_t;

// gdt_ptr_t gdt;
// gdt_entry_t gdt_entries[6];

idt_ptr_t idt;
idt_entry_t idt_entries[256] = {0};

tss_t tss;

// extern void gdt_update(gdt_ptr_t*);
extern void idt_update(idt_ptr_t*);

void gdt_setup();
void gdt_set_entry(u32, u32, u32, u8, u8);
void gdt_set_gate(u32 entry, u32 base, u32 limit, u8 access, u8 flags);

void idt_setup();
void idt_set_gate(u32 interrupt, u64 offset, u16 selector, u8 type_attr);

extern int isr0;         // divide error, no error code, fault
extern void isr3(void);  // breakpoint, no error code, trap
extern void isr4(void);  // overflow, no error code, trap
extern void isr8(void);
extern int isr11(void);   // segment not present
extern int isr12(void);   // stack-segment fault, error code, fault
extern void isr13(void);  // general protection fault, error code, fault
extern void isr14(void);  // page fault, error code, fault
extern void isr32(void);
extern void isr0x31(void);
extern void isr0x32(void);
extern void isr0x33(void);
extern void isr0x34(void);

#define EXCEPTION_PAGE_FAULT 14

char* interrupt_names[22] = {
    "Divide Error",
    "1",
    "2",
    "3",
    "Overflow",
    "5",
    "6",
    "7",
    "Double Fault Exception",  // 8
    "CoProcessor Segment Overrun (reserved)",
    "Invalid TSS",
    "Segment Not Present",
    "Stack Segment Fault",
    "General Protection Exception",  // 13
    "Page Fault Exception",          // 14
    "15",
    "16",
    "17",
    "18",
    "19",
    "20",
    "21",
};

typedef union {
  u32 raw;
  struct {
    u32 p : 1;
    u32 wr : 1;
    u32 us : 1;
    u32 rsvd : 1;
    u32 id : 1;
    u32 pk : 1;
    u32 ss : 1;
    u32 hlat : 1;
    u32 reserved : 24;
  } flags;
} page_fault_error_t;

void ps2_wait_ready() {
  // Bit 1 is the Input Buffer Full flag.
  // 0 = input buffer is empty
  // 1 = input buffer is full
  // We wait until it's empty.
  // ref: Input buffer
  // https://web.archive.org/web/20210417040153/http://www.diakom.ru/el/elfirms/datashts/Smsc/42w11.pdf
  while (inb(0x64) & 0x2) {
  };
}

void ps2_wait_data() {
  // Bit 0 is the Ouput Buffer Full flag
  // 0 = output buffer is empty
  // 1 = output buffer is full
  // We wait until it's full.
  while (!(inb(0x64) & 0x1)) {
  };
}

struct mouse_data {
  u8 data;
  u8 x;
  u8 y;
};
typedef struct mouse_data mouse_data_t;

// Task declarations

void task1(u8 id) {
  while (1) {
    printf("running task 1 %d\n", id);
    sleep(2000);
  }
}

void task2(u8 id) {
  printf("running task 2 %d\n", id);
}

// Let's say I want to make this my first user task.
// I need
// GDT entries for user code and data / stack
// map the binary to 0x0
// allocate a stack and map it
// somehow transfer control to main()
// how do I even know it ran?
// easiest would be to let it 'print', but we cannot do that yet.
// so the goal would be to make it do a syscall, any syscall, that prints
// something.

void task_network() {
  // This will try to resolve the host to the ip and send the echo, but it
  // relies on the network stack being up. We need to wait for that.
  // Could just be a global for now, but that's boring to continue.
  // printf("task_network: sending echo\n");
  // send_echo();
}

int mouse_bytes_received = 0;
u8 mouse_data[3] = {0};

// This function will look the same for the keyboard, because it's the ps/2
// driver. It just differs where it sends the byte. One time it's to the mouse
// driver and one time it's to the keyboard driver.
void mouse_handle_interrupt() {
  // printf("mouse_handle_interrupt\n");
  //  data:
  //  bit 0:
  //  bit 1:
  //  bit 2:
  //  bit 3:
  //  bit 4: x sign
  //  bit 5: y sign
  //  bit 6: x overflow
  //  bit 7: y overflow

  // 1st byte: data
  // 2nd byte: x
  // 3rd byte: y

  u8 byte = inb(0x60);

  // printf("mouse_handle_interrupt: %x\n", byte);

  u8* ps2_byte = malloc(sizeof(u8));
  *ps2_byte = byte;

  // TODO: find this service differnetly
  message_send(&service_mouse->queue, message_type_ps2_byte, ps2_byte);

  // TODO: I have the feeling this is the ps2 driver not the mouse driver.
}

void keyboard_handle_interrupt() {
  u8 byte = inb(0x60);

  u8* ps2_byte = malloc(sizeof(u8));
  *ps2_byte = byte;

  // If I were to merge the things into a ps/2 handler we would need to know the
  // IRQ, so we can map it to devices.
  message_send(&service_keyboard->queue, message_type_ps2_byte, ps2_byte);
}

// keyboard_service should convert scancodes which can be several bytes to
// something more useful for the system. Scancodes are divided into make, i.e.
// key pressed, and break, i.e. key released. Currently this only handles make
// scancodes.
void keyboard_service() {
  while (true) {
    message m;
    message_receive(&task_current->queue, &m);

    u8 scancode = *(u8*)(m.data);
    printf("scancode: %x\n", scancode);

    // Scancode set 2
    // ref:
    // https://webdocs.cs.ualberta.ca/~amaral/courses/329/labs/scancodes.html
    switch (scancode) {
      case 0x5a:  // enter
	printf("\n");
	break;
      case 0x29:  // space
	printf(" ");
	break;
      case 0x1c:
	printf("a");
	break;
      case 0x32:
	printf("b");
	break;
      case 0x21:
	printf("c");
	break;
      case 0x23:
	printf("d");
	break;
      case 0x24:
	printf("e");
	break;
      case 0x2b:
	printf("f");
	break;
      case 0x34:
	printf("g");
	break;
      case 0x33:
	printf("h");
	break;
      case 0x43:
	printf("i");
	break;
      case 0x3b:
	printf("j");
	break;
      case 0x42:
	printf("k");
	break;
      case 0x4b:
	printf("l");
	break;
      case 0x3a:
	printf("m");
	break;
      case 0x31:
	printf("n");
	break;
      case 0x44:
	printf("o");
	break;
      case 0x4d:
	printf("p");
	break;
      case 0x15:
	printf("q");
	break;
      case 0x2d:
	printf("r");
	break;
      case 0x1b:
	printf("s");
	break;
      case 0x2c:
	printf("t");
	break;
      case 0x3c:
	printf("u");
	break;
      case 0x2a:
	printf("v");
	break;
      case 0x1d:
	printf("w");
	break;
      case 0x22:
	printf("x");
	break;
      case 0x35:
	printf("y");
	break;
      case 0x1a:
	printf("z");
	break;
      case 0x45:
	printf("0");
	break;
      case 0x16:
	printf("1");
	break;
      case 0x1e:
	printf("2");
	break;
      case 0x26:
	printf("3");
	break;
      case 0x25:
	printf("4");
	break;
      case 0x2e:
	printf("5");
	break;
      case 0x36:
	printf("6");
	break;
      case 0x3d:
	printf("7");
	break;
      case 0x3e:
	printf("8");
	break;
      case 0x46:
	printf("9");
	break;
    }

    // Scancode set 1
    // key released
    // It seems this is numbered from top left to bottom right.
    // switch (scancode) {
    //   case 0x1c:  // enter
    //	printf("\n");
    //	break;
    //   case 0xb9:  // space
    //	printf(" ");
    //	break;
    //   case 0x9e:
    //	printf("a");
    //	break;
    //   case 0xb0:
    //	printf("b");
    //	break;
    //   case 0xae:
    //	printf("c");
    //	break;
    //   case 0xa0:
    //	printf("d");
    //	break;
    //   case 0x92:
    //	printf("e");
    //	break;
    //   case 0xa1:
    //	printf("f");
    //	break;
    //   case 0xa2:
    //	printf("g");
    //	break;
    //   case 0xa3:
    //	printf("h");
    //	break;
    //   case 0x97:
    //	printf("i");
    //	break;
    //   case 0xa4:
    //	printf("j");
    //	break;
    //   case 0xa5:
    //	printf("k");
    //	break;
    //   case 0xa6:
    //	printf("l");
    //	break;
    //   case 0xb2:
    //	printf("m");
    //	break;
    //   case 0xb1:
    //	printf("n");
    //	break;
    //   case 0x98:
    //	printf("o");
    //	break;
    //   case 0x99:
    //	printf("p");
    //	break;
    //   case 0x90:
    //	printf("q");
    //	break;
    //   case 0x93:
    //	printf("r");
    //	break;
    //   case 0x9f:
    //	printf("s");
    //	break;
    //   case 0x94:
    //	printf("t");
    //	break;
    //   case 0x96:
    //	printf("u");
    //	break;
    //   case 0xaf:
    //	printf("v");
    //	break;
    //   case 0x91:
    //	printf("w");
    //	break;
    //   case 0xad:
    //	printf("x");
    //	break;
    //   case 0x95:
    //	printf("y");
    //	break;
    //   case 0xac:
    //	printf("z");
    //	break;
    // }
  }
}

// I imagine there are two parts to a driver. One part reads the data quickly
// from the device to unblock it in case it has limited buffer size like a
// network card or even keyboard.
// The other part waits for this data to process it, so it could be a task
// that waits on device/driver specific data. It then pieces together the data
// like in the mouse driver case, 3 bytes make 1 packet, and publishes it to
// consumers. Consumers can be applications. The terminal waiting for keyboard
// input, an application waiting for a network packet, etc.
//
// We need some kind of data transport mechanism.
// Should be usable in several places.
// Is it a queue? Would make sense.
// Add a new thing: put() / enqueue()
// Is there something waiting?: peek()
// Take a thing: get() / dequeue()
//
// What if there are multiple subscribers?
// If one dequeues a thing it's lost.
// Do we have one queue per subscriber?
// We maybe have a type, i.e. 'mouse event', 'keyboard event'.
// We have a registry where consumers register for the type of event they are
// interested in.
//
// How do we handle copying of the data?
// 1. The consumer could provide a buffer where we copy the data and the
// consumer needs to clean it up. We re-use the buffer or clean it up once
// every consumer got the data.
// 2. We hand the consumer a copy and the consumer cleans it up.
// 3. We hand the consumer the original which is reference counted and the
// consumer needs to tell us they are done with it. Benefit over 2 is we can
// use a pool.
//
// Let's go with 1 for simplicity.

// TODO: just to make it compile. what to do about this?
extern int mouse_x;
extern int mouse_y;

void mouse_service() {
  while (1) {
    // Currently the mouse interrupt handler knows this tasks queue and sends it
    // there directly. We may want to decouple this so that interrupt data can
    // be sent to whatever registers.

    // message_register registers a queue to receive messages of a specific
    // type.
    // TODO: This is too boring to work on right now.
    // message_register(mouse_byte, task_current->queue);

    message m;
    message_receive(&task_current->queue, &m);

    u8* byte = m.data;
    mouse_data[mouse_bytes_received++] = *byte;
    // This was allocated by the sender. How can we be sure the sender does not
    // access it afterwards? This will be even trickier once sender and receiver
    // are not in the same address space hence I will delay.
    free(byte);
    if (mouse_bytes_received == 3) {
      // packet is complete
      mouse_bytes_received = 0;
      // the 9 bit two's complements relative x,y values come in 2 pieces.
      // an 8 bit value and a sign bit.
      // wikipedia says to subtract the sign bit. extract and subtract.

      // x,y,data is what we need.
      mouse_data_t data = {
	  .data = mouse_data[0], .x = mouse_data[1], .y = mouse_data[2]};
      int16_t rel_x = data.x - ((data.data << 4) & 0x100);
      int16_t rel_y = -(data.y - ((data.data << 3) & 0x100));

      // Restrict to terminal width and height.
      mouse_x = min(max(0, mouse_x + rel_x), 79);
      mouse_y = min(max(0, mouse_y + rel_y), 24);

      // If there is no print we don't render the mouse pointer. Therefore
      // mouse events should trigger a render. printf("mouse_service: %d
      // %d\n", mouse_x, mouse_y);
      display();
    }
  }
}

// Bug:
// At some point the mouse freezes and the scheduling stops, but keyboard
// interrupts are printed.

void schedule() {
  while (1) {
    __asm__ volatile("cli");
    // printf("schedule: start loop\n");
    // TODO: this crashes if task_current == nullptr.

    // Instead of just picking the next task lets iterate until we find a good
    // task, because tasks may sleep now.
    // We should probably now loop around to the beginning.
    u64 now = get_global_timer_value();

    // Note:
    // Why did I take so much time to implement this? (2h) I tried to wing it
    // just writing some code, there were a bunch of edge cases that did not
    // work. I did not take the time to document the rules (now written down
    // below). Should have written some pseudo code too.

    // Rules
    // 1. We want to pick the next task.
    // 2. If no task is ready we want to try and pick the next task again.
    // 3. If there is just one task it should work the same. This task should
    // always be selected.
    // 4. We don't want to pick the scheduler because it's always ready. It's
    // just the fallback.
    // 5. Finished tasks are deleted.

    // task_current was marked to be removed.
    // Assumption: when a task finished it's always task_current when coming
    // here. Probably this changes with multiple CPUs.
    // If we would return to this task we would segfault because we jump after
    // the halt to a RET and the stack is empty.
    if (task_current->state == finished) {
      // printf("schedule: cleaning up finished task\n");
      // task_current is the last task that ran.
      task_current = task_remove(task_current);
    }

    task* next_task = task_idle;

    // TODO:
    // - remove all _t typedefs
    // - don't cast void pointers
    // - integer return for verb function errors
    // - boolean return if function name is conditional has, is etc.
    // - use variable name in sizeof

    task* t = task_current->next;
    // If there is just one task (scheduler?) this will not run.
    for (; t != task_current; t = t->next) {
      // TODO: how to get rid of all these exceptions?
      // scheduler and idle task are just always there and they are invoked by
      // us. The scheduler will not schedule itself. The idle task could
      // probably be handled in the list as the lowest priority task that is
      // always ready.
      // The input services... when they are unblocked they could be added to
      // the prioritized task queue.
      // It seems having some kind of ordering vs. round robin may simplify
      // the code, but it's still not essential for progress.

      // Skip the scheduler task itself We probably never want to remove the
      // scheduler. If we want, we need to move this down.
      if (t == task_scheduler) {
	// printf("scheduler: this is the scheduler itself\n");
	continue;
      }

      if (t == task_idle) {
	// printf("scheduler: this is the idle task\n");
	continue;
      }

      if (t->state == blocked) {
	// Check if it can be unblocked
	// Sleep has priority. Even if there is a message waiting, we sleep.
	if (t->sleep_until != 0 && now > t->sleep_until) {
	  t->sleep_until = 0;
	  t->state = running;
	  next_task = t;
	  break;
	} else if (message_peek(&t->queue) == true) {
	  // BUG: having any of those print statements results in the mouse
	  // not moving. Found it. Random guess. The schedule timer was 1ms
	  // and printing took too much time so tasks had no time and we were
	  // constantly scheduling. When I used the debugger I somehow ended
	  // up inside the scheduler when I expected to end up in a task. I
	  // didn't make the connection. I thought my pointers were messed up.
	  // I need to know somehow when an interrupt hits.

	  // printf("service_mouse %x service_keyboard %x\n", service_mouse,
	  //	 service_keyboard);
	  // printf("schedule: found blocked task %x that can be unblocked\n",
	  // t);
	  t->state = running;
	  next_task = t;
	  break;
	} else if (t->timeout != 0 && now > t->timeout) {
	  t->timeout = 0;
	  t->timed_out = true;
	  t->state = running;
	  next_task = t;
	  break;
	}
	continue;
      }

      // // This task is sleeping
      // // TODO: integrate this somehow with state blocked.
      // if (t->sleep_until > now) {
      //	//	printf("scheduler: task is sleeping until: %d now:
      // %d\n",
      //	//	       t->sleep_until, now);
      //	continue;
      // }

      // If we get here we found a ready task. Why don't we use t? Because
      // with this end condition we may always choose T1 even if T1 is
      // sleeping.
      next_task = t;
      break;
    }

    // printf("schedule: switching to task: %x at %x\n", next_task,
    //	   next_task->eip);
    task_current = next_task;
    // We call sti inside switch_task
    // asm volatile("sti");
    switch_task(task_scheduler, task_current);
  }
}

void local_apic_eoi() {
  volatile u32* local_apic_eoi = physical2virtual((void*)0xfee000b0);
  *local_apic_eoi = 0;
}

#define IRQ_TIMER 0x34
#define IRQ_NETWORK 0x33
#define IRQ_MOUSE 0x32
#define IRQ_KEYBOARD 0x31

// regs is passed via rdi
void interrupt_handler(interrupt_registers* regs) {
  // Timer IRQ
  if (regs->int_no == IRQ_TIMER) {
    // This is our schedule timer.
    // printf("interrupt handler\n");

    // Switch to scheduler task.
    // First we have to save the regs
    task_update_context(task_current, regs);
    // Then we load the scheduler task.
    update_regs_from_task(task_scheduler, regs);

    // TODO: Should this timer be set to periodic or re-armed every time?
    set_timer0();

    // TODO: is there a better way to do this?
    // The interrupt handler should do this. Maybe we don't always do this?
    local_apic_eoi();
    return;
  }
  if (regs->int_no == IRQ_NETWORK) {  // network IRQ
    handle_network_interrupt();
    local_apic_eoi();
    return;
  }

  if (regs->int_no == IRQ_MOUSE) {  // mouse IRQ

    // How do we notify the driver? Think about it. Can a device have multiple
    // drivers? Maybe, but I cannot see why, they would probably interfere. So
    // assume 1 driver per device. Every device has an interrupt. The driver
    // can be connected to this interrupt. 1:1 mapping. So here we need to do
    // what? Wake the driver that has a specific interrupt? Where do they run?

    // Good points about why we probably should read data here. The mouse /
    // keyboard has limited buffer size and if we don't read the data inside
    // this handler it will be overridden and we lose data. For mouse position
    // it may not mean much, but for keyboard interrupts we may lose keys. So
    // given there are many different mice. A driver needs some function that
    // is called inside this handler. There's no special data we get from this
    // interrupt. Only that it's 0x32 from the mouse.

    // A lambda task would seem nice here. Bind the values to it and run it
    // deferred. Can I have a task that has parameters?
    // Doing one parameter which is a pointer to a struct is simple.
    // Many.. seems annoying.

    mouse_handle_interrupt();
    local_apic_eoi();
    return;
  }
  if (regs->int_no == IRQ_KEYBOARD) {  // keyboard IRQ
    keyboard_handle_interrupt();
    local_apic_eoi();
    return;
  }

  if (regs->int_no < 15) {
    printf("interrupt: %s\n", interrupt_names[regs->int_no]);
  } else {
    printf("interrupt: %x\n", regs->int_no);
  }
  printf("rflags: %d, ss: %d, cs: %d\n", regs->rflags, regs->ss, regs->cs);
  printf("rsp: %x, ip: %x\n", regs->rsp, regs->eip);

  if (regs->int_no == EXCEPTION_PAGE_FAULT) {
    page_fault_error_t* e = (page_fault_error_t*)&regs->err_code;
    printf(
	"p: %d, wr: %d, us: %d, rsvd: %d, id: %d, pk: %d, ss: %d, hlat: %d\n",
	e->flags.p, e->flags.wr, e->flags.us, e->flags.rsvd, e->flags.id,
	e->flags.pk, e->flags.ss, e->flags.hlat);
    // cr2 contains the address that caused the page fault.
    u64 addr;
    __asm__ volatile("mov %%cr2, %0" : "=r"(addr));
    printf("fault address: %x\n", addr);
  } else {
    printf("error code: %d\n", regs->err_code);
  }

  if (regs->int_no != 3) {
    __asm__ volatile("hlt" : :);
  }
}

void idt_setup() {
  idt.limit = sizeof(idt_entry_t) * 256 - 1;
  //  idt.base = (uint32)&idt_entries;
  idt.base = (u64)&idt_entries;
  // 0x0E = 14 = 64-bit Interrupt Gate
  // but this has p=0 (present bit)
  // 0x8E has p=1 and type=14
  // ref: Table 3-2

  // 0x08 in binary is 0b1000
  // which sets selector to 1
  // ref: Fig 3-6
  // ref: https://wiki.osdev.org/Segment_Selector

  // ref: 3.4.5.1 Code- and Data-Segment Descriptor Types

  // The INT n instruction can be used to emulate exceptions in software; but
  // there is a limitation.1 If INT n provides a vector for one of the
  // architecturally-defined exceptions, the processor generates an interrupt
  // to the correct vector (to access the exception handler) but does not push
  // an error code on the stack. This is true even if the associated
  // hardware-generated exception normally produces an error code. The
  // exception handler will still attempt to pop an error code from the stack
  // while handling the exception. Because no error code was pushed, the
  // handler will pop off and discard the EIP instead (in place of the missing
  // error code). This sends the return to the wrong location.

  // ref: 6.4.2 Software-Generated Exceptions

  // 00077694943d[CPU0  ] LONG MODE IRET
  // 00077694943e[CPU0  ] fetch_raw_descriptor: GDT: index (e67) 1cc > limit
  // (f)
#define INTERRUPT_GATE 0x8E
  idt_set_gate(0, 0, 0x08, 0x0E);
  idt_set_gate(1, 0, 0x08, 0x0E);
  idt_set_gate(2, 0, 0x08, 0x0E);
  idt_set_gate(3, (u64)isr3, 0x08, 0x8E);
  idt_set_gate(4, (u64)isr4, 0x08, 0x8E);
  idt_set_gate(5, 0, 0x08, 0x0E);
  idt_set_gate(6, 0, 0x08, 0x0E);
  idt_set_gate(7, 0, 0x08, 0x0E);
  idt_set_gate(8, (u64)isr8, 0x08, 0x8E);
  idt_set_gate(9, 0, 0x08, 0x0E);
  idt_set_gate(10, 0, 0x08, 0x0E);
  idt_set_gate(11, (u64)isr11, 0x08, 0x0E);
  idt_set_gate(12, (u64)isr12, 0x08, 0x0E);
  idt_set_gate(13, (u64)isr13, 0x08, 0x8E);
  idt_set_gate(14, (u64)isr14, 0x08, 0x8E);
  idt_set_gate(15, 0, 0x08, 0x0E);
  idt_set_gate(16, 0, 0x08, 0x0E);
  idt_set_gate(17, 0, 0x08, 0x0E);
  idt_set_gate(18, 0, 0x08, 0x0E);
  idt_set_gate(19, 0, 0x08, 0x0E);
  idt_set_gate(20, 0, 0x08, 0x0E);
  idt_set_gate(21, 0, 0x08, 0x0E);
  idt_set_gate(22, 0, 0x08, 0x0E);
  idt_set_gate(23, 0, 0x08, 0x0E);
  idt_set_gate(24, 0, 0x08, 0x0E);
  idt_set_gate(25, 0, 0x08, 0x0E);
  idt_set_gate(26, 0, 0x08, 0x0E);
  idt_set_gate(27, 0, 0x08, 0x0E);
  idt_set_gate(28, 0, 0x08, 0x0E);
  idt_set_gate(29, 0, 0x08, 0x0E);
  idt_set_gate(30, 0, 0x08, 0x0E);
  idt_set_gate(31, 0, 0x08, 0x0E);
  idt_set_gate(32, (u64)isr32, 0x08, 0x8E);
  idt_set_gate(0x31, (u64)isr0x31, 0x08, 0x8E);  // keyboard
  idt_set_gate(0x32, (u64)isr0x32, 0x08, 0x8E);  // mouse
  idt_set_gate(0x33, (u64)isr0x33, 0x08, 0x8e);  // ethernet
  idt_set_gate(0x34, (u64)isr0x34, 0x08, 0x8e);  // timer

  idt_update(&idt);
}

void idt_set_gate(u32 interrupt, u64 offset, u16 selector, u8 type_attr) {
  // first 16 bits
  idt_entries[interrupt].offset_start = offset;  //(offset & 0xFFFF);
  // next 16 bits
  idt_entries[interrupt].offset_mid = offset >> 16;  // & 0xFFFF;
  // last 32 bits
  idt_entries[interrupt].offset_end = offset >> 32;  // & 0xFFFFFFFF;

  idt_entries[interrupt].selector = selector;

  idt_entries[interrupt].type_attr = type_attr;

  idt_entries[interrupt].zero = 0;
}

/* void idt_set_gate(uint32 interrupt, uint32 offset, uint16 selector, */
/*                   uint8 type_attr) { */
/*   idt_entries[interrupt].offset_start = (offset & 0xff); */
/*   idt_entries[interrupt].offset_end = (offset >> 16) & 0xff; */

/*   idt_entries[interrupt].selector = selector; */

/*   idt_entries[interrupt].type_attr = type_attr; */
/* } */

/* put this into some bitfield enum? */

#define TYPE_SYSTEM
#define TYPE_CODE_DATA
#define RING0
#define RING3
#define DATA_READ
#define DATA_WRITE
#define DATA_EXPAND_DOWN
#define DATA_ACCESSED
#define CODE_EXECUTE
#define CODE_READ
#define CODE_ACCESSED
#define CODE_CONFORMING

// void gdt_setup() {
//   gdt.limit = sizeof(gdt_entry_t) * 5 - 1;
//   gdt.base = (u32)&gdt_entries;

//   gdt_set_gate(0, 0, 0, 0, 0);
//   gdt_set_gate(1, 0, 0xffffffff, 0x9a, 0xcf);  // kernel mode code segment
//   gdt_set_gate(2, 0, 0xffffffff, 0x92, 0xcf);  // kernel mode data segment
//   gdt_set_gate(3, 0, 0xffffffff, 0xfa, 0xcf);  // user mode code segment
//   gdt_set_gate(4, 0, 0xffffffff, 0xf2, 0xcf);  // user mode data segment
//   gdt_set_gate(5, (u32)&tss, sizeof(tss), 0x89,
//	       0x40);  // cpu1 task switching segment

//   gdt_update(&gdt);
// }

// void gdt_set_gate(u32 entry, u32 base, u32 limit, u8 access, u8 flags) {
//   gdt_entries[entry].base_start = (0xffff & base);
//   gdt_entries[entry].base_middle = (base >> 16) & 0xff;
//   gdt_entries[entry].base_end = (base >> 24) & 0xff;

//   gdt_entries[entry].limit_start = (0xffff & limit);
//   gdt_entries[entry].limit_and_flags = (limit >> 16) & 0x0f;
//   gdt_entries[entry].limit_and_flags |= (flags & 0xf0);

//   gdt_entries[entry].access = access;
// }

// I/O ports
// ref: https://wiki.osdev.org/I/O_ports
#define PIC1 0x20
#define PIC2 0xA0
#define PIC1_COMMAND PIC1
#define PIC1_DATA (PIC1 + 1)
#define PIC2_COMMAND PIC2
#define PIC2_DATA (PIC2 + 1)

#define PIC_END_OF_INTERRUPT 0x20

#define ICW1_ICW4 0x01      /* ICW4 (not) needed */
#define ICW1_SINGLE 0x02    /* Single (cascade) mode */
#define ICW1_INTERVAL4 0x04 /* Call address interval 4 (8) */
#define ICW1_LEVEL 0x08     /* Level triggered (edge) mode */
#define ICW1_INIT 0x10      /* Initialization - required! */

#define ICW4_8086 0x01       /* 8086/88 (MCS-80/85) mode */
#define ICW4_AUTO 0x02       /* Auto (normal) EOI */
#define ICW4_BUF_SLAVE 0x08  /* Buffered mode/slave */
#define ICW4_BUF_MASTER 0x0C /* Buffered mode/master */
#define ICW4_SFNM 0x10       /* Special fully nested (not) */

/*
  arguments:
  offset1 - vector offset for master PIC
  vectors on the master become offset1..offset1+7
  offset2 - same for slave PIC: offset2..offset2+7
 */
void pic_remap(int offset1, int offset2) {
  /* unsigned char a1, a2; */

  /* a1 = inb(PIC1_DATA);  // save masks */
  /* a2 = inb(PIC2_DATA); */

  outb(PIC1_COMMAND,
       ICW1_INIT + ICW1_ICW4);  // starts the initialization sequence
  outb(PIC2_COMMAND, ICW1_INIT + ICW1_ICW4);

  // ICW2
  // Sets the offset into the idt.
  outb(PIC1_DATA, offset1);  // ICW2: define the PIC vectors
  outb(PIC2_DATA, offset2);

  // ICW3
  // define IRQ 2 (0,1,2) to be connected to PIC 2. 4 binary is 100.
  outb(PIC1_DATA, 4);
  // defines that pic 2 is connected to IRQ2 of pic 1.
  // There is a table in the spec that explains it.
  // ref: https://pdos.csail.mit.edu/6.828/2005/readings/hardware/8259A.pdf
  outb(PIC2_DATA, 2);

  outb(PIC1_DATA, ICW4_8086);
  outb(PIC2_DATA, ICW4_8086);

  // Mask (disable) all IRQs
  outb(PIC1_DATA, 0xff);
  outb(PIC2_DATA, 0xff);
}

struct apic_registers {
  u64 reserve_1[4] __attribute__((aligned(16)));
  u32 local_apic_id __attribute__((aligned(16)));
  u64 local_apic_version[2] __attribute__((aligned(16)));
  u64 reserve_2[8] __attribute__((aligned(16)));
  u64 tpr __attribute__((aligned(16)));
  u64 apr __attribute__((aligned(16)));
  u64 prr __attribute__((aligned(16)));
  u64 eoi __attribute__((aligned(16)));
  u64 rrd __attribute__((aligned(16)));
  u64 logical_destination __attribute__((aligned(16)));
  u64 destination_format __attribute__((aligned(16)));
  u64 spurious_interrupt_vector __attribute__((aligned(16)));
  u32 isr_0 __attribute__((aligned(16)));  // in service register ro
  u32 isr_1 __attribute__((aligned(16)));
  u32 isr_2 __attribute__((aligned(16)));
  u32 isr_3 __attribute__((aligned(16)));
  u32 isr_4 __attribute__((aligned(16)));
  u32 isr_5 __attribute__((aligned(16)));
  u32 isr_6 __attribute__((aligned(16)));
  u32 isr_7 __attribute__((aligned(16)));
  u32 tmr_0 __attribute__((aligned(16)));  // trigger mode register ro
  u32 tmr_1 __attribute__((aligned(16)));
  u32 tmr_2 __attribute__((aligned(16)));
  u32 tmr_3 __attribute__((aligned(16)));
  u32 tmr_4 __attribute__((aligned(16)));
  u32 tmr_5 __attribute__((aligned(16)));
  u32 tmr_6 __attribute__((aligned(16)));
  u32 tmr_7 __attribute__((aligned(16)));
  u32 irr_0 __attribute__((aligned(16)));  // interrupt request register ro
  u32 irr_1 __attribute__((aligned(16)));
  u32 irr_2 __attribute__((aligned(16)));
  u32 irr_3 __attribute__((aligned(16)));
  u32 irr_4 __attribute__((aligned(16)));
  u32 irr_5 __attribute__((aligned(16)));
  u32 irr_6 __attribute__((aligned(16)));
  u32 irr_7 __attribute__((aligned(16)));
  u32 error_status __attribute__((aligned(16)));
  u64 reserve_3[12] __attribute__((aligned(16)));
  u32 cmci __attribute__((aligned(16)));
  u32 icr_low __attribute__((aligned(16)));
  u32 icr_high __attribute__((aligned(16)));
  u32 lvt_timer __attribute__((aligned(16)));
  u32 lvt_thermal_sensor __attribute__((aligned(16)));
  u32 lvt_performance_monitoring_counters __attribute__((aligned(16)));
  u32 lvt_lint0 __attribute__((aligned(16)));  // FEE00350
  u32 lvt_lint1 __attribute__((aligned(16)));  // FEE00360
} __attribute__((aligned(16)));
typedef struct apic_registers apic_registers_t;

#define APIC_REGISTER_BASE 0xFEE00000

void apic_setup() {
  apic_registers_t* regs = (apic_registers_t*)APIC_REGISTER_BASE;
  // print as hex?
  printf("apic id addr: %x, value: %d\n", &regs->local_apic_id,
	 regs->local_apic_id >> 24);

  printf("apic tmr0  addr: %x\n", &regs->cmci);
  printf("apic lvt_lint0 addr: %x\n", &regs->lvt_lint0);
  // this just accesses the pointer. have to do [0] but then page fault
  // because maybe not mapped. it's defined as 64 bit but I think it's just
  // 32? aligned 16 worked.

  // TODO
  // set up spurious interrupt
  // remap & disable pic
  // enable apic
  // configure keyboard IRQ on ioapic
}

// TODO: use timer to print clock

u64 rdmsr(u32 reg) {
  u64 value;
  // A = eax + edx
  // c = ecx (the c register)
  // ref:
  // https://gcc.gnu.org/onlinedocs/gcc/extensions-to-the-c-language-family/how-to-use-inline-assembly-language-in-c-code.html#x86-family-config-i386-constraints-md
  __asm__ volatile("rdmsr" : "=A"(value) : "c"(reg));
  return value;
}

// ref: Vol 4 Chapter 2 Table 2-2
#define MSR_APIC_BASE 0x1b

// ref: Vol 4 2.1
typedef union {
  u64 raw;
  struct {
    u64 reserved : 8;
    u64 bsp : 1;
    u64 reserved_2 : 1;
    u64 x2apic : 1;
    u64 apic_global : 1;
    u64 base_address : 52;
  } bits;
} msr_apic_base_t;

struct ioapic_redirection_register {
  union {
    u32 lower;
    struct {
      u32 interrupt_vector : 8;
      u32 delivery_mode : 3;
      u32 destination_mode : 1;
      u32 delivery_status : 1;
      u32 pin_polarity : 1;
      u32 remote_irr : 1;
      u32 trigger_mode : 1;  // 1=level, 0=edge
      u32 interrupt_mask : 1;
      u32 reserved : 16;
    } lower_bits;
  };

  union {
    u32 upper;
    struct {
      u32 reserved : 24;
      u32 destination : 8;
    } upper_bits;
  };
} __attribute__((packed));
typedef struct ioapic_redirection_register ioapic_redirection_register_t;

// Seems Intel and AMD have this at the same address.
// Normally it can be found via the Multiple APIC Description Table (MADT).
// ref: https://wiki.osdev.org/MADT
// Specification:
// The APIC default base memory addresses defined by this specification are
// 0FEC0_0000h and 0FEE0_0000h.
// ref:
// https://web.archive.org/web/20070112195752/http://developer.intel.com/design/pentium/datashts/24201606.pdf
#define IOAPIC_BASE 0xfec00000
#define IOAPIC_IOREGSEL 0xfec00000
#define IOAPIC_IOWIN 0xfec00010

/*
  IOAPIC registers
  00h: IOAPIC ID
  01h: IOAPIC Version
  02h: IOAPIC Arbitration ID
  10-3Fh: Redirection Table (Entries 0-23) (64 bits each)

  How it works:
  1. Write register index as byte to IOREGSEL.
  2. Read data as 32bit value from IOWIN.
 */

u32 ioapic_read_register(u32 reg) {
  u32 volatile* ioregsel =
      (u32 volatile*)physical2virtual((void*)IOAPIC_IOREGSEL);
  ioregsel[0] = reg;
  return ioregsel[4];  // IOAPIC_IOREGSEL+10h (4*4 byte = 16 byte) =
  // IOAPIC_IOWIN
}

void ioapic_write_register(u32 reg, ioapic_redirection_register_t* r) {
  u32 volatile* ioregsel =
      (u32 volatile*)physical2virtual((void*)IOAPIC_IOREGSEL);
  ioregsel[0] = reg;
  ioregsel[4] = r->lower;
  ioregsel[0] = reg + 1;
  ioregsel[4] = r->upper;
}

// keyboard
#define IOAPIC_IRQ1 0x12
// timer
#define IOAPIC_IRQ4 0x18
// ethernet
#define IOAPIC_IRQ11 0x26
// mouse
#define IOAPIC_IRQ12 0x28

void ioapic_setup() {
  // Quote "APIC registers are memory-mapped to a 4-KByte region of the
  // processor’s physical address space with an initial starting address of
  // FEE00000H." ref: vol 3 12.4.1

  // Or
  // 0FEC0_0000h – 0FECF_FFFFh APIC I/O unit
  // 0FEE0_0000h - 0FEEF_FFFFh APIC Local Unit
  // ref:
  // https://web.archive.org/web/20070112195752/http://developer.intel.com/design/pentium/datashts/24201606.pdf

  // Quote "APIC Base field, bits 12 through 35 ⎯ Specifies the base address of
  // the APIC registers. This 24-bit value is extended by 12 bits at the low end
  // to form the base address. This automatically aligns the address on a
  // 4-KByte boundary. Following a power-up or reset, the field is set to FEE0
  // 0000H."
  // ref: vol 3 12.4.4
  msr_apic_base_t apic_base;
  apic_base.raw = rdmsr(MSR_APIC_BASE);
  printf("apic enabled: %d, apic base address: %x\n",
	 apic_base.bits.apic_global, apic_base.bits.base_address << 12);

  printf("ioapic id: %d\n", ioapic_read_register(0));
  printf("ioapic version: %d\n", ioapic_read_register(1) & 0xFF);  // version
  // = first 8 bits.

  // next steps:
  // find apic id
  apic_registers_t* regs =
      (apic_registers_t*)physical2virtual((void*)0xFEE00000);
  printf("apic id: %d\n", regs->local_apic_id >> 24);
  // mask irq 2, this is already masked
  /* ioapic_redirection_register_t r0 = {0}; */
  /* r0.upper_bits.destination = regs->local_apic_id >> 24;  // apic id */
  /* r0.lower_bits.interrupt_mask = 1; */
  /* ioapic_write_register(0x14, &r0); */
  // printf("ioapic irq 0 vector: %d\n", ioapic_read_register(0x10) &
  // 0x000000FF);

  // Keyboard
  //  map irq 1 to user defined interrupt vector
  ioapic_redirection_register_t r = {0};
  r.upper_bits.destination = regs->local_apic_id >> 24;  // apic id
  r.lower_bits.interrupt_vector = 0x31;
  ioapic_write_register(IOAPIC_IRQ1, &r);
  printf("ioapic irq 1 vector: %d\n",
	 ioapic_read_register(IOAPIC_IRQ1) & 0x000000FF);

  //
  // map irq to user defined interrupt vector
  ioapic_redirection_register_t r2 = {0};
  r2.upper_bits.destination = regs->local_apic_id >> 24;  // apic id
  r2.lower_bits.interrupt_vector = 0x32;
  ioapic_write_register(IOAPIC_IRQ12, &r2);
  printf("ioapic irq 12 vector: %d\n",
	 ioapic_read_register(IOAPIC_IRQ12) & 0x000000FF);

  ioapic_redirection_register_t r3 = {0};
  r3.upper_bits.destination = regs->local_apic_id >> 24;  // apic id
  r3.lower_bits.interrupt_vector = 0x33;
  // r3.lower_bits.trigger_mode = 1;
  ioapic_write_register(IOAPIC_IRQ11, &r3);
  printf("ioapic irq 11: %x\n", ioapic_read_register(IOAPIC_IRQ11));

  ioapic_redirection_register_t r4 = {0};
  r4.upper_bits.destination = regs->local_apic_id >> 24;  // apic id
  r4.lower_bits.interrupt_vector = 0x34;
  ioapic_write_register(IOAPIC_IRQ4, &r4);
  printf("ioapic irq 4: %x\n", ioapic_read_register(IOAPIC_IRQ4));

  // Setup ps2 controller, mouse and keyboard.
  // ps2 controller spec:
  // https://web.archive.org/web/20210417040153/http://www.diakom.ru/el/elfirms/datashts/Smsc/42w11.pdf
  // https://www-ug.eecg.utoronto.ca/desl/manuals/ps2.pdf
  // https://www.infineon.com/dgdl/Infineon-PS2D_001-13681-Software+Module+Datasheets-v01_02-EN.pdf?fileId=8ac78c8c7d0d8da4017d0fab8b401c89
  // http://www.mcamafia.de/pdf/ibm_hitrc07.pdf
  // https://www.eecg.utoronto.ca/~jayar/ece241_08F/AudioVideoCores/ps2/ps2.html
  // https://web.archive.org/web/20210417040153/http://www.diakom.ru/el/elfirms/datashts/Smsc/42w11.pdf
  // https://web.archive.org/web/20041213194610/http://www.computer-engineering.org/ps2keyboard/
  // https://web.archive.org/web/20041213193626/http://www.computer-engineering.org/ps2mouse/

  // write command to
  // command port 0x64
  // and then read from
  // data port 0x60

  // how to check if ps2 controller exists
  // check bit1 = 2 in 8042 flag IA PC BOOT ARCHITECTURE FLAGS FADT

  // TODO: this could be an interrupt driven state machine.

#define PS2_STATUS_REGISTER 0x64
#define PS2_COMMAND_REGISTER 0x64
#define PS2_DATA_REGISTER 0x60
#define PS2_COMMAND_READ_CONFIG_BYTE 0x20
#define PS2_COMMAND_WRITE_CONFIG_BYTE 0x60
#define PS2_COMMAND_TEST_CONTROLLER 0xAA
#define PS2_COMMAND_TEST_PORT_1 0xAB
#define PS2_COMMAND_TEST_PORT_2 0xA9
#define PS2_COMMAND_ENABLE_PORT_1 0xAE
#define PS2_COMMAND_DISABLE_PORT_1 0xAD
#define PS2_COMMAND_ENABLE_PORT_2 0xA8
#define PS2_COMMAND_DISABLE_PORT_2 0xA7
#define PS2_COMMAND_NEXT_BYTE_TO_PORT_2 0xD4
#define PS2_DEVICE_COMMAND_RESET 0xFF
#define PS2_DEVICE_COMMAND_ENABLE_DATA_REPORTING 0xF4
#define PS2_DEVICE_COMMAND_DISABLE_DATA_REPORTING 0xF5
#define PS2_DEVICE_COMMAND_IDENTIFY 0xF2
#define PS2_DEVICE_RESPONSE_ACK 0xFA

  u8 status = inb(PS2_STATUS_REGISTER);
  printf("configuring ps2: %x\n", status);

  u8 config;
  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_DISABLE_PORT_1);
  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_DISABLE_PORT_2);

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_READ_CONFIG_BYTE);
  ps2_wait_data();
  config = inb(PS2_DATA_REGISTER);
  printf("config byte 1: %x\n", config);
  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_WRITE_CONFIG_BYTE);
  ps2_wait_ready();
  outb(PS2_DATA_REGISTER, config & ~0x43);
  // disable translation and interrupts, bit 1,2 and 6
  // returns 0x61 = 0b1100001
  // bit 0: first ps2 port interrupt enabled
  // bit 1: second ps2 port interrupt enabled
  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_READ_CONFIG_BYTE);
  ps2_wait_data();
  config = inb(PS2_DATA_REGISTER);
  printf("config byte 2: %x\n", config);

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_TEST_CONTROLLER);
  ps2_wait_data();
  u8 resp = inb(PS2_DATA_REGISTER);
  printf("self test response: %x\n", resp);
  // Must be 0x55

  // Validated. At least qemu does not reset the controller during self-test.
  // The config byte is the same before and after.
  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_READ_CONFIG_BYTE);
  ps2_wait_data();
  config = inb(PS2_DATA_REGISTER);
  printf("config byte 3: %x\n", config);

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_TEST_PORT_1);
  ps2_wait_data();
  u8 response = inb(PS2_DATA_REGISTER);
  printf("test port 1: %x\n", response);
  // Must be 0 otherwise error

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_TEST_PORT_2);
  ps2_wait_data();
  response = inb(PS2_DATA_REGISTER);
  printf("test port 2: %x\n", response);
  // Must be 0 otherwise error

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_ENABLE_PORT_1);

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_READ_CONFIG_BYTE);
  ps2_wait_data();
  config = inb(PS2_DATA_REGISTER);
  printf("config byte 4: %x\n", config);

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_ENABLE_PORT_2);

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_READ_CONFIG_BYTE);
  ps2_wait_data();
  config = inb(PS2_DATA_REGISTER);
  printf("config byte 5: %x\n", config);

  // After reset devices are supposed to send 0xFA 0xAA and the device ID In
  // the case of a ps/2 mouse the device ID is 0x00, but an ancient AT
  // keyboard sends nothing. That's why we read 2 times for the keyboard and 3
  // times for the mouse.
  ps2_wait_ready();
  outb(PS2_DATA_REGISTER, PS2_DEVICE_COMMAND_RESET);
  ps2_wait_data();
  response = inb(PS2_DATA_REGISTER);
  printf("reset response 1: %x\n", response);
  ps2_wait_data();
  response = inb(PS2_DATA_REGISTER);
  printf("reset response 2: %x\n", response);

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_NEXT_BYTE_TO_PORT_2);
  ps2_wait_ready();
  outb(PS2_DATA_REGISTER, PS2_DEVICE_COMMAND_RESET);
  ps2_wait_data();
  response = inb(PS2_DATA_REGISTER);
  printf("reset response 1: %x\n", response);
  ps2_wait_data();
  response = inb(PS2_DATA_REGISTER);
  printf("reset response 2: %x\n", response);
  ps2_wait_data();
  response = inb(PS2_DATA_REGISTER);
  printf("reset response 3: %x\n", response);

  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_NEXT_BYTE_TO_PORT_2);
  ps2_wait_ready();
  outb(PS2_DATA_REGISTER, PS2_DEVICE_COMMAND_ENABLE_DATA_REPORTING);
  ps2_wait_data();
  response = inb(PS2_DATA_REGISTER);
  printf("enable response: %x\n", response);
  // should be 0xFA

  // enable IRQs
  printf("setting config to: %x\n", config | 0x3);
  ps2_wait_ready();
  outb(PS2_COMMAND_REGISTER, PS2_COMMAND_WRITE_CONFIG_BYTE);
  ps2_wait_ready();
  outb(PS2_DATA_REGISTER, config | 0x3);  // enable interrupts. bits 1,2

  // Bug: here the problems start. After enabling interrupts we somehow
  // receive the response to PS2_COMMAND_READ_CONFIG_BYTE via keyboard
  // interrupt.
  //
  // Output:
  // scancode: 0x3
  // config byte 6: 0x3
  //
  // This also means reading from the buffer does not clear it.
  //
  // The answer is on the wiki already...
  // Quote
  //
  // Unfortunately, there is one problem to worry about. If you send a command
  // to the PS/2 controller that involves a response, the PS/2 controller may
  // generate IRQ1, IRQ12, or no IRQ (depending on the firmware) when it puts
  // the "response byte" into the buffer. In all three cases, you can't tell
  // if the byte came from a PS/2 device or the PS/2 controller. In the no IRQ
  // case, you additionally will need to poll for the byte. Fortunately, you
  // should never need to send a command to the PS/2 controller itself after
  // initialisation (and you can disable IRQs and both PS/2 devices where
  // necessary during initialisation).
  /* ps2_wait_ready(); */
  /* outb(0x64, PS2_COMMAND_READ_CONFIG_BYTE);  // 0x20 = read config byte */
  /* // ps2_wait_data();                           // this hangs with qemu */
  /* config = inb(0x60); */
  /* printf("config byte 6: %x", config); */

  // Note: consider that there is a bad parity bit. In this case resending is
  // necessary.

  // returns: 0x41 = 0b1000001
  // we can find irq that are remapped in the ioapic from the default.
  // https://uefi.org/specs/ACPI/6.5/05_ACPI_Software_Programming_Model.html#interrupt-source-override-structure
}

// sdt: system description table
struct acpi_sdt_header {
  u8 signature[4];
  u32 length;
  u8 revision;
  u8 checksum;
  u8 oem_id[6];
  u8 oem_table_id[8];
  u32 oem_revision;
  u32 creator_id;
  u32 creator_revision;
} __attribute__((packed));
typedef struct acpi_sdt_header acpi_sdt_header;

struct acpi_rsdt {
  acpi_sdt_header h;
  u32 entries[];  // h.length - sizeof(h)
} __attribute__((packed));
typedef struct acpi_rsdt acpi_rsdt;

struct acpi_rsdp {
  u8 signature[8];
  u8 checksum;
  u8 oem_id[6];
  u8 revision;
  u32 rsdt;
} __attribute__((packed));
typedef struct acpi_rsdp acpi_rsdp_t;

acpi_rsdp_t* locate_rsdp() {
  // ref: https://wiki.osdev.org/RSDP
  // ref:
  // https://uefi.org/specs/ACPI/6.5/05_ACPI_Software_Programming_Model.html#finding-the-rsdp-on-ia-pc-systems
  u64* start = (u64*)physical2virtual((void*)0xE0000);
  u64* end = (u64*)physical2virtual((void*)0xFFFFF);

  printf("locate_rsdp: from=%x to=%x\n", start, end);

  // "RSD PTR "
  for (; start < end; start += 2) {
    u8* s = (u8*)start;
    if (s[0] == 'R' && s[1] == 'S' && s[2] == 'D' && s[3] == ' ' &&
	s[4] == 'P' && s[5] == 'T' && s[6] == 'R' && s[7] == ' ') {
      printf("locate_rsdp: found at %x\n", start);
      return (acpi_rsdp_t*)start;
    }
  }
  printf("locate_rsdp: not found\n");
  return nullptr;
}

// ics = interrupt controller structure
struct acpi_ics_header {
  u8 type;
  u8 length;
} __attribute__((packed));
typedef struct acpi_ics_header acpi_ics_header;

struct acpi_madt {
  acpi_sdt_header header;
  u32 local_interrupt_controller_address;
  u32 flags;
  acpi_ics_header interrupt_controller_structure[];
} __attribute__((packed));
typedef struct acpi_madt acpi_madt;

struct acpi_ics_ioapic {
  acpi_ics_header header;
  u8 id;
  u8 reserved;
  u32 address;
  u32 global_system_interrupt_base;
} __attribute__((packed));
typedef struct acpi_ics_ioapic acpi_ics_ioapic_t;

struct acpi_ics_input_source_override {
  acpi_ics_header header;
  u8 bus;
  u8 source;
  u32 global_system_interrupt;
  u16 flags;
} __attribute__((packed));
typedef struct acpi_ics_input_source_override acpi_ics_input_source_override_t;

struct acpi_generic_address_structure {
  u8 address_space_id;
  u8 register_bit_width;
  u8 register_bit_offset;
  u8 reserved;
  u64 address;
} __attribute__((packed));
typedef struct acpi_generic_address_structure acpi_generic_address_structure_t;

struct acpi_hpet_header {
  acpi_sdt_header header;
  u32 event_timer_block_id;
  acpi_generic_address_structure_t base_address;
  u8 hpet_number;
  u16 main_counter_minimum_clock_tick;
  u8 page_attribution;
} __attribute__((packed));
typedef struct acpi_hpet_header acpi_hpet_header_t;

// note: take care when taking references of a pointer.
void list_tables(acpi_rsdt* rsdt) {
  printf("list_tables: rsdt=%x\n", rsdt);
  int count = (rsdt->h.length - sizeof(rsdt->h)) / sizeof(u32);
  printf("rsdt: entry count: %d\n", count);
  //  acpi_sdt_header_t* h = physical2virtual((void*)rsdt->entries);
  // printf("rsdt: entries physical=%x virtual=%x\n", rsdt->entries, h);
  for (int i = 0; i < count; i++) {
    // &entries to get the first entry.
    // +i uses size of type which is u32.
    // * because it's a pointer to some place.

    // acpi_sdt_header_t* h = (acpi_sdt_header_t*)(*(&rsdt->entries + i));
    printf("rsdt: entry=%x\n", rsdt->entries[i]);
    acpi_sdt_header* e = physical2virtual((void*)rsdt->entries[i]);
    printf("table %d: %.*s\n", i, 4, e->signature);
    if (strncmp(e->signature, "APIC", 4)) {
      acpi_madt* madt = (acpi_madt*)e;
      int count = (madt->header.length - sizeof(madt->header)) / sizeof(u32);
      printf("madt: interrupt_controller_address=%x entries=%d\n",
	     madt->local_interrupt_controller_address, count);
      for (u32 i = 0; i < count; i++) {
	acpi_ics_header* h = &madt->interrupt_controller_structure[i];
	printf("type: %d, length: %d\n", h->type, h->length);
	if (h->type == 1) {
	  acpi_ics_ioapic_t* ioapic = (acpi_ics_ioapic_t*)h;
	  printf("ioapic: id: %d address: %x\n", ioapic->id, ioapic->address);
	  // 0x000000e3, e3 implies dirty flag is set
	  // 0x00200083, this is a normal page with dirty flag not set
	  // 0xFEC00000 is beyond 4GB.

	  // Our page tables have the first 512 2mb
	  // To reach 0xfec00000 we need to map the 2038th 2mb range.
	  // Skip number 2 and 3 and set up page 502.
	}
	if (h->type == 2) {
	  printf("interrupt source overrides\n");
	  acpi_ics_input_source_override_t* iso =
	      (acpi_ics_input_source_override_t*)h;
	  printf("source: %x, interrupt: %x\n", iso->source,
		 iso->global_system_interrupt);
	}
      }
    } else if (strncmp(e->signature, "HPET", 4)) {
      acpi_hpet_header_t* hpet = (acpi_hpet_header_t*)e;
      printf("HPET: hpet number: %d\n", hpet->hpet_number);
      printf("HPET: address space: %d base: %x\n",
	     hpet->base_address.address_space_id, hpet->base_address.address);
    }
  }
}

struct pcmp_processor_entry {
  u8 type;  // 0
  u8 local_apic_id;
  u8 local_apic_version;
  u8 cpu_flags;
  u32 signature;
  u32 feature_flags;
  u64 reserved;
} __attribute__((packed));
typedef struct pcmp_processor_entry pcmp_processor_entry_t;

// TODO: since we use sizeof later during parsing we need to make sure this
// actually is the right size.
struct pcmp_bus_entry {
  u32 type : 8;  // 1
  u32 bus_id : 8;
  u64 bus_type : 48;
} __attribute__((packed));
typedef struct pcmp_bus_entry pcmp_bus_entry_t;

struct pcmp_ioapic_entry {
  u8 type;  // 2
  u8 apic_io;
  u8 apic_version;
  u8 apic_flags;
  u32 address;
} __attribute__((packed));
typedef struct pcmp_ioapic_entry pcmp_ioapic_entry_t;

struct pcmp_interrupt_entry {
  u8 type;  // 3
  u8 interrupt_type;
  u16 interrupt_flags;
  u8 source_bus_id;
  struct {
    u8 signal_type : 2;
    u8 pci_device_number : 5;
    u8 reserved : 1;
  } source_bus_irq;
  u8 destination_apic_id;
  u8 destination_apic_int;
} __attribute__((packed));
typedef struct pcmp_interrupt_entry pcmp_interrupt_entry_t;

void locate_pcmp() {
  // ref:
  // https://web.archive.org/web/20121002210153/http://download.intel.com/design/archives/processors/pro/docs/24201606.pdf
  u8* start = (u8*)0xE0000;
  u8* end = (u8*)0xFFFFF;

  // "RSD PTR "
  for (; start < end; start += 1) {
    u8* s = start;
    if (s[0] == 'P' && s[1] == 'C' && s[2] == 'M' && s[3] == 'P') {
      // find PCI interrupt on I/O apic
      u16 count = *(u16*)(start + 34);
      start += 44;  // skip header
      printf("first entry: %x count: %d\n", start, count);
      s = start;
      for (int i = 0; i < count; i++) {
	if (*s == 0) {
	  //	  printf("cpu header\n");
	  s += sizeof(pcmp_processor_entry_t);
	} else if (*s == 1) {
	  pcmp_bus_entry_t* b = (pcmp_bus_entry_t*)s;
	  //	  printf("bus header: %d\n", b->bus_id);
	  s += sizeof(pcmp_bus_entry_t);
	} else if (*s == 2) {
	  //	  printf("io apic header\n");
	  s += sizeof(pcmp_ioapic_entry_t);
	} else if (*s == 3) {
	  //	  printf("interrupt header\n");
	  pcmp_interrupt_entry_t* e = (pcmp_interrupt_entry_t*)s;
	  printf(
	      "type: %x, flags: %x, bus: %x, "
	      "irq.t: %x, irq.d: %x, id: %x, "
	      "int: %x\n",
	      e->interrupt_type, e->interrupt_flags, e->source_bus_id,
	      e->source_bus_irq.signal_type,
	      e->source_bus_irq.pci_device_number, e->destination_apic_id,
	      e->destination_apic_int);
	  s += sizeof(pcmp_interrupt_entry_t);
	} else if (*s == 4) {
	  // printf("interrupt 2 header\n");
	  pcmp_interrupt_entry_t* e = (pcmp_interrupt_entry_t*)s;
	  /* printf( */
	  /*     "interrupt_type: %x, interrupt_flags: %x, source_bus_id: %x,
	   * "
	   */
	  /*     "source_bus_irq: %x, destination_apic_id: %x, " */
	  /*     "destination_apic_int %x\n", */
	  /*     e->interrupt_type, e->interrupt_flags, e->source_bus_id, */
	  /*     e->source_bus_irq, e->destination_apic_id, */
	  /*     e->destination_apic_int); */
	  s += sizeof(pcmp_interrupt_entry_t);
	} else {
	  printf("other %d\n", *s);
	}
      }

      return;
    }
  }
  return;
}

struct multiboot2_information {
  u32 total_size;
  u32 reserved;
} __attribute__((packed));
typedef struct multiboot2_information multiboot2_information_t;

struct multiboot2_tag_header {
  u32 type;
  u32 size;
} __attribute__((packed));
typedef struct multiboot2_tag_header multiboot2_tag_header_t;

struct multiboot2_tag_memory_map_header {
  u32 type;
  u32 size;
  u32 entry_size;
  u32 entry_version;
} __attribute__((packed));
typedef struct multiboot2_tag_memory_map_header
    multiboot2_tag_memory_map_header_t;

struct multiboot2_tag_memory_map_entry {
  u64 base_addr;
  u64 length;
  u32 type;
  u32 reserved;
} __attribute__((packed));
typedef struct multiboot2_tag_memory_map_entry
    multiboot2_tag_memory_map_entry_t;

#define MULTIBOOT2_TAG_END 0
#define MULTIBOOT2_TAG_MEMORY_MAP 6

extern void pages_init_wrapper();
extern void syscall_wrapper();
extern void enable_syscalls(void*);

void syscall_handler(u8 syscall) {
  printf("syscall_handler: syscall=%d\n", syscall);
  switch (syscall) {
    case 1:
      task_mark_finished(task_current);
      break;
  }
}

int kmain(multiboot2_information_t* mbd, u32 magic) {
  // TODO:
  // paging should be one of the first things to be configured, but errors will
  // be silent if we don't set upt interrupts.
  printf("kernel start=%x end=%x size=%x\n", &_kernel_start, &_kernel_end,
	 (u64)&_kernel_end - (u64)&_kernel_start);

  if (magic != 0x36d76289) {
    printf("multiboot error: %x\n", magic);
    __asm__ volatile("hlt");
  }

  serial_init();

  // These live on the stack
  //
  // ffffffff80001efe: mov    QWORD PTR [rbp-0x8],0x0
  // ffffffff80001f06: mov    QWORD PTR [rbp-0x10],0x0
  u64 memory_size = 0;
  u64 first = 0;
  // We need to store all available memory somewhere. the below gives usable
  // memory, we have to subtract the kernel size from it. The kernel currently
  // is identity mapped with 2mb pages.
  multiboot2_tag_header_t* h =
      (multiboot2_tag_header_t*)((uintptr_t)mbd +
				 sizeof(multiboot2_information_t));

  while (h->type != MULTIBOOT2_TAG_END) {
    //   printf("header type: %x size: %x\n", h->type, h->size);
    if (h->type == MULTIBOOT2_TAG_MEMORY_MAP) {
      multiboot2_tag_memory_map_header_t* mh =
	  (multiboot2_tag_memory_map_header_t*)h;
      u32 num_entries = mh->size / mh->entry_size;
      //   printf("memory map: entries = %d\n", num_entries);
      multiboot2_tag_memory_map_entry_t* e =
	  (multiboot2_tag_memory_map_entry_t*)((uintptr_t)h +
					       sizeof(
						   multiboot2_tag_memory_map_header_t));
      for (u8 i = 0; i < num_entries; i++) {
	e += i;

	if (i == 0) {
	  first = e->base_addr;
	}

	// type
	// 1: available RAM
	// 3: usable memory containing ACPI information
	// 4: reserved memory (needs to be preserved during hibernation)
	// 5: bad RAM
	// others: reserved area
	printf("entry %d: type = %d\n", i, e->type);
	printf("base = %x length = %x\n", e->base_addr, e->length);
	if (e->type == 1) {
	  memory_init(e->base_addr, e->length);
	  memory_size += e->length;
	}
      }
    }
    // Tags are 8-bytes aligned.
    // ref:
    // https://www.gnu.org/software/grub/manual/multiboot2/multiboot.html#Boot-information-format-1
    h = (multiboot2_tag_header_t*)((uintptr_t)h + ((h->size + 7) & ~7));
  }

  printf("total memory: %x start addr %x\n", memory_size, first);

  // TODO: could add some inline tests here if I know available memory or
  // something.

  // I created this wrapper to fix up the rbp. This should be solved in a nicer
  // way but I wanted to get it done and move on for now.
  // Possible follow ups
  // - set up paging before calling kmain
  // - reset stack and tail call kmain
  pages_init_wrapper();

  // Here we are order dependent. Only allow malloc after the pages are
  // configured, otherwise the memory malloc allocated before may become
  // inaccessible.
  // Should give chopped log
  u8* c = malloc(1);
  u8* c2 = malloc(1);
  u8* c3 = malloc(1);
  free(c);
  // Should give perfect size log
  u8* c4 = malloc(1);
  u8* c5 = malloc(1);
  free(c2);

  free(c3);
  free(c4);
  free(c5);

  // set pit 0 to one shot mode
  // bit 4-5 = access mode
  // bit 2-3 = mode
  // ref: https://www.diamondsystems.com/files/binaries/har82c54.pdf
  outb(0x43, 0b110010);

  /* gdt_setup(); */
  pic_remap(0x20, 0x28);
  idt_setup();

  cls();
  char test[10];
  itoa(123, test, 10);
  print_string(test);

  itoa(-123, test, 10);
  print_string(test);

  printf(
      "\nstring: %s\nchar: %c\npositive integer: %d\nnegative integer: % d\n",
      "test", 'c', 123, -123);

  print_string(
      "hello world\nneue Zeile\nnoch eine neue Zeile\nscheint zu "
      "gehen\n\n\n\n4 neue zeilen");

  printf("some hex: 3=%x 15=%x 16=%x 27=%x 26=%x 32=%x\n", 3, 15, 16, 17, 26,
	 32);

  // I validated that this prints IP at nop after int3
  __asm__ volatile("int $0x3");

  acpi_rsdp_t* rsdp = locate_rsdp();
  //  if (rsdp == nullptr) {
  //    // panic
  //  }
  //  printf("rsdp: revision: %d, rsdt_addr: %x\n", rsdp->revision,
  //  rsdp->rsdt);
  acpi_rsdt* rsdt = (acpi_rsdt*)physical2virtual((void*)rsdp->rsdt);
  //  printf("rsdt: %.*s", 4, rsdt->signature);
  list_tables(rsdt);

  // TODO:
  // map FEC address range with info from the tables dynamically.
  pages_map_contiguous(nullptr, (u64)physical2virtual((void*)0xFEE00000),
		       0xFEE00000, 0xFEEFFFFF);
  pages_map_contiguous(nullptr, (u64)physical2virtual((void*)0xFEC00000),
		       0xFEC00000, 0xFECFFFFF);

  // apic_setup();
  ioapic_setup();

  printf("configure ethernet device\n");

  // Initialize network card
  pci_enumerate();

  // locate_pcmp();  // This told us that bus 0 device X (ethernet) is mapped
  // to.
  //  TODO: check delivery mode. IRQ 11 (0xB).

  // syscalls
  // vol 3 6.8.8
  // syscall and sysret

  // timer
  // ref:
  // http://www.intel.com/content/dam/www/public/us/en/documents/technical-specifications/software-developers-hpet-spec-1-0a.pdf
  // 0xfed00000 HPET
  // how to configure:
  // 1. set timer type one shot / periodic
  // 2. set interrupt enable
  //  edge seems easier, level needs to be acknowledged like PCI.
  // 3. set comparator match
  // 4. set overall enable bit

  // What to do next?
  //
  // 1. DONE reschedule immediately after sleep/clean up?
  // 2. DONE extract ps2/keyboard driver
  // 2.1. DONE we will need some way to communicate new data to the driver task
  //
  // DONE
  // Make message_receive time out.
  // Encapsulate queues in a struct.
  // Paging, virtual memory for malloc.
  //
  // Issues:
  // Lots of addresses hardcoded, HPET for example.
  // Read everything from the ACPI tables.
  // Improve network transmit code, i.e. interrupt based, solve race.
  //
  // Next:
  // Load some user space process?
  //
  // Somehow register for specific messages.
  // 3. extract more to separate files
  // 4. tasks/processes that have their own address space
  // 5. keyboard commands?
  // 6. two displays? kernel log and keyboard command console?
  // 7. window system?
  // 8. enable more cpus

  // What kind of drivers are there?
  // Timer driver?
  // ioapic driver?
  // PCI bus driver to scan for devices?
  // Are the above drivers?
  // PS2 driver
  // Mouse driver
  // Keyboard driver
  // Network driver
  // Sound driver
  // Disk

  // Testing the message_ functions
  message_queue test_queue = {};
  test_queue.head = malloc(sizeof(test_queue.head));
  *test_queue.head = nullptr;
  u8* c6 = malloc(1);
  *c6 = 234;
  message_send(&test_queue, message_type_ps2_byte, c6);
  if (*test_queue.head == nullptr) {
    return -1;
  }
  if (message_peek(&test_queue) == false) {
    return -1;
  }
  message test_message;
  message_receive(&test_queue, &test_message);
  u8* c7 = test_message.data;
  if (*c7 != 234) {
    return -1;
  }

  if (*test_queue.head != nullptr) {
    return -1;
  }
  free(c6);
  free(test_queue.head);

  // task_scheduler = 0x2
  // task_idle = 0x6
  // service_mouse = 0xA
  // t1 = 0xE
  // t2 = 0x12
  // TODO: also mentioned above, we probably don't want to rely on having
  // pointers to service_mouse and service_keyboard, etc.
  task_current = task_scheduler = task_new_kernel((u64)schedule);
  task_idle = task_new_kernel((u64)idle_task);
  // service_mouse = task_new_malloc((u64)mouse_service);
  // service_keyboard = task_new_malloc((u64)keyboard_service);
  // service_network = task_new_malloc((u64)network_service);
  // service_dhcp = task_new_malloc((u64)dhcp_service);
  // service_dns = task_new_malloc((u64)dns_service);
  // task_new_malloc((u64)task_network);
  // task_new_malloc((u64)task1);
  // task_new_malloc((u64)task2);

  // How do we start the schedule task here? Call 'switch_task'? We will lose
  // the current stack. Can we reclaim it, or we don't care because it's so
  // small? I guess I could at least preserve it? Or the schedule task could
  // actually use it and stay the 'kernel' task, then we would just call
  // schedule here.
  // printf("switching to scheduler task\n");
  // setup_hpet();

  // task_replace(task_scheduler);

  // Next would be to load an elf from disk and load it into memory as a
  // process. We can find a file on FAT32
  fat32_context* fat32_ctx = fat32_initialize();
  // Better if this would return a file that has size and buffer
  fat32_buffer* buffer = fat32_read_file(fat32_ctx, "TEST_APP");

  printf("fat32: data=%x\n", buffer->data);

  elf_header* eh = buffer->data;
  if (eh->class != 2) {
    printf("elf: only 64 bit supported\n");
    __asm__("hlt");
  }

  // TODO: make a function that does the conversion already
  memory* m = memory_remove();
  u8* p = (u8*)physical2virtual((void*)m->address);
  u64 base_page = 0x600000;
  elf_program_header* ph = buffer->data + eh->program_header_offset;
  for (u32 i = 0; i < eh->program_header_entries; i++) {
    printf("elf: type=%x\n", ph[i].type);
    if (ph[i].type == ELF_SEGMENT_TYPE_LOAD) {
      printf("elf: offset=%x file_size=%x\n", ph[i].offset, ph[i].file_size);
      u64 offset = ph[i].vaddr - base_page;
      memcpy(&buffer->data[ph[i].offset], &p[offset], ph[i].file_size);
    }
  }

  pml4_entry* table = pages_new_table();
  // TODO: get the 0x60000... dynamically
  pages_map_contiguous(table, 0x600000, (u64)virtual2physical(p),
		       (u64)virtual2physical(p + buffer->size));

  task* user = task_new_user(table, eh->entry);
  printf("kmain: enable_syscalls\n");
  enable_syscalls(syscall_wrapper);
  task dummy;
  printf("kmain: switch_task\n");
  switch_task(&dummy, user);

  // How to make devices available.
  // Currently everything is global.
  // E.g. pci_enumerate finds devices, knows exactly which initialize function
  // to call, rtl8139_initialize, ata_initialize, etc.
  // Then we use those as globals. Very brittle.
  // We want to make those available as devices somehow.
  // There should be a place to get them.
  // "Device manager" could use several methods to find devices, PCI is maybe
  // just one of them, some things are only available through ports
  // (keyboard,mouse,...)
  // Assume device manager would use pci_enumerate to find devices. It could
  // then call the right drivers, they could be registered somehow to make it
  // extendable, and call an interface function on them.
  // I delay this until it becomes necessary.

  // About FS abstraction
  // I delay this until I have more than 1 FS.
  //
  // fs_read_file("README.md")
  // fs_read_file would then use the underlying format like fat32/ntfs to look
  // for the file. They need to implement some interface of FS to be
  // exchangable.
  // we don't know where to read this from, do
  // fs_initialize(disk?) before?

  return 0xDEADBABA;
}

/* // Can we link 32 and 64 bit code into the same binary? */
/* //
 * https://stackoverflow.com/questions/49473061/linking-32-and-64-bit-code-together-into-a-single-binary
 */

// Bochs: CTRL-F for "long mode activated"

// networking
// # Connect e1000-82540em - Intel Gigabit Ethernet to VM
// qemu-system-xxx -netdev vmnet-bridged,id=vmnet,ifname=en0 -device
// e1000-82540em,netdev=vmnet
// sudo qemu-system-x86_64 -netdev vmnet-bridged,id=vmnet,ifname=en0 -device
// rtl8139,netdev=vmnet

// how to get a hex dump of the binary
// x86_64-elf-objdump -M intel -d kernel.bin -x | less

/*
  x86 calling conventions
  Integer/pointer arguments 1-6: rdi, rsi, rdx, rcx, r8, r9
  Floating point arguments 1-8: xmm0-xmm7
  Rest: Stack
  Static chain pointer: r10 (no idea what this is)
  Stack cleanup: caller
*/

// qemu monitor
// https://en.wikibooks.org/wiki/QEMU/Monitor
// https://qemu-project.gitlab.io/qemu/system/monitor.html
// useful to see lapic status etc
// commands
// info lapic

// gdb to lldb
// https://lldb.llvm.org/use/map.html

// Linus on typedefs
// https://yarchive.net/comp/linux/typedefs.html
// https://yarchive.net/comp/linux/bitfields.html

// When I tried to change the linker script to move part of the kernel further
// up (VMA) I got "relocation truncated to fit: R_X86_64_32S against symbol".
// See relocations:
// x86_64-elf-readelf --relocs kernel.o
//
// This solves it GCC option
// -mcmodel=large
// Generate code for the large model. This model makes no assumptions about
// addresses and sizes of sections.
//
// Turns out mdmodel=large can be really slow because jump instructions are all
// indirect and so they turn into two instructions. One to load the address and
// one to jump. Because the compiler cannot constrain how far we jump. The
// compiler has a kernel mode which we use -mcmodel=kernel. This is for the
// linux kernel which sits 2gb from the top of the memory range. Then we know we
// can reach everything with 32bit RIP relative jumps.

// in qemu console
// print page table if they work
// info mem
// for debugging
// print page table (exchange address)
// memory read -s8 -fu -c512 0x000600 --force
// s1 is 1 byte
// -f is format
// -c is count

// brew install mtools
//
// # Create a 2 MB file
// dd if=/dev/zero of=disk.img bs=1M count=2
//
// # Put a FAT filesystem on it (use -F for FAT32, otherwise it's automatic)
// mformat -F -i disk.img ::
//
// # Add a file to it
// mcopy -i disk.img example.txt ::
//
// # List files
// mdir -i disk.img ::
//
// # Extract a file
// mcopy -i disk.img ::/example.txt extracted.txt
//
// ref: https://unix.stackexchange.com/a/629494
