#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define va_start(v, l) __builtin_va_start(v, l)
#define va_arg(v, l) __builtin_va_arg(v, l)
#define va_end(v) __builtin_va_end(v)
#define va_copy(d, s) __builtin_va_copy(d, s)
typedef __builtin_va_list va_list;

char* reverse(char* buffer, unsigned int length) {
  char* start = buffer;
  char* end = buffer + length - 1;
  while (start < end) {
    char tmp = *start;
    *start++ = *end;
    *end-- = tmp;
  }
  return buffer;
}

char digit(int value) {
  // Hack: jumps over ascii values between 9 and A to display hex.
  if (value > 9) {
    return '0' + 7 + value;
  } else {
    return '0' + value;
  }
}

char* itoa(int value, char* buffer, unsigned int base) {
  char* s = buffer;
  // This seems too specific to base 10?
  // Answer from stdlib:
  // If base is 10 and value is negative, the resulting string is preceded with
  // a minus sign (-). With any other base, value is always considered unsigned.
  if (base == 10 && value < 0) {
    *s++ = '-';
    ++buffer;
    value = -value;
  }
  while (value >= base) {
    int remainder = value % base;
    value /= base;
    *s++ = digit(remainder);
  }
  *s++ = digit(value);
  if (base == 16) {
    *s++ = 'x';
    *s++ = '0';
  }
  *s = 0;
  reverse(buffer, s - buffer);
  return buffer;
}

// Do we have a memory problem?

/*
  without terminal buffer
  000000000010d0e0 g     O .bss   0000000000002010 network_rx_buffer

  with terminal buffer
  000000000041a500 g     O .bss   0000000000002010 network_rx_buffer
  000000000010c020 g     O .bss   000000000030d400 terminal_buffer
 */

// ref: https://en.wikipedia.org/wiki/VGA_text_mode#Access_methods
unsigned char* videoram = (unsigned char*)0xb8000;

int xpos = 0;
int ypos = 0;

// How can we improve this?
// Have a ring buffer that holds N pages 25*80
// How do we write to it? Printf should do what?
// Just write to that buffer instead of the video ram.
// Write that buffer to video ram, could be optimized, but not needed, yet.
// Have a pointer/"cursor" that says which line is the top/bottom line on the
// screen. Can control the pointer with arrow keys to scroll up and down.

uint8_t* terminal_buffer[80 * 25 * 2 * 100] = {0};  // 100 pages

// ring buffer start. once we are at the end it needs to wrap. should be aligned
// to start of line I guess.
uint16_t terminal_buffer_start = 0;
uint16_t terminal_buffer_row_index = 0;
uint16_t terminal_buffer_column_index = 0;
uint16_t terminal_cursor_index = 0;

// TODO: once we wrap there will be old data in the buffer. we need to clear it.
// E.g. at the end of the line there is noise.
void display() {
  for (int y = 0; y < 25; y++) {
    for (int x = 0; x < 80; x++) {
      // contract: at minimum we want to start at terminal_buffer_start default
      // is cursor locked at last line so we want to subtract 25 lines, but only
      // if we are over 25.
      bool is_after_first_page = terminal_cursor_index > 25;
      int adjusted_cursor_index =
	  (terminal_cursor_index - is_after_first_page * 25);
      int terminal_buffer_offset =
	  terminal_buffer_start + adjusted_cursor_index * 80 * 2;
      int idx = y * 80 * 2 + x * 2;
      videoram[idx] = terminal_buffer[terminal_buffer_offset + idx];
      videoram[idx + 1] = terminal_buffer[terminal_buffer_offset + idx + 1];
    }
  }
}

void new_line() {
  // reset to first column
  terminal_buffer_column_index = 0;
  // one line down
  ++terminal_buffer_row_index;
  // lock cursor at last line
  terminal_cursor_index = terminal_buffer_row_index;
  /* // super hacky */
  /* if (terminal_buffer_row_index % 25 == 0) { */
  /*   // cls(); */
  /*   // automatically move cursor */
  /*   terminal_buffer_row_index++; */
  /* } */
}

void print_character_color(char c, char color) {
  switch (c) {
    case '\n':
      new_line();
      break;
    default:
      /* if (xpos > 80 * 2) { */
      /*	new_line(); */
      /* } */
      int idx =
	  terminal_buffer_row_index * 80 * 2 + terminal_buffer_column_index * 2;
      terminal_buffer[idx] = (int)c;
      terminal_buffer[idx + 1] = color;
      terminal_buffer_column_index++;
  }
}

/* void new_line() { */
/*   xpos = 0; */
/*   ++ypos; */
/*   if (ypos > 25) { */
/*     ypos = 0; */
/*   } */
/* } */

/* void print_character_color(char c, char color) { */
/*   switch (c) { */
/*     case '\n': */
/*       new_line(); */
/*       break; */
/*     default: */
/*       if (xpos > 80 * 2) { */
/*	new_line(); */
/*       } */
/*       videoram[ypos * 80 * 2 + xpos * 2] = (int)c; */
/*       videoram[ypos * 80 * 2 + xpos * 2 + 1] = color; */
/*       xpos++; */
/*   } */
/* } */

void print_character(char c) {
  print_character_color(c, 0x07);
}

void print_string_n(char* s, int limit) {
  int i = 0;
  while (*s != 0) {
    print_character(*s);
    s++;
    i++;
    if (i == limit) {
      return;
    }
  }
}

void print_string(char* s) {
  print_string_n(s, 0);
}

void print_integer(int d) {
  char number_buffer[11];
  itoa(d, number_buffer, 10);
  print_string(number_buffer);
}

void print_hex(int d) {
  char number_buffer[11];
  itoa(d, number_buffer, 16);
  print_string(number_buffer);
}

// memset with vectorized implementation can be faster
void cls() {
  int i = 0;
  for (i = 0; i < 80 * 25 * 2; ++i) {
    videoram[i] = 0;
  }
  xpos = 0;
  ypos = 0;
}

void cll(int line) {
  for (int i = 0; i < 80 * 2; i++) {
    videoram[line * 80 * 2 + i] = 0;
  }
}

void print_warning(char* s) {
  print_string("Warning: ");
  print_string(s);
}

void print_error(char* s) {
  print_string("Error: ");
  print_string(s);
}

int printf(const char* format, ...) {
  va_list args;
  int d;
  char c;
  char* s;
  va_start(args, format);
  while (*format != 0) {
    char c = *format;
    if (c == '%') {
      c = *++format;
      // hacky ad hoc modifier parsing.
      int length = 0;
      switch (c) {
	case '.':
	  c = *++format;
	  switch (c) {
	    case '*':
	      length = va_arg(args, int);
	      break;
	    default:
	      // panic()
	      break;
	  }
	  c = *++format;
	  break;
      }
      switch (c) {
	case 'd':
	  d = va_arg(args, int);
	  print_integer(d);
	  break;
	case 'c':
	  c = va_arg(args, int);
	  print_character(c);
	  break;
	case 's':
	  s = va_arg(args, char*);
	  print_string_n(s, length);
	  break;
	case 'x':
	  d = va_arg(args, int);
	  print_hex(d);
	  break;
      }
    } else {
      print_character(c);
    }
    ++format;
  }
  va_end(args);
  display();
}

/* available colors

   0:black, 1:blue, 2:green, 3:cyan, 4:red,
   5:magenta, 6:brown, 7:light grey, 8:dark grey,
   9:light blue, 10:light green, 11:light cyan,
   12:light red, 13:light magneta, 14: light brown, 15: white
*/

typedef uint64_t uint64;
typedef int64_t int64;
// typedef unsigned int uint32;
typedef uint32_t uint32;
// typedef int int32;
typedef int32_t int32;
// typedef unsigned short uint16;
typedef uint16_t uint16;
// typedef short int16;
typedef int16_t int16;
// typedef unsigned char uint8;
typedef uint8_t uint8;
// typedef char int8;
typedef int8_t int8;

/* task-state segment */
struct tss_struct {
  uint16 link;
  uint16 link_r;
  uint32 esp0;
  uint16 ss0;
  uint16 ss0_r;
  uint32 esp1;
  uint16 ss1;
  uint16 ss1_r;
  uint32 esp2;
  uint16 ss2;
  uint16 ss2_r;
  uint32 cr3;
  uint32 eip;
  uint32 eflags;
  uint32 eax;
  uint32 ecx;
  uint32 edx;
  uint32 ebx;
  uint32 esp;
  uint32 ebp;
  uint32 esi;
  uint32 edi;
  uint16 es;
  uint16 es_r;
  uint16 cs;
  uint16 cs_r;
  uint16 ss;
  uint16 ss_r;
  uint16 ds;
  uint16 ds_r;
  uint16 fs;
  uint16 fs_r;
  uint16 gs;
  uint16 gs_r;
  uint16 iopb_r;
  uint16 iopb;
} __attribute__((packed));
typedef struct tss_struct tss_t;

struct gdt_entry_struct {
  uint16 limit_start;
  uint16 base_start;
  uint8 base_middle;
  uint8 access;
  uint8 limit_and_flags;
  uint8 base_end;
} __attribute__((packed));
typedef struct gdt_entry_struct gdt_entry_t;

struct gdt_ptr_struct {
  uint16 limit;
  uint32 base;
} __attribute__((packed));
typedef struct gdt_ptr_struct gdt_ptr_t;

// #IFDEF __x86_64__
struct idt_entry {
  uint16 offset_start;
  uint16 selector;
  uint8 zero;
  uint8 type_attr;
  uint16 offset_mid;
  uint32 offset_end;
  uint32 reserved;
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
  uint16 limit;

  uint64 base;
} __attribute__((packed));
typedef struct idt_ptr_struct idt_ptr_t;

gdt_ptr_t gdt;
gdt_entry_t gdt_entries[6];

idt_ptr_t idt;
idt_entry_t idt_entries[256] = {0};

tss_t tss;

extern void gdt_update(gdt_ptr_t*);
extern void idt_update(idt_ptr_t*);

void gdt_setup();
void gdt_set_entry(uint32, uint32, uint32, uint8, uint8);
void gdt_set_gate(uint32 entry,
		  uint32 base,
		  uint32 limit,
		  uint8 access,
		  uint8 flags);

void idt_setup();
// void idt_set_entry(uint32, uint32, uint16, uint8);
void idt_set_gate(uint32 interrupt,
		  uint64 offset,
		  uint16 selector,
		  uint8 type_attr);

struct interrupt_registers_struct {
  uint32 ds;                                      // Data segment selector
  uint32 edi, esi, ebp, esp, ebx, edx, ecx, eax;  // Pushed by pusha.
  uint32 int_no, err_code;  // Interrupt number and error code (if applicable)
  uint32 eip, cs, eflags, useresp,
      ss;  // Pushed by the processor automatically.
} __attribute__((packed));
typedef struct registers_struct interrupt_registers_t;

extern isr0;             // divide error, no error code, fault
extern void isr3(void);  // breakpoint, no error code, trap
extern void isr4(void);  // overflow, no error code, trap
extern void isr8(void);
extern isr12;             // stack-segment fault, error code, fault
extern void isr13(void);  // general protection fault, error code, fault
extern void isr14(void);  // page fault, error code, fault
extern void isr32(void);
extern void isr0x31(void);
extern void isr0x32(void);
extern void isr0x33(void);

struct interrupt_registers {
  //  uint32 ds;                                     // data segment selector
  // uint32 edi, esi, ebp, esp, ebx, edx, ecx, eax; // pushed by pushad
  uint64 int_no, err_code;
  uint64 eip, cs, eflags, rsp, ss;  // pushed by cpu after interrupt
} __attribute__((packed));

#define EXCEPTION_PAGE_FAULT 14

char* interrupt_names[15] = {
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "Double Fault Exception",
    "9",
    "10",
    "11",
    "12",
    "General Protection Exception",  // 13
    "Page Fault Exception"           // 14
    "15",
    "16",
    "17",
    "18",
    "19",
    "20",
    "21",
};

typedef union {
  uint32_t raw;
  struct {
    uint32_t p : 1;
    uint32_t wr : 1;
    uint32_t us : 1;
    uint32_t rsvd : 1;
    uint32_t id : 1;
    uint32_t pk : 1;
    uint32_t ss : 1;
    uint32_t hlat : 1;
    uint32_t reserved : 24;
  } flags;
} page_fault_error_t;

// assembler
static inline void outb(uint16 port, uint8 val) {
  __asm__ volatile("outb %0, %1" : : "a"(val), "Nd"(port));
  /* There's an outb %al, $imm8  encoding, for compile-time constant port
   * numbers that fit in 8b.  (N constraint).
   * Wider immediate constants would be truncated at assemble-time (e.g. "i"
   * constraint).
   * The  outb  %al, %dx  encoding is the only option for all other cases.
   * %1 expands to %dx because  port  is a uint16_t.  %w1 could be used if
   * we
   * had the port number a wider C type */
}

static inline uint8 inb(uint16 port) {
  uint8 ret;
  __asm__ volatile("inb %1, %0" : "=a"(ret) : "Nd"(port));
  return ret;
}

void ps2_wait_ready() {
  while (inb(0x64) & 0x2) {
  };
}

void ps2_wait_data() {
  while (!(inb(0x64) & 0x1)) {
  };
}

int mouse_x = 0;
int mouse_y = 0;

int max(int x, int y) {
  if (x > y) {
    return x;
  }
  return y;
}

int min(int x, int y) {
  if (x < y) {
    return x;
  }
  return y;
}

// regs is passed via rdi
void interrupt_handler(struct interrupt_registers* regs) {
  if (regs->int_no == 0x33) {  // network IRQ
    // read ISR and figure out which interrupt triggered.

    volatile uint32_t* local_apic_eoi = 0xfee000b0;
    *local_apic_eoi = 0;
    return;
  }
  if (regs->int_no == 0x32) {  // mouse IRQ
    // data:
    // bit 0:
    // bit 1:
    // bit 2:
    // bit 3:
    // bit 4: x sign
    // bit 5: y sign
    // bit 6: x overflow
    // bit 7: y overflow
    uint8_t data, x, y;
    ps2_wait_data();
    data = inb(0x60);
    ps2_wait_data();
    x = inb(0x60);
    ps2_wait_data();
    y = inb(0x60);

    // the 9 bit two's complements relative x,y values come in 2 pieces.
    // an 8 bit value and a sign bit.
    // wikipedia says to subtract the sign bit. extract and subtract.

    int16_t rel_x = x - ((data << 4) & 0x100);

    int16_t rel_y = -(y - ((data << 3) & 0x100));

    mouse_x = min(max(0, mouse_x + rel_x), 79);
    mouse_y = min(max(1, mouse_y + rel_y), 24);

    ypos = mouse_y;
    xpos = mouse_x;
    print_character_color('o', 0x04);

    cll(0);
    ypos = 0;
    xpos = 0;
    printf("data: %x x: %d, y: %d, rel_x: %d, rel_y: %d", data, mouse_x,
	   mouse_y, rel_x, rel_y);
    volatile uint32_t* local_apic_eoi = 0xfee000b0;
    *local_apic_eoi = 0;
    return;
  }
  if (regs->int_no == 0x31) {  // keyboard IRQ
    // ref: https://wiki.osdev.org/%228042%22_PS/2_Controller
    // read from port 0x60? this is the data port
    // 0x64 is the status register if read and command register if written.
    uint8 scancode = inb(0x60);
    // key released
    // It seems this is numbered from top left to bottom right.
    switch (scancode) {
      case 0x1c:  // enter
	printf("\n");
	break;
      case 0xb9:  // space
	printf(" ");
	break;
      case 0x9e:
	printf("a");
	break;
      case 0xb0:
	printf("b");
	break;
      case 0xae:
	printf("c");
	break;
      case 0xa0:
	printf("d");
	break;
      case 0x92:
	printf("e");
	break;
      case 0xa1:
	printf("f");
	break;
      case 0xa2:
	printf("g");
	break;
      case 0xa3:
	printf("h");
	break;
      case 0x97:
	printf("i");
	break;
      case 0xa4:
	printf("j");
	break;
      case 0xa5:
	printf("k");
	break;
      case 0xa6:
	printf("l");
	break;
      case 0xb2:
	printf("m");
	break;
      case 0xb1:
	printf("n");
	break;
      case 0x98:
	printf("o");
	break;
      case 0x99:
	printf("p");
	break;
      case 0x90:
	printf("q");
	break;
      case 0x93:
	printf("r");
	break;
      case 0x9f:
	printf("s");
	break;
      case 0x94:
	printf("t");
	break;
      case 0x96:
	printf("u");
	break;
      case 0xaf:
	printf("v");
	break;
      case 0x91:
	printf("w");
	break;
      case 0xad:
	printf("x");
	break;
      case 0x95:
	printf("y");
	break;
      case 0xac:
	printf("z");
	break;
    }
    volatile uint32_t* local_apic_eoi = 0xfee000b0;
    *local_apic_eoi = 0;
    return;
  }

  if (regs->int_no < 15) {
    printf("interrupt: %s\n", interrupt_names[regs->int_no]);
  } else {
    printf("interrupt: %x\n", regs->int_no);
  }
  printf("eflags: %d\n", regs->eflags);
  printf("ss: %d\n", regs->ss);
  printf("rsp: %x\n", regs->rsp);
  printf("cs: %d\n", regs->cs);
  printf("ip: %x\n", regs->eip);

  if (regs->int_no == EXCEPTION_PAGE_FAULT) {
    page_fault_error_t* e = &regs->err_code;
    printf(
	"p: %d, wr: %d, us: %d, rsvd: %d, id: %d, pk: %d, ss: %d, hlat: %d\n",
	e->flags.p, e->flags.wr, e->flags.us, e->flags.rsvd, e->flags.id,
	e->flags.pk, e->flags.ss, e->flags.hlat);
    // cr2 contains the address that caused the page fault.
    uint64_t addr;
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
  idt.base = (uint64)&idt_entries;
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
// architecturally-defined exceptions, the processor generates an interrupt to
// the correct vector (to access the exception handler) but does not push an
// error code on the stack. This is true even if the associated
// hardware-generated exception normally produces an error code. The exception
// handler will still attempt to pop an error code from the stack while
// handling the exception. Because no error code was pushed, the handler will
// pop off and discard the EIP instead (in place of the missing error code).
// This sends the return to the wrong location.

// ref: 6.4.2 Software-Generated Exceptions

// 00077694943d[CPU0  ] LONG MODE IRET
// 00077694943e[CPU0  ] fetch_raw_descriptor: GDT: index (e67) 1cc > limit (f)
#define INTERRUPT_GATE 0x8E
  idt_set_gate(0, 0, 0x08, 0x0E);
  idt_set_gate(1, 0, 0x08, 0x0E);
  idt_set_gate(2, 0, 0x08, 0x0E);
  idt_set_gate(3, isr3, 0x08, 0x8E);
  idt_set_gate(4, isr4, 0x08, 0x8E);
  idt_set_gate(5, 0, 0x08, 0x0E);
  idt_set_gate(6, 0, 0x08, 0x0E);
  idt_set_gate(7, 0, 0x08, 0x0E);
  idt_set_gate(8, isr8, 0x08, 0x8E);
  idt_set_gate(9, 0, 0x08, 0x0E);
  idt_set_gate(10, 0, 0x08, 0x0E);
  idt_set_gate(11, 0, 0x08, 0x0E);
  idt_set_gate(12, 0, 0x08, 0x0E);
  idt_set_gate(13, isr13, 0x08, 0x8E);
  idt_set_gate(14, isr14, 0x08, 0x8E);
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
  idt_set_gate(32, isr32, 0x08, 0x8E);
  idt_set_gate(0x31, isr0x31, 0x08, 0x8E);  // keyboard
  idt_set_gate(0x32, isr0x32, 0x08, 0x8E);  // mouse
  idt_set_gate(0x33, isr0x33, 0x08, 0x8e);  // ethernet

  idt_update(&idt);
}

void idt_set_gate(uint32 interrupt,
		  uint64 offset,
		  uint16 selector,
		  uint8 type_attr) {
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

void gdt_setup() {
  gdt.limit = sizeof(gdt_entry_t) * 5 - 1;
  gdt.base = (uint32)&gdt_entries;

  gdt_set_gate(0, 0, 0, 0, 0);
  gdt_set_gate(1, 0, 0xffffffff, 0x9a, 0xcf);  // kernel mode code segment
  gdt_set_gate(2, 0, 0xffffffff, 0x92, 0xcf);  // kernel mode data segment
  gdt_set_gate(3, 0, 0xffffffff, 0xfa, 0xcf);  // user mode code segment
  gdt_set_gate(4, 0, 0xffffffff, 0xf2, 0xcf);  // user mode data segment
  gdt_set_gate(5, (uint32)&tss, sizeof(tss), 0x89,
	       0x40);  // cpu1 task switching segment

  gdt_update(&gdt);
}

void gdt_set_gate(uint32 entry,
		  uint32 base,
		  uint32 limit,
		  uint8 access,
		  uint8 flags) {
  gdt_entries[entry].base_start = (0xffff & base);
  gdt_entries[entry].base_middle = (base >> 16) & 0xff;
  gdt_entries[entry].base_end = (base >> 24) & 0xff;

  gdt_entries[entry].limit_start = (0xffff & limit);
  gdt_entries[entry].limit_and_flags = (limit >> 16) & 0x0f;
  gdt_entries[entry].limit_and_flags |= (flags & 0xf0);

  gdt_entries[entry].access = access;
}

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
  uint64_t reserve_1[4] __attribute__((aligned(16)));
  uint32_t local_apic_id __attribute__((aligned(16)));
  uint64_t local_apic_version[2] __attribute__((aligned(16)));
  uint64_t reserve_2[8] __attribute__((aligned(16)));
  uint64_t tpr __attribute__((aligned(16)));
  uint64_t apr __attribute__((aligned(16)));
  uint64_t prr __attribute__((aligned(16)));
  uint64_t eoi __attribute__((aligned(16)));
  uint64_t rrd __attribute__((aligned(16)));
  uint64_t logical_destination __attribute__((aligned(16)));
  uint64_t destination_format __attribute__((aligned(16)));
  uint64_t spurious_interrupt_vector __attribute__((aligned(16)));
  uint32_t isr_0 __attribute__((aligned(16)));  // in service register ro
  uint32_t isr_1 __attribute__((aligned(16)));
  uint32_t isr_2 __attribute__((aligned(16)));
  uint32_t isr_3 __attribute__((aligned(16)));
  uint32_t isr_4 __attribute__((aligned(16)));
  uint32_t isr_5 __attribute__((aligned(16)));
  uint32_t isr_6 __attribute__((aligned(16)));
  uint32_t isr_7 __attribute__((aligned(16)));
  uint32_t tmr_0 __attribute__((aligned(16)));  // trigger mode register ro
  uint32_t tmr_1 __attribute__((aligned(16)));
  uint32_t tmr_2 __attribute__((aligned(16)));
  uint32_t tmr_3 __attribute__((aligned(16)));
  uint32_t tmr_4 __attribute__((aligned(16)));
  uint32_t tmr_5 __attribute__((aligned(16)));
  uint32_t tmr_6 __attribute__((aligned(16)));
  uint32_t tmr_7 __attribute__((aligned(16)));
  uint32_t irr_0 __attribute__((aligned(16)));  // interrupt request register ro
  uint32_t irr_1 __attribute__((aligned(16)));
  uint32_t irr_2 __attribute__((aligned(16)));
  uint32_t irr_3 __attribute__((aligned(16)));
  uint32_t irr_4 __attribute__((aligned(16)));
  uint32_t irr_5 __attribute__((aligned(16)));
  uint32_t irr_6 __attribute__((aligned(16)));
  uint32_t irr_7 __attribute__((aligned(16)));
  uint32_t error_status __attribute__((aligned(16)));
  uint64_t reserve_3[12] __attribute__((aligned(16)));
  uint32_t cmci __attribute__((aligned(16)));
  uint32_t icr_low __attribute__((aligned(16)));
  uint32_t icr_high __attribute__((aligned(16)));
  uint32_t lvt_timer __attribute__((aligned(16)));
  uint32_t lvt_thermal_sensor __attribute__((aligned(16)));
  uint32_t lvt_performance_monitoring_counters __attribute__((aligned(16)));
  uint32_t lvt_lint0 __attribute__((aligned(16)));  // FEE00350
  uint32_t lvt_lint1 __attribute__((aligned(16)));  // FEE00360
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
  // this just accesses the pointer. have to do [0] but then page fault because
  // maybe not mapped.
  // it's defined as 64 bit but I think it's just 32?
  // aligned 16 worked.

  // TODO
  // set up spurious interrupt
  // remap & disable pic
  // enable apic
  // configure keyboard IRQ on ioapic
}

// TODO: use timer to print clock

uint64_t rdmsr(uint32_t reg) {
  uint64_t value;
  // A = eax + edx
  // c = ecx (the c register)
  // ref:
  // https://gcc.gnu.org/onlinedocs/gcc/extensions-to-the-c-language-family/how-to-use-inline-assembly-language-in-c-code.html#x86-family-config-i386-constraints-md
  asm volatile("rdmsr" : "=A"(value) : "c"(reg));
  return value;
}

#define MSR_APIC_BASE 0x1b

// ref: Vol 4 2.1
typedef union {
  uint64_t raw;
  struct {
    uint64_t reserved : 8;
    uint64_t bsp : 1;
    uint64_t reserved_2 : 1;
    uint64_t x2apic : 1;
    uint64_t apic_global : 1;
    uint64_t base_address : 52;
  } bits;
} msr_apic_base_t;

struct ioapic_redirection_register {
  union {
    uint32_t lower;
    struct {
      uint32_t interrupt_vector : 8;
      uint32_t delivery_mode : 3;
      uint32_t destination_mode : 1;
      uint32_t delivery_status : 1;
      uint32_t pin_polarity : 1;
      uint32_t remote_irr : 1;
      uint32_t trigger_mode : 1;  // 1=level, 0=edge
      uint32_t interrupt_mask : 1;
      uint32_t reserved : 16;
    } lower_bits;
  };

  union {
    uint32_t upper;
    struct {
      uint32_t reserved : 24;
      uint32_t destination : 8;
    } upper_bits;
  };
} __attribute__((packed));
typedef struct ioapic_redirection_register ioapic_redirection_register_t;

// Seems Intel and AMD have this at the same address.
// Normally it can be found via the Multiple APIC Description Table (MADT).
// ref: https://wiki.osdev.org/MADT
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

uint32_t ioapic_read_register(uint32_t reg) {
  uint32_t volatile* ioregsel = (uint32_t volatile*)IOAPIC_IOREGSEL;
  ioregsel[0] = reg;
  return ioregsel[4];  // IOAPIC_IOREGSEL+10h (4*4 byte = 16 byte) =
		       // IOAPIC_IOWIN
}

void ioapic_write_register(uint32_t reg, ioapic_redirection_register_t* r) {
  uint32_t volatile* ioregsel = (uint32_t volatile*)IOAPIC_IOREGSEL;
  ioregsel[0] = reg;
  ioregsel[4] = r->lower;
  ioregsel[0] = reg + 1;
  ioregsel[4] = r->upper;
}

// keyboard
#define IOAPIC_IRQ1 0x12
// ethernet
#define IOAPIC_IRQ11 0x26
// mouse
#define IOAPIC_IRQ12 0x28

void ioapic_setup() {
  msr_apic_base_t apic_base;
  apic_base.raw = rdmsr(MSR_APIC_BASE);
  printf("apic enabled: %d, apic base address: %x\n",
	 apic_base.bits.apic_global, apic_base.bits.base_address);

  printf("ioapic id: %d\n", ioapic_read_register(0));
  printf("ioapic version: %d\n", ioapic_read_register(1) & 0xFF);  // version
  // = first 8 bits.

  // next steps:
  // find apic id
  apic_registers_t* regs = (apic_registers_t*)0xFEE00000;
  printf("apic id: %d\n", regs->local_apic_id >> 24);
  // mask irq 2, this is already masked
  /* ioapic_redirection_register_t r0 = {0}; */
  /* r0.upper_bits.destination = regs->local_apic_id >> 24;  // apic id */
  /* r0.lower_bits.interrupt_mask = 1; */
  /* ioapic_write_register(0x14, &r0); */
  // printf("ioapic irq 0 vector: %d\n", ioapic_read_register(0x10) &
  // 0x000000FF);
  //  map irq 1 to user defined interrupt vector
  ioapic_redirection_register_t r = {0};
  r.upper_bits.destination = regs->local_apic_id >> 24;  // apic id
  r.lower_bits.interrupt_vector = 0x31;
  ioapic_write_register(0x12, &r);
  printf("ioapic irq 1 vector: %d\n", ioapic_read_register(0x12) & 0x000000FF);
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

  // Setup ps2 controller, mouse and keyboard.
  // Sometimes ps2_wait_data can hang until a key is pressed. Not sure why.
  // do we need to enable ps2 port 2? mouse
  // ps2 controller spec:
  // https://web.archive.org/web/20210417040153/http://www.diakom.ru/el/elfirms/datashts/Smsc/42w11.pdf
  // write command to
  // command port 0x64
  // and then read from
  // data port 0x60
  ps2_wait_ready();
  outb(0x64, 0x20);  // 0x20 = read config byte
  ps2_wait_data();
  uint8 config = inb(0x60);
  printf("config byte 0: %x\n", config);
  ps2_wait_ready();

  ps2_wait_ready();
  outb(0x64, 0xAD);  // disable first port
  ps2_wait_ready();
  outb(0x64, 0xA7);  // disable second port

  ps2_wait_ready();
  outb(0x64, 0x20);  // 0x20 = read config byte
  ps2_wait_data();
  config = inb(0x60);
  printf("config byte 1: %x\n", config);
  ps2_wait_ready();
  outb(0x64, 0x60);  // set
  ps2_wait_ready();
  //  outb(0x60, (config | 0x2) & ~0x20);
  outb(0x60,
       config & ~0x43);  // disable translation and interrupts, bit 1,2 and 6
  // returns 0x61 = 0b1100001
  // bit 0: first ps2 port interrupt enabled
  // bit 1: second ps2 port interrupt enabled
  ps2_wait_ready();
  outb(0x64, 0x20);  // 0x20 = read config byte
  ps2_wait_data();
  config = inb(0x60);
  printf("config byte 2: %x\n", config);

  ps2_wait_ready();
  outb(0x64, 0xAA);  // self test
  ps2_wait_data();
  uint8 resp = inb(0x60);
  printf("self test response: %x\n", resp);

  ps2_wait_ready();
  outb(0x64, 0x20);  // 0x20 = read config byte
  ps2_wait_data();
  config = inb(0x60);
  printf("config byte 3: %x\n", config);

  ps2_wait_ready();
  outb(0x64, 0xAB);  // test first */
  ps2_wait_data();
  uint8 response = inb(0x60);
  // printf("response 1: %x\n", response);

  ps2_wait_ready();
  outb(0x64, 0xA9);  // test second
  ps2_wait_data();
  response = inb(0x60);
  // printf("response 2: %x\n", response);

  ps2_wait_ready();
  outb(0x64, 0xAE);  // enable first port

  ps2_wait_ready();
  outb(0x64, 0x20);  // 0x20 = read config byte
  ps2_wait_data();
  config = inb(0x60);
  printf("config byte 4: %x\n", config);

  ps2_wait_ready();
  outb(0x64, 0xA8);  // enable second port

  ps2_wait_ready();
  outb(0x64, 0x20);  // 0x20 = read config byte
  ps2_wait_data();
  config = inb(0x60);
  printf("config byte 5: %x\n", config);

  ps2_wait_ready();
  outb(0x60, 0xFF);  // reset device 1
  ps2_wait_data();
  response = inb(0x60);
  // printf("reset response 1: %x\n", response);
  ps2_wait_data();
  response = inb(0x60);
  // printf("reset response 1: %x\n", response);

  ps2_wait_ready();
  outb(0x64, 0xD4);
  ps2_wait_ready();
  outb(0x60, 0xFF);  // reset device 2
  ps2_wait_data();
  response = inb(0x60);
  //  printf("reset response 1: %x\n", response);
  ps2_wait_data();
  response = inb(0x60);
  // printf("reset response 1: %x\n", response);
  ps2_wait_data();
  response = inb(0x60);
  // printf("reset response 1: %x\n", response);

  ps2_wait_ready();
  outb(0x64, 0xD4);
  ps2_wait_ready();
  outb(0x60, 0xF4);  // enable mouse data reporting
  ps2_wait_data();
  response = inb(0x60);
  // printf("enable response: %x\n", response);

  // enable IRQs
  printf("setting config to: %x\n", config | 0x3);
  ps2_wait_ready();
  outb(0x64, 0x60);  // set
  ps2_wait_ready();
  outb(0x60, config | 0x3);  // enable interrupts. bits 1,2
  ps2_wait_ready();
  outb(0x64, 0x20);  // 0x20 = read config byte
  ps2_wait_data();
  config = inb(0x60);
  printf("config byte 6: %x", config);

  // returns: 0x41 = 0b1000001
  // we can find irq that are remapped in the ioapic from the default.
  // https://uefi.org/specs/ACPI/6.5/05_ACPI_Software_Programming_Model.html#interrupt-source-override-structure
}

// TODO: pull entries out as 'first' into sdt struct.
// sdt: system description table
struct acpi_sdt_header2 {
  uint8_t signature[4];
  uint32_t length;
  uint8_t revision;
  uint8_t checksum;
  uint8_t oem_id[6];
  uint8_t oem_table_id[8];
  uint32_t oem_revision;
  uint32_t creator_id;
  uint32_t creator_revision;
} __attribute__((packed));
typedef struct acpi_sdt_header2 acpi_sdt_header2_t;

struct acpi_sdt_header {
  uint8_t signature[4];
  uint32_t length;
  uint8_t revision;
  uint8_t checksum;
  uint8_t oem_id[6];
  uint8_t oem_table_id[8];
  uint32_t oem_revision;
  uint32_t creator_id;
  uint32_t creator_revision;
  uint32_t entries;  // length - (sizeof(header) - 4 byte)
} __attribute__((packed));
typedef struct acpi_sdt_header acpi_sdt_header_t;

struct acpi_rsdp {
  uint8_t signature[8];
  uint8_t checksum;
  uint8_t oem_id[6];
  uint8_t revision;
  uint32_t rsdt;
} __attribute__((packed));
typedef struct acpi_rsdp acpi_rsdp_t;

acpi_rsdp_t* locate_rsdp() {
  // ref: https://wiki.osdev.org/RSDP
  // ref:
  // https://uefi.org/specs/ACPI/6.5/05_ACPI_Software_Programming_Model.html#finding-the-rsdp-on-ia-pc-systems
  uint64_t* start = 0xE0000;
  uint64_t* end = 0xFFFFF;

  // "RSD PTR "
  for (; start < end; start += 2) {
    uint8_t* s = start;
    if (s[0] == 'R' && s[1] == 'S' && s[2] == 'D' && s[3] == ' ' &&
	s[4] == 'P' && s[5] == 'T' && s[6] == 'R' && s[7] == ' ') {
      return start;
    }
  }
  return NULL;
}

bool strncmp(char* s1, char* s2, int n) {
  for (int i = 0; i < n; i++) {
    if (s1[i] != s2[i]) {
      return false;
    }
  }
  return true;
}

struct acpi_madt {
  acpi_sdt_header2_t header;
  uint32_t local_interrupt_controller_address;
  uint32_t flags;
  uint32_t first;
} __attribute__((packed));
typedef struct acpi_madt acpi_madt_t;

// ics = interrupt controller structure
struct acpi_ics_header {
  uint8_t type;
  uint8_t length;
} __attribute__((packed));
typedef struct acpi_ics_header acpi_ics_header_t;

struct acpi_ics_ioapic {
  acpi_ics_header_t header;
  uint8_t id;
  uint8_t reserved;
  uint32_t address;
  uint32_t global_system_interrupt_base;
} __attribute__((packed));
typedef struct acpi_ics_ioapic acpi_ics_ioapic_t;

struct acpi_ics_input_source_override {
  acpi_ics_header_t header;
  uint8_t bus;
  uint8_t source;
  uint32_t global_system_interrupt;
  uint16_t flags;
} __attribute__((packed));
typedef struct acpi_ics_input_source_override acpi_ics_input_source_override_t;

// note: take care when taking references of a pointer.
void list_tables(acpi_sdt_header_t* rsdt) {
  int count = (rsdt->length - sizeof(acpi_sdt_header_t) + sizeof(uint32_t)) /
	      sizeof(uint32_t);
  printf("rsdt: entry count: %d\n", count);
  for (int i = 0; i < count; i++) {
    // &entries to get the first entry.
    // +i uses size of type which is uint32_t.
    // * because it's a pointer to some place.
    acpi_sdt_header_t* h = *(&rsdt->entries + i);
    printf("table %d: %.*s\n", i, 4, h->signature);
    if (strncmp(h->signature, "APIC", 4)) {
      acpi_madt_t* madt = h;
      printf("configuring acpi: %x\n",
	     madt->local_interrupt_controller_address);
      // how many? madt->length?
      // first
      int j = 0;
      for (acpi_ics_header_t* h = &(madt->first);; h = (char*)h + h->length) {
	printf("type: %d, length: %d\n", h->type, h->length);
	if (h->type == 1) {
	  acpi_ics_ioapic_t* ioapic = h;
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
	  acpi_ics_input_source_override_t* iso = h;
	  printf("source: %x, interrupt: %x\n", iso->source,
		 iso->global_system_interrupt);
	}
	j++;
	if (j == 10) {
	  return;
	}
      }
    }
  }
}

#define PCI_CONFIG_ADDRESS 0xcf8
#define PCI_CONFIG_DATA 0xcfc

typedef union {
  uint32_t raw;
  struct {
    uint32_t offset : 8;
    uint32_t function : 3;
    uint32_t device : 5;
    uint32_t bus : 8;
    uint32_t reserved : 7;
    uint32_t enabled : 1;
  } bits;
} pci_config_address_t;

typedef union {
  uint32_t raw;
  struct {
    uint16_t vendor_id;
    uint16_t device_id;
  } __attribute__((packed)) fields;
} pci_config_register_0_t;

typedef union {
  uint32_t raw;
  struct {
    union {
      uint16_t command;
      struct {
	uint16_t io_space : 1;
	uint16_t memory_space : 1;
	uint16_t bus_master : 1;
	uint16_t reserved : 13;
      } command_bits;
    };
    union {
      uint16_t status;
      struct {
	uint16_t reserved : 16;
      } status_bits;
    };
  } __attribute__((packed)) fields;
} pci_config_register_1_t;

typedef union {
  uint32_t raw;
  struct {
    uint8_t revision_id;
    uint8_t prog_if;
    uint8_t subclass;
    uint8_t class;
  } __attribute__((packed)) fields;
} pci_config_register_2_t;

typedef union {
  uint32_t raw;
  struct {
    uint8_t cache_line_size;
    uint8_t latency_timer;
    uint8_t header_type;
    uint8_t bist;
  } __attribute__((packed)) fields;
} pci_config_register_3_t;

typedef union {
  uint32_t raw;
  struct {
    uint32_t is_io_space : 1;
    uint32_t type : 2;
    uint32_t prefetchable : 1;
    uint32_t address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    uint32_t is_io_space : 1;
    uint32_t reserved : 1;
    uint32_t address : 30;
  } __attribute__((packed)) io_space;
} pci_config_register_4_t;

typedef union {
  uint32_t raw;
  struct {
    uint32_t is_io_space : 1;
    uint32_t type : 2;
    uint32_t prefetchable : 1;
    uint32_t address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    uint32_t is_io_space : 1;
    uint32_t reserved : 1;
    uint32_t io_size : 6;
    uint32_t address : 24;
  } __attribute__((packed)) io_space;
} pci_config_register_4_rtl8139_t;

typedef union {
  uint32_t raw;
  struct {
    uint8_t interrupt_line;
    uint8_t interrupt_pin;
    uint8_t min_grant;
    uint8_t max_latency;
  } __attribute__((packed)) fields;
} pci_config_register_f_t;

static inline void outl(uint16_t port, uint32_t val) {
  __asm__ volatile("outl %k0, %w1" : : "a"(val), "Nd"(port) : "memory");
}

static inline uint32_t inl(uint16_t port) {
  uint32_t ret;
  __asm__ volatile("inl %w1, %k0" : "=a"(ret) : "Nd"(port) : "memory");
  return ret;
}

static inline void outw(uint16_t port, uint16_t val) {
  __asm__ volatile("outw %w0, %w1" : : "a"(val), "Nd"(port) : "memory");
}

static inline uint16_t inw(uint16_t port) {
  uint16_t ret;
  __asm__ volatile("inw %w1, %w0" : "=a"(ret) : "Nd"(port) : "memory");
  return ret;
}

#define RTL8139_MAC 0x0
#define RTL8139_MAR 0x8
#define RTL8139_RBSTART 0x30
#define RTL8139_CMD 0x37
#define RTL8139_IMR 0x3c
#define RTL8139_ISR 0x3e

#define RTL8139_CONFIG_1 0x52

// PCI vendor
// - device
// 0x8086 Intel
// - 0x1237 440FX - 82441FX PMC [Natoma]
// - 0x7000 82371SB PIIX3 ISA [Natoma/Triton II]
// 0x1234 Bochs
// - 0x1111 Bochs Graphic Adapter
// 0x10ec Realtek
// - 0x8139 RTL-8100/8101L/8139 PCI Fast Ethernet Adapter

// PCI class
// - subclass
// 0x2 Network controller
// - 0x0 Ethernet controller
// 0x3 Display controller
// - 0x0 VGA compatible
// 0x6 Bridge
// - 0x0 Host Bridge
// - 0x1 ISA Bridge
//

char network_rx_buffer[8208];

void pci_enumerate() {
  pci_config_address_t a = {0};
  a.bits.enabled = 1;
  a.bits.bus = 0;
  a.bits.device = 0;

  // Root bus is apparently always 0 so we could scan from there.
  for (int bus = 0; bus < 256; bus++) {
    if (bus > 1) {
      break;
    }
    // printf("pci: scanning bus %d\n", bus);
    a.bits.bus = bus;
    for (int device = 0; device < 32; device++) {
      a.bits.device = device;
      a.bits.offset = 0;
      outl(PCI_CONFIG_ADDRESS, a.raw);
      pci_config_register_0_t h1;
      h1.raw = inl(PCI_CONFIG_DATA);

      if (h1.fields.vendor_id == 0xffff) {
	continue;
      }
      //    printf("pci: %d\n", device);
      //    printf("pci: vendor: %x, device: %x\n", h1.fields.vendor_id,
      //	     h1.fields.device_id);

      a.bits.offset = 0x8;
      outl(PCI_CONFIG_ADDRESS, a.raw);
      pci_config_register_2_t h2;
      h2.raw = inl(PCI_CONFIG_DATA);
      //      printf("pci: class: %x, subclass: %x\n", h2.fields.class,
      //	     h2.fields.subclass);

      a.bits.offset = 0xc;
      outl(PCI_CONFIG_ADDRESS, a.raw);
      pci_config_register_3_t h3;
      h3.raw = inl(PCI_CONFIG_DATA);
      //    printf("pci header: %x\n", h3.fields.header_type);

      if (h1.fields.vendor_id == 0x10ec && h1.fields.device_id == 0x8139) {
	// found ethernet controller
	a.bits.offset = 0x4;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_1_t h1;
	h1.raw = inl(PCI_CONFIG_DATA);
	/* printf("pci status: %x, command: %x, bus master: %d\n", */
	/*        h1.fields.status, h1.fields.command, */
	/*        h1.fields.command_bits.bus_master); */

	h1.fields.command_bits.bus_master = 1;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	outl(PCI_CONFIG_DATA, h1.raw);

	outl(PCI_CONFIG_ADDRESS, a.raw);
	h1.raw = inl(PCI_CONFIG_DATA);
	/* printf("pci status: %x, command: %x, bus master: %d\n", */
	/*        h1.fields.status, h1.fields.command, */
	/*        h1.fields.command_bits.bus_master); */

	a.bits.offset = 0x10;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_4_rtl8139_t h4;
	h4.raw = inl(PCI_CONFIG_DATA);
	uint32_t base = h4.raw & 0xFFFFFFFC;
	//	printf("pci header: address 1: %x\n", base);
	if (h4.io_space.is_io_space) {
	  // printf("pci header: io space: address: %x, base: %x\n",
	  //	 h4.io_space.address, base);
	  //	  outb(base + 0x52, 0x0);  // start?

	  uint8_t mac[6] = {0};
	  mac[0] = inb(base + 0x00);
	  mac[1] = inb(base + 0x01);
	  mac[2] = inb(base + 0x02);
	  mac[3] = inb(base + 0x03);
	  mac[4] = inb(base + 0x04);
	  mac[5] = inb(base + 0x05);

	  printf("ethernet: mac: %x:%x:%x:%x:%x:%x\n", mac[0], mac[1], mac[2],
		 mac[3], mac[4], mac[5]);

	  //	  uint8_t config_1 = inb(base + RTL8139_CONFIG_1);
	  //	  printf("ethernet: config 1: %x\n", config_1);

	  //	  uint8_t config_3 = inb(base + 0x59);
	  //	  printf("ethernet: config 3: %x\n", config_3);

	  //	  uint8_t config_4 = inb(base + 0x5A);
	  //	  printf("ethernet: config 4: %x\n", config_4);

	  uint8_t cmd = inb(base + RTL8139_CMD);
	  // here reset is 1 as written on osdev. qemu bug.
	  printf("ethernet: cmd: %x\n", cmd);

	  // soft reset
	  outb(base + RTL8139_CMD, 0x10);
	  while ((inb(base + RTL8139_CMD) & 0x10) != 0) {
	  }
	  printf("ethernet: reset successful \n");

	  // 0x30 is a 4 byte receive buffer start address register.
	  //	  outl(base + 0x30, network_rx_buffer);

	  // Interrupt Mask Register
	  // 0x3c 16 bit
	  // bit 0: rx OK
	  // note: it is important to read / write the right size, i.e.
	  // outb/outw/outl. using the wrong one results in no action.
	  outw(base + 0x3c, 0x1);

	  // Receive Configuration Register
	  // 0x44
	  // Bit 1: Accept all packets
	  // Bit 2: Accept physical match packets
	  // Bit 3: Accept multicast packets
	  // Bit 4: Accept broadcast packets
	  // Bit 11, 12: decide receive buffer length.
	  // 00 = 8k + 16 byte
	  // 01 = 16k + 16 byte
	  // 10 = 32K + 16 byte
	  // 11 = 64K + 16 byte

	  outl(base + 0x44,
	       0xf | (1 << 7));  // wrap bit and rx flags

	  outb(base + 0x37, 0x0C);  // Enable RX and TX in command register

	  // everything until here works.
	} else {
	  printf("pci header: memory space: %d, type: %d, address: %x\n",
		 h4.memory_space.is_io_space, h4.memory_space.type,
		 h4.memory_space.address);
	}

	a.bits.offset = 0x14;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	h4.raw = inl(PCI_CONFIG_DATA);
	printf("pci header: address 2: %x, addr: %x\n", h4.raw,
	       h4.memory_space.address);

	a.bits.offset = 0x3c;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_f_t hf;
	hf.raw = inl(PCI_CONFIG_DATA);
	printf("pci header: interrupt line: %d, interrupt pin: %d\n",
	       hf.fields.interrupt_line, hf.fields.interrupt_pin);
      }
    }
  }
}

struct pcmp_processor_entry {
  uint8_t type;  // 0
  uint8_t local_apic_id;
  uint8_t local_apic_version;
  uint8_t cpu_flags;
  uint32_t signature;
  uint32_t feature_flags;
  uint64_t reserved;
} __attribute__((packed));
typedef struct pcmp_processor_entry pcmp_processor_entry_t;

// TODO: since we use sizeof later during parsing we need to make sure this
// actually is the right size.
struct pcmp_bus_entry {
  uint32_t type : 8;  // 1
  uint32_t bus_id : 8;
  uint64_t bus_type : 48;
} __attribute__((packed));
typedef struct pcmp_bus_entry pcmp_bus_entry_t;

struct pcmp_ioapic_entry {
  uint8_t type;  // 2
  uint8_t apic_io;
  uint8_t apic_version;
  uint8_t apic_flags;
  uint32_t address;
} __attribute__((packed));
typedef struct pcmp_ioapic_entry pcmp_ioapic_entry_t;

struct pcmp_interrupt_entry {
  uint8_t type;  // 3
  uint8_t interrupt_type;
  uint16_t interrupt_flags;
  uint8_t source_bus_id;
  struct {
    uint8_t signal_type : 2;
    uint8_t pci_device_number : 5;
    uint8_t reserved : 1;
  } source_bus_irq;
  uint8_t destination_apic_id;
  uint8_t destination_apic_int;
} __attribute__((packed));
typedef struct pcmp_interrupt_entry pcmp_interrupt_entry_t;

void locate_pcmp() {
  // ref:
  // https://web.archive.org/web/20121002210153/http://download.intel.com/design/archives/processors/pro/docs/24201606.pdf
  uint8_t* start = 0xE0000;
  uint8_t* end = 0xFFFFF;

  // "RSD PTR "
  for (; start < end; start += 1) {
    uint8_t* s = start;
    if (s[0] == 'P' && s[1] == 'C' && s[2] == 'M' && s[3] == 'P') {
      // find PCI interrupt on I/O apic
      uint16_t count = *(uint16_t*)(start + 34);
      start += 44;  // skip header
      printf("first entry: %x count: %d\n", start, count);
      s = start;
      for (int i = 0; i < count; i++) {
	if (*s == 0) {
	  //	  printf("cpu header\n");
	  s += sizeof(pcmp_processor_entry_t);
	} else if (*s == 1) {
	  pcmp_bus_entry_t* b = s;
	  //	  printf("bus header: %d\n", b->bus_id);
	  s += sizeof(pcmp_bus_entry_t);
	} else if (*s == 2) {
	  //	  printf("io apic header\n");
	  s += sizeof(pcmp_ioapic_entry_t);
	} else if (*s == 3) {
	  //	  printf("interrupt header\n");
	  pcmp_interrupt_entry_t* e = s;
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
	  pcmp_interrupt_entry_t* e = s;
	  /* printf( */
	  /*     "interrupt_type: %x, interrupt_flags: %x, source_bus_id: %x, "
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

void kmain(void* mbd, unsigned int magic) {
  if (magic != 0x36d76289) {
    printf("multiboot error: %x\n", magic);
    asm volatile("hlt");
  }

  // set pit 0 to one shot mode
  // bit 4-5 = access mode
  // bit 2-3 = mode
  // ref: https://www.diamondsystems.com/files/binaries/har82c54.pdf
  outb(0x43, 0b110010);

  /* You could either use multiboot.h */
  /* (http://www.gnu.org/software/grub/manual/multiboot/multiboot.html#multiboot_002eh)
   */
  /* or do your offsets yourself. The following is merely an example. */
  char* boot_loader_name = (char*)((long*)mbd)[16];

  /* Write your kernel here. */
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

  // apic_setup();
  ioapic_setup();
  acpi_rsdp_t* rsdp = locate_rsdp();
  //  if (rsdp == NULL) {
  //    // panic
  //  }
  //  printf("rsdp: revision: %d, rsdt_addr: %x\n", rsdp->revision,
  //  rsdp->rsdt);
  acpi_sdt_header_t* rsdt = rsdp->rsdt;
  //  printf("rsdt: %.*s", 4, rsdt->signature);
  //  list_tables(rsdt);
  pci_enumerate();
  // locate_pcmp();  // This told us that bus 0 device X (ethernet) is mapped
  // to.
  //  TODO: check delivery mode. IRQ 11 (0xB).
  return 0xDEADBABA;
}

/* // Note: I found using bitfields strongly discouraged, but I will still try
 * to */
/* // use them. ref: https://news.ycombinator.com/item?id=17056301 and many
 * more. */

/* // Paging table entry is 64 bits / 8 byte on a 64 bit system. */
/* struct pml4_entry { */
/*   unsigned int present : 1; */
/*   unsigned int rw : 1; */
/*   unsigned int us : 1; */
/*   unsigned int pwt : 1; */
/*   unsigned int pcd : 1; */
/*   unsigned int a : 1; */
/*   unsigned int ign : 1; */
/*   unsigned int reserved: 1; */
/*   unsigned int ignored : 3; */
/*   unsigned int r : 1; */
/*   unsigned int pointer_table_ptr : 40; */
/*   unsigned int reserved_2 : 12; */
/* }; */
/* typedef struct pml4_entry pml4_entry_t; */

/* struct page_directory_pointer_table_entry { */
/*   unsigned int present : 1; */
/*   unsigned int rw : 1; */
/*   unsigned int us : 1; */
/*   unsigned int pwt : 1; */
/*   unsigned int pcd : 1; */
/*   unsigned int a : 1; */
/*   unsigned int ign : 1; */
/*   unsigned int always_0 : 1; */
/*   unsigned int ignored : 3; */
/*   unsigned int r : 1; */
/*   unsigned int directory_ptr : 40; */
/*   unsigned int reserved_2 : 12; */
/* }; */
/* typedef page_directory_pointer_table_entry
 * page_directory_pointer_table_entry_t; */

/* struct page_directory_entry { */
/*   unsigned int present : 1; */
/*   unsigned int rw : 1; */
/*   unsigned int us : 1; */
/*   unsigned int pwt : 1; */
/*   unsigned int pcd : 1; */
/*   unsigned int a : 1; */
/*   unsigned int ign : 1; */
/*   unsigned int always_0 : 1; */
/*   unsigned int ignored : 3; */
/*   unsigned int r : 1; */
/*   unsigned int page_table_ptr : 40; */
/*   unsigned int reserved_2 : 12; */
/* }; */
/* typedef struct page_directory_entry page_directory_entry_t; */

/* struct page_table_entry { */
/*   unsigned int present : 1; */
/*   unsigned int rw : 1; */
/*   unsigned int us : 1; */
/*   unsigned int pwt : 1; */
/*   unsigned int pcd : 1; */
/*   unsigned int a : 1; */
/*   unsigned int d : 1; */
/*   unsigned int pat : 1; */
/*   unsigned int g : 1; */
/*   unsigned int ign : 2; */
/*   unsigned int r : 1; */
/*   unsigned int page_table_ptr : 40; */
/*   unsigned int reserved_2 : 12; // actually has prot. key inside. */
/* }; */
/* typedef struct page_table_entry page_table_entry_t; */

/* // Each paging table has 512 entries of size 8 bytes on a 64bit
 * architecture.
 */
/* // Section 4.2 in Intel Developer Manual. */
/* // Root paging structure needs to be put into CR3. */
/* // We will do 4 level paging. Section 4.5. */
/* // To enable we have to set */
/* // CR0.PG = 1, CR4.PAE = 1, IA32_EFER.LME = 1, and CR4.LA57 = 0. */
/* // Paging maps linear address space to physical address space. */
/* // PML4[PML4Entries] -> Page Directory Pointer Table[PDPTEntries] -> 1Gb
 * page | Page Directory */
/* // Page Directory -> 2Mb page | Page Table */
/* // Page Table Entry -> 4kb Page */
/* pml4_entry_t pml4[512]; */
/* page_directory_pointer_table_entry_t page_directory_pointer_table[512]; */
/* page_directory_entry_t page_directory[512]; */
/* page_table_entry_t page_table[512]; */

/* void init_temporary_page_tables() { */
/*   /\* for (unsigned int i = 0; i < 512; i++) { *\/ */
/*   /\*   page_table *\/ */
/*   /\* } *\/ */

/*   /\* pml4_entry_t e0 = { *\/ */
/*   /\*   .present = 1, *\/ */
/*   /\* }; *\/ */

/*   /\* pml4[0] = e0; *\/ */
/* } */

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
