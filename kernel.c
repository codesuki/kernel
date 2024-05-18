#include <stdint.h>

#define va_start(v, l) __builtin_va_start(v, l)
#define va_arg(v, l) __builtin_va_arg(v, l)
#define va_end(v) __builtin_va_end(v)
#define va_copy(d, s) __builtin_va_copy(d, s)
typedef __builtin_va_list va_list;

char *reverse(char *buffer, unsigned int length) {
  char *start = buffer;
  char *end = buffer + length - 1;
  while (start < end) {
    char tmp = *start;
    *start++ = *end;
    *end-- = tmp;
  }
  return buffer;
}

char *itoa(int value, char *buffer, unsigned int base) {
  char *s = buffer;
  if (value < 0) {
    *s++ = '-';
    ++buffer;
    value = -value;
  }
  while (value > base) {
    int remainder = value % base;
    value /= base;
    *s++ = '0' + remainder;
  }
  *s++ = '0' + value;
  *s = 0;
  reverse(buffer, s - buffer);
  return buffer;
}

unsigned char *videoram = (unsigned char *)0xb8000;

int xpos = 0;
int ypos = 0;

void new_line() {
  xpos = 0;
  ++ypos;
}

void print_character(char c) {
  switch (c) {
    case '\n':
      new_line();
      break;
    default:
      if (xpos > 80 * 2) {
        new_line();
      }
      videoram[ypos * 80 * 2 + xpos] = (int)c;
      videoram[ypos * 80 * 2 + xpos + 1] = 0x07;
      xpos += 2;
  }
}

void print_string(char *s) {
  while (*s != 0) {
    print_character(*s);
    s++;
  }
}

void print_integer(int d) {
  char number_buffer[11];
  itoa(d, number_buffer, 10);
  print_string(number_buffer);
}

void cls() {
  int i = 0;
  for (i = 0; i < 80 * 25 * 2; ++i) {
    videoram[i] = 0;
  }
}

void print_warning(char *s) {
  print_string("Warning: ");
  print_string(s);
}

void print_error(char *s) {
  print_string("Error: ");
  print_string(s);
}

int printf(const char *format, ...) {
  va_list args;
  int d;
  char c;
  char *s;
  va_start(args, format);
  while (*format != 0) {
    char c = *format;
    if (c == '%') {
      c = *++format;
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
          s = va_arg(args, char *);
          print_string(s);
          break;
      }
    } else {
      print_character(c);
    }
    ++format;
  }
  va_end(args);
}

/* available colors

   0:black, 1:blue, 2:green, 3:cyan, 4:red,
   5:magenta, 6:brown, 7:light grey, 8:dark grey,
   9:light blue, 10:light green, 11:light cyan,
   12:light red, 13:light magneta, 14: light brown, 15: white
*/

typedef uint64_t uint64;
typedef int64_t int64;
//typedef unsigned int uint32;
typedef uint32_t uint32;
//typedef int int32;
typedef int32_t int32;
//typedef unsigned short uint16;
typedef uint16_t uint16;
//typedef short int16;
typedef int16_t int16;
//typedef unsigned char uint8;
typedef uint8_t uint8;
//typedef char int8;
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

//#IFDEF __x86_64__
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

extern void gdt_update(gdt_ptr_t *);
extern void idt_update(idt_ptr_t *);

void gdt_setup();
void gdt_set_entry(uint32, uint32, uint32, uint8, uint8);
void gdt_set_gate(uint32 entry, uint32 base, uint32 limit, uint8 access,
                  uint8 flags);

void idt_setup();
//void idt_set_entry(uint32, uint32, uint16, uint8);
void idt_set_gate(uint32 interrupt, uint64 offset, uint16 selector,
                  uint8 type_attr);

struct interrupt_registers_struct {
  uint32 ds;                                     // Data segment selector
  uint32 edi, esi, ebp, esp, ebx, edx, ecx, eax; // Pushed by pusha.
  uint32 int_no, err_code; // Interrupt number and error code (if applicable)
  uint32 eip, cs, eflags, useresp, ss; // Pushed by the processor automatically.
} __attribute__((packed));
typedef struct registers_struct interrupt_registers_t;

extern isr0;            // divide error, no error code, fault
extern void isr3(void); // breakpoint, no error code, trap
extern void isr4(void); // overflow, no error code, trap
extern void isr8(void);
extern isr12; // stack-segment fault, error code, fault
extern void isr13(void); // general protection fault, error code, fault
extern isr14; // page fault, error code, fault
extern void isr32(void);

struct interrupt_registers {
  //  uint32 ds;                                     // data segment selector
  // uint32 edi, esi, ebp, esp, ebx, edx, ecx, eax; // pushed by pushad
  uint64 int_no, err_code;
  uint64 eip, cs, eflags, rsp, ss; // pushed by cpu after interrupt
} __attribute__((packed));

// regs is passed via rdi
void interrupt_handler(struct interrupt_registers *regs) {
  print_string("interrupt\n");
  printf("eflags: %d\n", regs->eflags);
  printf("ss: %d\n", regs->ss);
  printf("rsp: %d\n", regs->rsp);
  printf("cs: %d\n", regs->cs);
  printf("eip: %d\n", regs->eip);
  printf("interrupt number: %d\n", regs->int_no);
  printf("error code: %d\n", regs->err_code);
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
  idt_set_gate(14, 0, 0x08, 0x0E);
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
  //  idt_set_gate(255, 0, 0x08, 0x0E);

  idt_update(&idt);
}

void idt_set_gate(uint32 interrupt, uint64 offset, uint16 selector,
                  uint8 type_attr) {
  // first 16 bits
  idt_entries[interrupt].offset_start = offset; //(offset & 0xFFFF);
  // next 16 bits
  idt_entries[interrupt].offset_mid = offset >> 16;// & 0xFFFF;
  // last 32 bits
  idt_entries[interrupt].offset_end = offset >> 32;// & 0xFFFFFFFF;

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
  gdt_set_gate(1, 0, 0xffffffff, 0x9a, 0xcf); // kernel mode code segment
  gdt_set_gate(2, 0, 0xffffffff, 0x92, 0xcf); // kernel mode data segment
  gdt_set_gate(3, 0, 0xffffffff, 0xfa, 0xcf); // user mode code segment
  gdt_set_gate(4, 0, 0xffffffff, 0xf2, 0xcf); // user mode data segment
  gdt_set_gate(5, (uint32)&tss, sizeof(tss), 0x89,
               0x40); // cpu1 task switching segment

  gdt_update(&gdt);
}

void gdt_set_gate(uint32 entry, uint32 base, uint32 limit, uint8 access,
                  uint8 flags) {
  gdt_entries[entry].base_start = (0xffff & base);
  gdt_entries[entry].base_middle = (base >> 16) & 0xff;
  gdt_entries[entry].base_end = (base >> 24) & 0xff;

  gdt_entries[entry].limit_start = (0xffff & limit);
  gdt_entries[entry].limit_and_flags = (limit >> 16) & 0x0f;
  gdt_entries[entry].limit_and_flags |= (flags & 0xf0);

  gdt_entries[entry].access = access;
}

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

// I/O ports
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
  unsigned char a1, a2;

  a1 = inb(PIC1_DATA); // save masks
  a2 = inb(PIC2_DATA);

  outb(PIC1_COMMAND,
       ICW1_INIT + ICW1_ICW4); // starts the initialization sequence
  outb(PIC2_COMMAND, ICW1_INIT + ICW1_ICW4);

  outb(PIC1_DATA, offset1); // define the PIC vectors
  outb(PIC2_DATA, offset2);

  outb(PIC1_DATA, 4); // continue initialization sequence
  outb(PIC2_DATA, 2);

  outb(PIC1_DATA, ICW4_8086);
  outb(PIC2_DATA, ICW4_8086);

  outb(PIC1_DATA, a1); // restore saved masks.
  outb(PIC2_DATA, a2);
}

struct apic_registers {
  uint64_t reserve_1[4];
  uint64_t local_apic_id[2];
  uint64_t local_apic_version[2];
  uint64_t reserve_2[8];
  uint64_t task_priority;
  uint64_t ignore[4];
  uint64_t local_destination;
  uint64_t spurious_interrupt_vector;

} __attribute__((packed));
typedef struct apic_registers apic_registers_t;

void apic_setup() {
  apic_registers_t *regs = (apic_registers_t*)0xFEE00000;
  // print as hex?
  printf("apic id: %d\n", regs->local_apic_version);

  // TODO
  // set up spurious interrupt
  // remap & disable pic
  // enable apic
  // configure keyboard IRQ on ioapic
}

// TODO: use timer to print clock

/*
  00077690286d[CPU0  ] LONG MODE IRET
00077690286d[CPU0  ] LONG MODE INTERRUPT RETURN TO OUTER PRIVILEGE LEVEL or 64 BIT MODE
00077690295d[CPU0  ] page walk for address 0x00000000fee00010
00077690295d[CPU0  ] PAE  PDPE: entry not present
00077690295d[CPU0  ] page fault for address 00000000fee00010 @ 0000000000100e32
00077690295d[CPU0  ] exception(0x0e): error_code=0000
00077690295d[CPU0  ] interrupt(): vector = 0e, TYPE = 3, EXT = 1
00077690295e[CPU0  ] interrupt(long mode): gate.p == 0
00077690295d[CPU0  ] exception(0x0b): error_code=0073
00077690295d[CPU0  ] exception(0x08): error_code=0000
00077690295d[CPU0  ] interrupt(): vector = 08, TYPE = 3, EXT = 1
00077690295d[CPU0  ] interrupt(long mode): INTERRUPT TO SAME PRIVILEGE
00077696878d[CPU0  ] LONG MODE IRET
00077696878d[CPU0  ] LONG MODE INTERRUPT RETURN TO OUTER PRIVILEGE LEVEL or 64 BIT MODE
00077696879d[CPU0  ] page walk for address 0x00000000fee00010
00077696879d[CPU0  ] PAE  PDPE: entry not present
00077696879d[CPU0  ] page fault for address 00000000fee00010 @ 0000000000100e32
00077696879d[CPU0  ] exception(0x0e): error_code=0000
00077696879d[CPU0  ] interrupt(): vector = 0e, TYPE = 3, EXT = 1
00077696879e[CPU0  ] interrupt(long mode): gate.p == 0
00077696879d[CPU0  ] exception(0x0b): error_code=0073
00077696879d[CPU0  ] exception(0x08): error_code=0000
00077696879d[CPU0  ] interrupt(): vector = 08, TYPE = 3, EXT = 1
00077696879d[CPU0  ] interrupt(long mode): INTERRUPT TO SAME PRIVILEGE
00077703462d[CPU0  ] LONG MODE IRET
00077703462d[CPU0  ] LONG MODE INTERRUPT RETURN TO OUTER PRIVILEGE LEVEL or 64 BIT MODE
00077703463d[CPU0  ] page walk for address 0x00000000fee00010
00077703463d[CPU0  ] PAE  PDPE: entry not present
00077703463d[CPU0  ] page fault for address 00000000fee00010 @ 0000000000100e32
00077703463d[CPU0  ] exception(0x0e): error_code=0000
00077703463d[CPU0  ] interrupt(): vector = 0e, TYPE = 3, EXT = 1
00077703463e[CPU0  ] interrupt(long mode): gate.p == 0
00077703463d[CPU0  ] exception(0x0b): error_code=0073
00077703463d[CPU0  ] exception(0x08): error_code=0000
00077703463d[CPU0  ] interrupt(): vector = 08, TYPE = 3, EXT = 1
00077703463d[CPU0  ] interrupt(long mode): INTERRUPT TO SAME PRIVILEGE

*/

void kmain(void *mbd, unsigned int magic) {
  if (magic != 0x2BADB002) {
    /* Something went not according to specs. Print an error */
    /* message and halt, but do *not* rely on the multiboot */
    /* data structure. */
  }

  /* You could either use multiboot.h */
  /* (http://www.gnu.org/software/grub/manual/multiboot/multiboot.html#multiboot_002eh)
   */
  /* or do your offsets yourself. The following is merely an example. */
  char *boot_loader_name = (char *)((long *)mbd)[16];

  /* Write your kernel here. */
  /* gdt_setup(); */
   idt_setup();
  /* pic_remap(20, 28); */

  cls();

  /* char test[10]; */
  /* itoa(123, test, 10); */
  /* print_string(test); */

  /* itoa(-123, test, 10); */
  /* print_string(test); */

  /* printf("\nstring: %s\nchar: %c\npositive integer: %d\nnegative integer: %d\n", */
  /*        "test", 'c', 123, -123); */

  /* print_string("hello world\nneue Zeile\nnoch eine neue Zeile\nscheint zu " */
  /*              "gehen\n\n\n\n4 neue zeilen"); */

  __asm__ volatile("int $0x3");

  apic_setup();
  return 0xDEADBABA;
}

/* // Note: I found using bitfields strongly discouraged, but I will still try to */
/* // use them. ref: https://news.ycombinator.com/item?id=17056301 and many more. */

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
/* typedef page_directory_pointer_table_entry page_directory_pointer_table_entry_t; */

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


/* // Each paging table has 512 entries of size 8 bytes on a 64bit architecture. */
/* // Section 4.2 in Intel Developer Manual. */
/* // Root paging structure needs to be put into CR3. */
/* // We will do 4 level paging. Section 4.5. */
/* // To enable we have to set */
/* // CR0.PG = 1, CR4.PAE = 1, IA32_EFER.LME = 1, and CR4.LA57 = 0. */
/* // Paging maps linear address space to physical address space. */
/* // PML4[PML4Entries] -> Page Directory Pointer Table[PDPTEntries] -> 1Gb page | Page Directory */
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
/* // https://stackoverflow.com/questions/49473061/linking-32-and-64-bit-code-together-into-a-single-binary */
