unsigned char *videoram = (unsigned char*)0xb8000;

int xpos = 0;
int ypos = 0;

void print_character(char c)
{
    videoram[ypos*80*2+xpos] = (int)c;
    videoram[ypos*80*2+xpos+1] = 0x07;
    xpos += 2;

    if (c == '\n' || xpos > 80*2)
    {
        xpos = 0;
        ++ypos;
    }
}

void print_string(char *s)
{
    while(*s != 0)
    {
        print_character(*s);
        s++;
    }
}

void cls()
{
    int i = 0;
    for (i = 0; i < 80*25*2; ++i)
    {
        videoram[i] = 0;
    }
}

void print_warning(char *s)
{
    print_string("Warning: ");
    print_string(s);
}

void print_error(char *s)
{
    print_string("Error: ");
    print_string(s);
}

/* available colors

0:black, 1:blue, 2:green, 3:cyan, 4:red,
5:magenta, 6:brown, 7:light grey, 8:dark grey,
9:light blue, 10:light green, 11:light cyan,
12:light red, 13:light magneta, 14: light brown, 15: white
*/

typedef unsigned int uint32;
typedef int int32;
typedef unsigned short uint16;
typedef short int16;
typedef unsigned char uint8;
typedef char int8;

/* task-state segment */
struct tss_struct
{
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

struct gdt_entry_struct
{
    uint16 limit_start;
    uint16 base_start;
    uint8 base_middle;
    uint8 access;
    uint8 limit_and_flags;
    uint8 base_end;
} __attribute__((packed));
typedef struct gdt_entry_struct gdt_entry_t;

struct gdt_ptr_struct
{
    uint16 limit;
    uint32 base;
} __attribute__((packed));
typedef struct gdt_ptr_struct gdt_ptr_t;

struct idt_entry_struct
{
    uint16 offset_start;
    uint16 selector;
    uint8 zero;
    uint8 type_attr;
    uint16 offset_end;
} __attribute__((packed));
typedef struct idt_entry_struct idt_entry_t;

struct idt_ptr_struct
{
    uint16 limit;
    uint32 base;
} __attribute__((packed));
typedef struct idt_ptr_struct idt_ptr_t;

gdt_ptr_t gdt;
gdt_entry_t gdt_entries[6];

idt_ptr_t idt;
idt_entry_t idt_entries[256];

tss_t tss;

extern void gdt_update(gdt_ptr_t*);
extern void idt_update(idt_ptr_t*);

void gdt_setup();
void gdt_set_entry(uint32, uint32, uint32, uint8, uint8);
void gdt_set_gate(uint32 entry, uint32 base, uint32 limit, uint8 access, uint8 flags);

void idt_setup();
void idt_set_entry(uint32, uint32, uint16, uint8);
void idt_set_gate(uint32 interrupt, uint32 offset, uint16 selector, uint8 type_attr);

struct interrupt_registers_struct
{
    uint32 ds; // Data segment selector
    uint32 edi, esi, ebp, esp, ebx, edx, ecx, eax; // Pushed by pusha.
    uint32 int_no, err_code; // Interrupt number and error code (if applicable)
    uint32 eip, cs, eflags, useresp, ss; // Pushed by the processor automatically.
} __attribute__((packed));
typedef struct registers_struct interrupt_registers_t;

extern isr0; // divide error, no error code, fault
extern void isr3(void); // breakpoint, no error code, trap
extern void isr4(void); // overflow, no error code, trap
extern isr12; // stack-segment fault, error code, fault
extern isr13; // general protection fault, error code, fault
extern isr14; // page fault, error code, fault

void interrupt_handler()
{
    print_string("interrupt\n");
}

void idt_setup()
{
    idt.limit = sizeof(idt_entry_t) * 256 - 1;
    idt.base = (uint32)&idt_entries;

    idt_set_gate(0, 0, 0x08, 0x0E);
    idt_set_gate(1, 0, 0x08, 0x0E);
    idt_set_gate(2, 0, 0x08, 0x0E);
    idt_set_gate(3, isr3, 0x08, 0x8E);
    idt_set_gate(4, isr4, 0x08, 0x8E);
    idt_set_gate(5, 0, 0x08, 0x0E);
    idt_set_gate(6, 0, 0x08, 0x0E);
    idt_set_gate(7, 0, 0x08, 0x0E);
    idt_set_gate(8, 0, 0x08, 0x0E);
    idt_set_gate(9, 0, 0x08, 0x0E);
    idt_set_gate(10, 0, 0x08, 0x0E);
    idt_set_gate(11, 0, 0x08, 0x0E);
    idt_set_gate(12, 0, 0x08, 0x0E);
    idt_set_gate(13, 0, 0x08, 0x0E);
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

    idt_update(&idt);
}

void idt_set_gate(uint32 interrupt, uint32 offset, uint16 selector, uint8 type_attr)
{
    idt_entries[interrupt].offset_start = (offset & 0xff);
    idt_entries[interrupt].offset_end = (offset >> 16) & 0xff;

    idt_entries[interrupt].selector = selector;

    idt_entries[interrupt].type_attr = type_attr;
}

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

void gdt_setup()
{
    gdt.limit = sizeof(gdt_entry_t) * 5 - 1;
    gdt.base = (uint32)&gdt_entries;

    gdt_set_gate(0, 0, 0, 0, 0);
    gdt_set_gate(1, 0, 0xffffffff, 0x9a, 0xcf); // kernel mode code segment
    gdt_set_gate(2, 0, 0xffffffff, 0x92, 0xcf); // kernel mode data segment
    gdt_set_gate(3, 0, 0xffffffff, 0xfa, 0xcf); // user mode code segment
    gdt_set_gate(4, 0, 0xffffffff, 0xf2, 0xcf); // user mode data segment
    gdt_set_gate(5, (uint32)&tss, sizeof(tss), 0x89, 0x40); // cpu1 task switching segment

    gdt_update(&gdt);
}

void gdt_set_gate(uint32 entry, uint32 base, uint32 limit, uint8 access, uint8 flags)
{
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
  asm volatile ( "outb %0, %1" : : "a"(val), "Nd"(port) );
  /* There's an outb %al, $imm8  encoding, for compile-time constant port numbers that fit in 8b.  (N constraint).
   * Wider immediate constants would be truncated at assemble-time (e.g. "i" constraint).
   * The  outb  %al, %dx  encoding is the only option for all other cases.
   * %1 expands to %dx because  port  is a uint16_t.  %w1 could be used if we had the port number a wider C type */
}

static inline uint8 inb(uint16 port) {
    uint8 ret;
    asm volatile ( "inb %1, %0"
                   : "=a"(ret)
                   : "Nd"(port) );
    return ret;
}

// I/O ports
#define PIC1 0x20
#define PIC2 0xA0
#define PIC1_COMMAND PIC1
#define PIC1_DATA (PIC1+1)
#define PIC2_COMMAND PIC2
#define PIC2_DATA (PIC2+1)

#define PIC_END_OF_INTERRUPT 0x20

#define ICW1_ICW4 0x01		/* ICW4 (not) needed */
#define ICW1_SINGLE	0x02		/* Single (cascade) mode */
#define ICW1_INTERVAL4 0x04		/* Call address interval 4 (8) */
#define ICW1_LEVEL 0x08		/* Level triggered (edge) mode */
#define ICW1_INIT 0x10		/* Initialization - required! */

#define ICW4_8086 0x01		/* 8086/88 (MCS-80/85) mode */
#define ICW4_AUTO 0x02		/* Auto (normal) EOI */
#define ICW4_BUF_SLAVE 0x08		/* Buffered mode/slave */
#define ICW4_BUF_MASTER	0x0C		/* Buffered mode/master */
#define ICW4_SFNM 0x10		/* Special fully nested (not) */

/*
arguments:
    offset1 - vector offset for master PIC
        vectors on the master become offset1..offset1+7
    offset2 - same for slave PIC: offset2..offset2+7
*/
void pic_remap(int offset1, int offset2)
{
    unsigned char a1, a2;

    a1 = inb(PIC1_DATA);                        // save masks
    a2 = inb(PIC2_DATA);

    outb(PIC1_COMMAND, ICW1_INIT+ICW1_ICW4);  // starts the initialization sequence
    outb(PIC2_COMMAND, ICW1_INIT+ICW1_ICW4);

    outb(PIC1_DATA, offset1);                 // define the PIC vectors
    outb(PIC2_DATA, offset2);

    outb(PIC1_DATA, 4);                       // continue initialization sequence
    outb(PIC2_DATA, 2);

    outb(PIC1_DATA, ICW4_8086);
    outb(PIC2_DATA, ICW4_8086);

    outb(PIC1_DATA, a1);   // restore saved masks.
    outb(PIC2_DATA, a2);
}

void pic_end_of_interrupt()
{

}

void kmain(void* mbd, unsigned int magic)
{
    if (magic != 0x2BADB002)
    {
        /* Something went not according to specs. Print an error */
        /* message and halt, but do *not* rely on the multiboot */
        /* data structure. */
    }

    /* You could either use multiboot.h */
    /* (http://www.gnu.org/software/grub/manual/multiboot/multiboot.html#multiboot_002eh) */
    /* or do your offsets yourself. The following is merely an example. */
    char *boot_loader_name = (char*)((long*)mbd)[16];

    /* Write your kernel here. */
    gdt_setup();
    idt_setup();
    pic_remap(20, 28);

    /* Print a letter to screen to see everything is working: */
    cls();
    print_string("hello world\nneue Zeile\nnoch eine neue Zeile\nscheint zu gehen\n\n\n\n4 neue zeilen");
    //   return 0xDEADBABA;
}
