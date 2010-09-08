unsigned char *videoram = (unsigned char*)0xb8000;

void print_character(char c) 
{
    videoram[0] = (int)c;
    videoram[1] = 0x07;
}

void print_string(char *s)
{
    while(*s++ != '\n')
    {
        print_character(*s);
    }
}

void cls() 
{
    for (int i = 0; i < 80*25*2; ++i) 
    {
        videoram[i] = 0;
    }
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

gdt_ptr_t gdt;
gdt_entry_t gdt_entries[6];
tss_t tss;

void gdt_setup()
{
    gdt.limit = sizeof(gdt_entry) * 5 - 1;
    gdt.base = &gdt_entries;

    write_gdt_entry(gdt_entries[0], 0, 0, 0, 0);
    write_gdt_entry(gdt_entries[1], 0, 0xffffffff, 0x9a, 0xcf); // kernel mode code segment
    write_gdt_entry(gdt_entries[2], 0, 0xffffffff, 0x92, 0xcf); // kernel mode data segment 
    write_gdt_entry(gdt_entries[3], 0, 0xffffffff, 0xfa, 0xcf); // user mode code segment
    write_gdt_entry(gdt_entries[4], 0, 0xffffffff, 0xf2, 0xcf); // user mode data segment
    write_gdt_entry(gdt_entries[5], &tss, sizeof(tss), 0x89, 0x40); // cpu1 task switching segment
}

void gdt_write_entry(gdt_entry &entry, uint32 base, uint32 limit, uint8 access, uint8 flags) 
{
    entry.base_start = (0xffff & base);
    entry.base_middle = (base >> 16) & 0xff;
    entry.base_end = (base >> 32) & 0xff;

    entry.limit_start = (0xffff & limit);
    entry.limit_and_flags = (limit >> 16) & 0x0f;
    entry.limit_and_flags |= (flags & 0xf0);
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
  
    /* Print a letter to screen to see everything is working: */
    unsigned char *videoram = (unsigned char*)0xb8000;
    videoram[0] = 65; /* character 'A' */
    videoram[1] = 0x07; /* forground, background color. */
  
    /* Write your kernel here. */
  
    //   return 0xDEADBABA;
}
