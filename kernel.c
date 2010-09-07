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

struct gdt_entry
{
    uint16 limit_start;
    uint16 base_start;
    uint8 base_middle;
    uint8 access;
    uint8 limit_and_flags;
    uint8 base_end;
} __attribute__((packed));

struct gdt_ptr
{
    uint16 limit;
    uint32 base;
} __attribute__((packed));

void setup_gdt() 
{
    gdt.limit = sizeof(gdt_entry) * 5 - 1;
    gdt.base = &gdt_entries;
}

void write_gdt_entry(gdt_entry &entry, uint32 base, uint32 limit, uint8 access, uint8 flags) 
{
    entry.base_start = (0xffff & base);
    entry.base_middle = (base >> 16) & 0xff;
    entry.base_end = (base >> 32) & 0xff;

    entry.limit_start = (0xffff & limit);
    entry.limit_and_flags = (limit >> 16) & 0x0f;
    entry.limit_and_flags |= (flags & 0xf0);
}

gdt_ptr gdt;
gdt_entry gdt_entries[5];

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
