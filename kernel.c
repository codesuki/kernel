#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;
typedef uint16_t u16;
typedef int16_t s16;
typedef uint8_t u8;
typedef int8_t s8;

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
u8* videoram = (u8*)0xb8000;

int xpos = 0;
int ypos = 0;

int mouse_x = 0;
int mouse_y = 0;

// How can we improve this?
// Have a ring buffer that holds N pages 25*80
// How do we write to it? Printf should do what?
// Just write to that buffer instead of the video ram.
// Write that buffer to video ram, could be optimized, but not needed, yet.
// Have a pointer/"cursor" that says which line is the top/bottom line on the
// screen. Can control the pointer with arrow keys to scroll up and down.

// 2 is because every character has the character and a color/format.
#define num_pages 3
#define num_rows 25
#define num_cols 80
#define terminal_buffer_size (num_cols * num_rows * 2 * num_pages)
u8 terminal_buffer[num_cols * num_rows * 2 * num_pages] = {0};  // 100 pages

// ring buffer start. once we are at the end it needs to wrap. should be aligned
// to start of line I guess.
u16 terminal_buffer_start = 0;
u16 terminal_buffer_row_index = 0;
u16 terminal_buffer_column_index = 0;
u16 terminal_cursor_index = 0;

int printf(const char* format, ...);

// We need to wrap around and look at the end of the ring buffer if we are on
// the first screen. This is the adjusted_cursor_index.
void display() {
  for (int y = 0; y < num_rows; y++) {
    for (int x = 0; x < num_cols; x++) {
      if (x == mouse_x && y == mouse_y) {
	int idx = y * num_cols * 2 + x * 2;
	videoram[idx] = 'o';
	videoram[idx + 1] = 0x4;
	continue;
      }
      // contract: at minimum we want to start at terminal_buffer_start default
      // is cursor locked at last line so we want to subtract 25 lines, but only
      // if we are over 25.
      bool is_after_first_page = terminal_cursor_index > 24;
      int adjusted_cursor_index =
	  is_after_first_page * (terminal_cursor_index - 24) +
	  (1 - is_after_first_page) *
	      (num_rows * num_pages - num_rows + terminal_cursor_index);
      int terminal_buffer_offset =
	  terminal_buffer_start + adjusted_cursor_index * num_cols * 2;
      int idx = y * num_cols * 2 + x * 2;
      videoram[idx] = terminal_buffer[(terminal_buffer_offset + idx) %
				      terminal_buffer_size];
      videoram[idx + 1] = terminal_buffer[(terminal_buffer_offset + idx + 1) %
					  terminal_buffer_size];
    }
  }
}

void clear_page() {
  for (u32 i = 0; i < num_rows * num_cols * 2; i++) {
    u32 idx =
	(terminal_buffer_row_index * num_cols * 2 + i) % terminal_buffer_size;
    terminal_buffer[idx] = 0;
  }
  // printf("cleared from %d to %d\n", terminal_buffer_row_index,
  // terminal_buffer_row_index + num_rows );
}

void new_line() {
  // reset to first column
  terminal_buffer_column_index = 0;
  // one line down
  // printf("%d/%d L%d:", terminal_buffer_row_index/num_rows, num_pages,
  // terminal_buffer_row_index);

  terminal_buffer_row_index =
      (terminal_buffer_row_index + 1) % (num_rows * num_pages);
  if (terminal_buffer_row_index % num_rows == 0) {
    clear_page();
  }
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
      // Possible integer overrun
      u16 idx = terminal_buffer_row_index * num_cols * 2 +
		terminal_buffer_column_index * 2;
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

struct gdt_entry_struct {
  u16 limit_start;
  u16 base_start;
  u8 base_middle;
  u8 access;
  u8 limit_and_flags;
  u8 base_end;
} __attribute__((packed));
typedef struct gdt_entry_struct gdt_entry_t;

struct gdt_ptr_struct {
  u16 limit;
  u32 base;
} __attribute__((packed));
typedef struct gdt_ptr_struct gdt_ptr_t;

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

gdt_ptr_t gdt;
gdt_entry_t gdt_entries[6];

idt_ptr_t idt;
idt_entry_t idt_entries[256] = {0};

tss_t tss;

extern void gdt_update(gdt_ptr_t*);
extern void idt_update(idt_ptr_t*);

void gdt_setup();
void gdt_set_entry(u32, u32, u32, u8, u8);
void gdt_set_gate(u32 entry, u32 base, u32 limit, u8 access, u8 flags);

void idt_setup();
// void idt_set_entry(uint32, uint32, uint16, uint8);
void idt_set_gate(u32 interrupt, u64 offset, u16 selector, u8 type_attr);

/*
  struct interrupt_registers_struct {
  uint32 ds;                                      // Data segment selector
  uint32 edi, esi, ebp, esp, ebx, edx, ecx, eax;  // Pushed by pusha.
  uint32 int_no, err_code;  // Interrupt number and error code (if applicable)
  uint32 eip, cs, eflags, useresp,
  ss;  // Pushed by the processor automatically.
  } __attribute__((packed));
  typedef struct interrupt_registers_struct interrupt_registers_t;
 */

extern int isr0;         // divide error, no error code, fault
extern void isr3(void);  // breakpoint, no error code, trap
extern void isr4(void);  // overflow, no error code, trap
extern void isr8(void);
extern int isr12;         // stack-segment fault, error code, fault
extern void isr13(void);  // general protection fault, error code, fault
extern void isr14(void);  // page fault, error code, fault
extern void isr32(void);
extern void isr0x31(void);
extern void isr0x32(void);
extern void isr0x33(void);
extern void isr0x34(void);

struct interrupt_registers {
  //  uint32 ds;                                     // data segment selector
  u64 r15, r14, r13, r12, r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx, rax;
  u64 int_no, err_code;
  u64 eip, cs, rflags, rsp, ss;  // pushed by cpu after interrupt
} __attribute__((packed));
typedef struct interrupt_registers interrupt_registers;

#define EXCEPTION_PAGE_FAULT 14

char* interrupt_names[22] = {
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "Double Fault Exception",  // 8
    "9",
    "10",
    "11",
    "12",
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

// assembler
static inline void outb(u16 port, u8 val) {
  __asm__ volatile("outb %0, %1" : : "a"(val), "Nd"(port));
  /* There's an outb %al, $imm8  encoding, for compile-time constant port
   * numbers that fit in 8b.  (N constraint).
   * Wider immediate constants would be truncated at assemble-time (e.g. "i"
   * constraint).
   * The  outb  %al, %dx  encoding is the only option for all other cases.
   * %1 expands to %dx because  port  is a u16.  %w1 could be used if
   * we
   * had the port number a wider C type */
}

static inline u8 inb(u16 port) {
  u8 ret;
  __asm__ volatile("inb %1, %0" : "=a"(ret) : "Nd"(port));
  return ret;
}

static inline void outl(u16 port, u32 val) {
  __asm__ volatile("outl %k0, %w1" : : "a"(val), "Nd"(port) : "memory");
}

static inline u32 inl(u16 port) {
  u32 ret;
  __asm__ volatile("inl %w1, %k0" : "=a"(ret) : "Nd"(port) : "memory");
  return ret;
}

static inline void outw(u16 port, u16 val) {
  __asm__ volatile("outw %w0, %w1" : : "a"(val), "Nd"(port) : "memory");
}

static inline u16 inw(u16 port) {
  u16 ret;
  __asm__ volatile("inw %w1, %w0" : "=a"(ret) : "Nd"(port) : "memory");
  return ret;
}

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

// TODO: optimize by copying bigger blocks at once.
void memcpy(void* src, void* dst, size_t len) {
  for (int i = 0; i < len; i++) {
    ((u8*)dst)[i] = ((u8*)src)[i];
  }
}

void memset(void* dst, u8 byte, size_t len) {
  for (size_t i = 0; i < len; i++) {
    ((u8*)dst)[i] = byte;
  }
}

#define RX_BUFFER_SIZE 8192
// 00 = 8k + 16 byte
// wrap bit = 1
//
// When set to 1: The RTL8139D(L) will keep moving the rest of the packet data
// into the memory immediately after the end of the Rx buffer, if this packet
// has not been completely moved into the Rx buffer and the transfer has arrived
// at the end of the Rx buffer. The software driver must reserve at least 1.5K
// bytes buffer to accept the remainder of the packet. We assume that the
// remainder of the packet is X bytes. The next packet will be moved into the
// memory from the X byte offset at the top of the Rx buffer.
u8 network_rx_buffer[RX_BUFFER_SIZE + 16 + 1500]
    __attribute__((aligned(4))) = {0};
u16 network_rx_buffer_index = {0};

u8 network_packet[1518] = {0};

u8 network_current_tx_descriptor = 0;

u16 num_packets = 0;

// Now it's starting to get super dirty.
u32 base = 0;

u8 mac[6] = {0};
u8 broadcast_mac[6] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
u8 ip[4] = {0};
u8 broadcast_ip[4] = {0xff, 0xff, 0xff, 0xff};

// How are IPs stored in memory?
// ip global
// 000000000041f0de: 0xc0 0xa8 0x0b 0x08
// ip_2 uint32 generated by this code
// ip[0] = 0xc0, ip[1] = 0xa8, ip[2] = 0x0b, ip[3] = 0x08
// ip_2 = (ip[3] << 24) | (ip[2] << 16) | (ip[1] << 8) | ip[0];//
// 000000000041f0e4: 0xc0 0xa8 0x0b 0x08
// x /4xb 0x41f0e4
// 000000000041f0e4: 0xc0 0xa8 0x0b 0x08
// (qemu) x /4xw 0x41f0e4
// 000000000041f0e4: 0x080ba8c0 0x010ba8c0 0x00ffffff 0x010ba8c0

// note: I was confused. The UDP checksum was wrong, because I mistakenly put
// the address fields down as u16. Because I noticed that the only thing
// that made it fail were the addresses I thought it had something to do with
// endianess. I wondered why the above resulted in the same memory layout, but
// taking a step back it became obvious. I laid out the little endian u32
// value ip_2 such that as a little endian value it would be laid out like the
// address in big endian. Not sure why I was confused :)

u8 dhcp_identifier[4];
u8 dhcp_subnet_mask[4];
u8 dhcp_router[4];
u8 dhcp_router_mac[6];
u8 dhcp_dns[4];
u32 dhcp_offer_xid;

u16 ntohs(u16 netshort) {
  return (netshort >> 8) | (netshort << 8);
}

struct ethernet_frame {
  u8 destination_mac[6];
  u8 source_mac[6];
  u16 ethertype;  // network byte order
  // 14
  //  payload
  //  crc
} __attribute__((packed));
typedef struct ethernet_frame ethernet_frame;

struct ipv4_header {
  u8 ihl : 4;
  u8 version : 4;
  u8 ecn : 2;   // congestion notification
  u8 dscp : 6;  // type of service. so routers can see what to prioritize
  // 15
  // minimum is 20 with is just the ipv4 header.
  u16 length;                // network byte order
  u16 identification;        // network byte order
  u16 fragment_offset : 13;  // network byte order
  u16 flags : 3;             // network byte order
  // 21
  u8 ttl;
  // 22
  // ref: https://en.wikipedia.org/wiki/List_of_IP_protocol_numbers
  u8 protocol;
  // 23
  u16 checksum;  // network byte order?
  // 25
  u32 source_address;  // network byte order
  // 29
  u32 destination_address;  // network byte order
  // 33
} __attribute__((packed));
typedef struct ipv4_header ipv4_header;

struct udp_header {
  u16 source_port;  // network byte order
  // 35
  u16 destination_port;  // network byte order
  // 37
  u16 length;    // network byte order
  u16 checksum;  // network byte order
} __attribute__((packed));
typedef struct udp_header udp_header;

// ref: http://www.faqs.org/rfcs/rfc768.html
struct udp_pseudo_ip_header {
  u32 source_address;       // network byte order
  u32 destination_address;  // network byte order
  u8 zero;
  u8 protocol;
  u16 udp_length;  // network byte order
} __attribute__((packed));
typedef struct udp_pseudo_ip_header udp_pseudo_ip_header;

struct dhcp_message {
  u8 op;
  u8 htype;
  u8 hlen;
  u8 hops;
  u32 xid;        // network byte order
  u16 secs;       // network byte order
  u16 flags;      // network byte order
  u32 ciaddr;     // network byte order
  u8 yiaddr[4];   // network byte order
  u32 siaddr;     // network byte order
  u32 giaddr;     // network byte order
  u32 chaddr[4];  // network byte order
  u8 reserved[192];
  u8 magic[4];
} __attribute__((packed));
typedef struct dhcp_message dhcp_message;

struct dns_header {
  u16 id;
  u8 rd : 1;  // recursion desired
  u8 tc : 1;
  u8 aa : 1;
  // standard query (0), inverse query (1), server status request (2)
  u8 opcode : 4;
  u8 qr : 1;  // query (0), response (1)

  // response code: no error (0), format error (1), server failure (2), name
  // error (3), not implemented (4), refused (5)
  u8 rcode : 4;
  u8 z : 3;
  u8 ra : 1;  // recusion available

  u16 qdcount;
  u16 ancount;
  u16 nscount;
  u16 arcount;
} __attribute__((packed));
typedef struct dns_header dns_header;

struct dns_question {
  u16 qname;  // this is actually flex. so could be pretty long.
  u16 qtype;
  u16 qclass;
} __attribute__((packed));
typedef struct dns_question dns_question;

struct dns_resource {
  u16 name;
  u16 type;
  u16 class;
  u16 ttl;
  u16 rdlength;
  u16 rdata;
} __attribute__((packed));
typedef struct dns_resource dns_resource;

struct arp_message {
  u16 htype;  // 1 = ethernet, 0x0800 IPv4
  u16 ptype;
  u8 hlen;
  u8 plen;  // Protocol length in octets. IPv4 = 4
  u16 oper;
  u16 sender_mac_1;
  u16 sender_mac_2;
  u16 sender_mac_3;
  u16 sender_protocol_address_1;
  u16 sender_protocol_address_2;
  u16 target_mac_1;
  u16 target_mac_2;
  u16 target_mac_3;
  u16 target_protocol_address_1;
  u16 target_protocol_address_2;
} __attribute__((packed));
typedef struct arp_message arp_message;

struct icmp_header {
  u8 type;  // 8 = echo, 0 = echo reply
  u8 code;  // 0 = echo(?)
  u16 checksum;
};
typedef struct icmp_header icmp_header;

struct icmp_echo_message {
  u16 identifier;
  u16 sequence_number;
};
typedef struct icmp_echo_message icmp_echo_message;

u16 htons(u16 hostshort) {
  return (hostshort >> 8) | (hostshort << 8);
}

u32 htonl(u32 hostlong) {
  return ((hostlong & 0x000000ff) << 24) | ((hostlong & 0x0000ff00) << 8) |
	 ((hostlong & 0x00ff0000) >> 8) | ((hostlong & 0xff000000) >> 24);
}

u32 htonl_bytes(u8 bytes[4]) {
  return (bytes[3] << 24) | (bytes[2] << 16) | (bytes[1] << 8) | bytes[0];
}

// To be the same as strncmp it should stop after finding a 0 character.
bool strncmp(char* s1, char* s2, int n) {
  for (int i = 0; i < n; i++) {
    if (s1[i] != s2[i]) {
      return false;
    }
  }
  return true;
}

bool memcmp(void* s1, void* s2, size_t n) {
  for (size_t i = 0; i < n; i++) {
    if (((u8*)s1)[i] != ((u8*)s2)[i]) {
      printf("i: %d s1: %d s2: %d\n", i, ((u8*)s1)[i], ((u8*)s2)[i]);
      return false;
    }
  }
  return true;
}

// source: https://datatracker.ietf.org/doc/html/rfc1071#section-4.1
u16 ipv4_checksum(u16* addr, u8 count) {
  /* Compute Internet Checksum for "count" bytes
   *         beginning at location "addr".
   */
  u32 sum = 0;

  while (count > 1) {
    /*  This is the inner loop */
    sum += *addr++;
    count -= 2;
  }

  /*  Add left-over byte, if any */
  if (count > 0) {
    sum += *(u8*)addr;
  }

  /*  Fold 32-bit sum to 16 bits */
  while (sum >> 16) {
    sum = (sum & 0xffff) + (sum >> 16);
  }

  return ~sum;
}

#define HPET_CONFIG_REG 0x10
#define HPET_BASE 0xfed00000
#define HPET_MAIN_COUNTER 0xf0

// TODO: read timer resolution from config.
// The main timer tick interval is 10000000 (1e+7) femto seconds, read from
// config. This equals 10 nanoseconds. 1000000 (1e+6) nanoseconds are 1
// millisecond.
const u64 _1ms = 100000;  // we need this many timer periods to have 1 ms
const u64 _1s = _1ms * 1000;

void set_timer0() {
  // main counter 0xf0
  u64* counter_value = (u64*)(HPET_BASE + HPET_MAIN_COUNTER);
  // TODO: crash because of large number
  // printf("hpet: counter: %d", *counter_value);

  // comparator timer 0 0x108
  // printf("hpet: setting timer to %x %d\n", _1s, _1s);
  u64* comparator_0 = (u64*)(HPET_BASE + 0x108);
  // TODO: how is wrapping handled? By GPE :D
  *comparator_0 = *counter_value + _1ms * 10;

  // TODO: this crashes when wrapping 64bit value. Probably formatting code
  // wrong.
  // printf("hpet: set timer to %x %d\n", *comparator_0, *comparator_0);
}

u64 get_global_timer_value() {
  return *(u64*)(HPET_BASE + HPET_MAIN_COUNTER);
}

void setup_hpet() {
  // Timer 0: 100h – 107h, Timer 1: 120h – 127h, Timer 2: 140h – 147h
  u32* available_interrupts = (u32*)(HPET_BASE + 0x104);
  printf("hpet: interrupts timer 0: %x\n", *available_interrupts);

  u32* timer_0 = (u32*)(HPET_BASE + 0x100);  // set bit 2 and maybe 9-13
  printf("hpet: configured interrupt: %x\n", ((*timer_0) >> 9) & 31);
  *timer_0 = (*timer_0) | (1 << 2) | (4 << 9);
  printf("hpet: configured interrupt: %x\n", ((*timer_0) >> 9) & 31);

  u32* timer_period = (u32*)(HPET_BASE + 0x4);
  printf("hpet: femto: %d\n", *timer_period);

  // main counter 0xf0
  u64* counter_value = (u64*)(HPET_BASE + HPET_MAIN_COUNTER);
  printf("hpet: counter: %d", *counter_value);

  // comparator timer 0 0x108
  printf("hpet: setting timer to %x %d\n", _1s, _1s);
  u64* comparator_0 = (u64*)(HPET_BASE + 0x108);
  *comparator_0 = _1s;
  printf("hpet: set timer to %x %d\n", *comparator_0, *comparator_0);

  // bit 0 is enable flag
  u32* c = (u32*)(HPET_BASE + HPET_CONFIG_REG);
  printf("hpet: config %x\n", *c);
  *c = (*c) | 0x1;
  printf("hpet: config %x\n", *c);
  printf("hpet: counter: %d\n", *counter_value);
}

struct mouse_data {
  u8 data;
  u8 x;
  u8 y;
};
typedef struct mouse_data mouse_data_t;

// Memory start
// Defined in linker script.
// Need to take the address, because the address is the value in this case.
// ref: https://sourceware.org/binutils/docs/ld/Source-Code-Reference.html
extern u64 _kernel_start;
extern u64 _kernel_end;

const u32 page_size = 0x200000;  // 2mb

typedef struct memory memory;
struct memory {
  u64 address;
  u64 size;  // 4kb, 2mb, etc.
  memory* next;
  memory* prev;
};

// Current thought, keep two lists. Free and used memory.
// When we malloc we take from the free list and put it into the used list.
// When we free we search the address in the used list and move it back.
// What other way is there to implement free?
memory* memory_free_first = NULL;
memory* memory_used_first = NULL;

// memory_init creates the first memorys in physical memory, because we cannot
// call malloc.
void memory_init(u64 base_address, u64 length) {
  // We don't want to touch the memory block from 0 - 1mb.
  // All BIOS things live there.
  if (base_address == 0) {
    return;
  }
  // This will make our math below go awry.
  if (length == 0) {
    return;
  }

  // Kernel size was off because it was doing address arithmetic. Casting to
  // u64 fixed it. This caused the memorys to be allocated on kernel
  // memory which caused memory corruption.
  u64 base_address_after_kernel =
      base_address + ((u64)&_kernel_end - (u64)&_kernel_start);

  printf("memory_init: available memory with base %x and length %x\n",
	 base_address, length);

  printf("memory_init: kernel located between %x - %x\n", &_kernel_start,
	 &_kernel_end);

  // TODO: I think this overcounts. I.e. the last page is too big for the
  // available physical memory.
  // Maybe for now we don't track the memorys themselves.
  // How many do we need?
  u64 page_count = length / page_size;
  u64 memory_size = page_count * sizeof(memory);

  // After our memorys
  // Aligned on page_size.
  u64 usable_memory =
      ((base_address_after_kernel + memory_size + (page_size - 1)) &
       ~(page_size - 1));

  printf("memory_init: %d 2mb pages available\n", page_count);
  printf("memory_init: usable memory starts from %x\n", usable_memory);

  memory* current = (memory*)base_address_after_kernel;
  memory* prev = NULL;

  memory_free_first = current;

  for (u64 i = 0; i < page_count; i++) {
    current->address = usable_memory;
    current->next = NULL;
    current->prev = prev;
    current->size = page_size;

    if (prev != NULL) {
      prev->next = current;
    }

    // Nice in Bochs because we only have 30mb there.
    // printf("memory_init: assigned %x\n", m->address);

    usable_memory += page_size;
    prev = current;
    current++;
  }
  for (memory* m = memory_free_first; m != NULL; m = m->next) {
    printf("memory_init: %x %x %x\n", m->address, m->next, m->prev);
  }
}

void memory_add(u64 address) {
  if (memory_free_first == NULL) {
    memory_used_first = NULL;
    // Ouch! We want a new memory. Now do we malloc one? Haha.. Maybe the
    // kernel needs to reserve some memory beforehand for some kind of
    // bootstrap?
    // So when we read available memory, we remove the kernel area.
    // Then we know the real start of the memory.
    // Then we can just do memory * m = start of memory;
    // And so on.
  }
}

memory* memory_remove() {
  if (memory_free_first == NULL) {
    return NULL;
  }
  memory* memory = memory_free_first;
  memory_free_first = memory->next;
  memory_free_first->prev = memory->prev;
  return memory;
}

// Note about memory management.
// There is physical memory in the system. We get its address ranges from Grub.
// Then there is virtual memory (enforced in 64bit mode).
// Page tables describe a mapping from physical to virtual memory.
// A page can be 1Gb, 2mb, 4kb, ...
// A task has it's own virtual memory. There doesn't exist enough physical
// memory to fill this. Just some parts are mapped in via the page table.
// If a task needs memory it needs to request pages to be mapped to its virtual
// address space. Afterwards it can use this memory for all the objects it
// creates. We see that task memory can only grow by page size increments
// although they may be bigger or smaller than what the task needs. So there
// needs to be some kind of memory management to efficiently utilize the memory
// the task received. This is what malloc does.
//
// What will we do here? Currently we just allocate complete pages which is why
// we immediately run out of memory. Now we want malloc to request and manage
// only as much memory as necessary.
//
// Currently we only have the kernel page table.
//
// If there is no memory allocated to the task, ask the kernel for some memory
// however much is necessary to fullfill the request, in page size increments.
// Malloc then will only utilize the part that the task really needs. I.e. the
// task requests 1 byte but the page size is 2mb. So malloc will reserve 1 byte
// of memory. Malloc could keep a list of memory it gave out with the address as
// key. When we want to free memory we look into this list. This list also needs
// to live somewhere so we need to account for the size of this list when we
// request memory.
//
// Where should this structure live? It can be a list of free / used memory.
// Searching the list seems slow. O(n). Could be a hash map for O(1) lookup.
// Could be a tree for binary search. (What's the O? something log).
//
// When the task requests memory that memory needs to be contiguous.
//
// Interesting thought: Do pages need to be contiguous? I don't think so as long
// as they are mapped contiguously into the virtual address space. This is why
// alignment is so important. TODO: flesh this out.
//
// All too complicated. I will go with a free list again. On free we add blocks
// back and try to merge if possible. Fixed size 2mb heap.
//
// In the beginning we don't have memory again, so similarly to the free pages
// list we create a free list for malloc. The free pages I pre-allocated but
// with malloc that won't be possible.
//
// We can't know how big the list would be up front. We can do a worst case
// analysis. If the task requests 1 byte increments we create the biggest
// overhead because there is one structure allocated for 1 byte. Of course we
// could use as much as we have physical memory.
//
// I'll reuse the headers of the allocated memory that are used for freeing as
// free list structures.

typedef struct memory_header memory_header;
struct memory_header {
  u64 size;
  memory_header* next;
};

memory_header* malloc_free_list = nullptr;

void* malloc(u64 size) {
  if (malloc_free_list == nullptr) {
    // Reserve a 2mb page.
    memory* m = memory_remove();
    // TODO: all these printfs should be converted to a debug logger.
    // printf("malloc: received %d bytes from kernel\n", m->size);
    memory_header* h = (memory_header*)(m->address);
    h->size = m->size;
    h->next = NULL;
    malloc_free_list = h;
  }

  u64 actual_size = size + sizeof(memory_header) + sizeof(memory_header);
  // memory_header is 16 bytes. Times 2 is 32 bytes. If we allocate 1 byte we
  // have 32 bytes overhead.
  // printf("malloc: allocating %d bytes for the requested %d bytes\n",
  //	 actual_size, size);

  // First fit
  memory_header* m = malloc_free_list;
  memory_header* prev = nullptr;
  while (m != nullptr) {
    // Not big enough
    if (m->size < actual_size) {
      prev = m;
      m = m->next;
      continue;
    }

    // Positive cases. Perfect match or too big. In case it's too big we need to
    // chop it up. Maybe we can merge the cases by passing the perfect size to
    // the chop function which results in a no-op.
    if (m->size == actual_size) {
      u64 address = (u64)m;
      // printf("malloc2 perfect: %x\n", (void*)(address));

      // TODO: we don't need to do this anymore. It's the same type.
      memory_header* h = (memory_header*)address;
      h->size = actual_size;

      // TODO: I am not removing this from the free list so I expect it to be
      // used twice. I.e. log will show perfect twice.
      // Yes this happened.
      // Remove it from the free list.
      if (m == malloc_free_list) {
	malloc_free_list = m->next;
      } else {
	// We should have prev
	prev->next = m->next;
      }

      // printf("malloc: current free list\n");
      // for (memory_header* m = malloc_free_list; m != nullptr; m = m->next)
      // {	printf("malloc: %x %d\n", m, m->size);
      // }

      return (void*)(address + sizeof(memory_header));
    } else {
      // We need to chop it up. Because the address is the address of the
      // struct, it needs to move.

      // Naively we would:
      // Move the current header after the memory we allocate.
      // Reduce the size in the header by the size of the allocation
      // (actual_size).
      // Update the free list's previous items next pointer to
      // the new header. Allocate the space with header.

      // IMPORTANT: It was bigger than actual_size, but we don't know if it was
      // big enough to fit another memory_header into it. We can check, if
      // it's not big enough we could merge with the next slot if it's free, but
      // there is no guarantee for that. If it's not free we are out of luck.
      // Maybe best is to check if it's big enough. Worst case we write a header
      // for a 0 byte block. At least we can use it when merging. Alternatively
      // we overallocate and set the size of the actual allocation to the max
      // possible. This way we save one hop when iterating over the free list.

      // For now I went with the 0 size element by changing the continue
      // condition above.

      u64 address = (u64)m;

      // printf("malloc: found %d bytes slot to chop at %x\n", m->size,
      // address);

      memory_header* moved_m = (memory_header*)(address + actual_size);
      moved_m->size = m->size - actual_size;
      moved_m->next = m->next;

      // If we are moving the actual first element, update it.
      if (m == malloc_free_list) {
	malloc_free_list = moved_m;
      } else {
	// We should have prev
	prev->next = moved_m;
      }

      // Allocate the space with a header and return the address after the
      // header.
      memory_header* h = (memory_header*)address;
      h->size = actual_size;

      // printf("malloc: current free list\n");
      // for (memory_header* m = malloc_free_list; m != nullptr; m = m->next)
      // {	printf("malloc: %x %d\n", m, m->size);
      // }

      return (void*)(address + sizeof(memory_header));
    }
  }

  // If we came this far we didn't find free memory.
  // printf("malloc2: OOM\n");
  return nullptr;
}

void free(void* memory) {
  // All we know currently is the address. We stored a struct that contains the
  // size before the address.
  memory_header* h = (memory_header*)memory - 1;
  // printf("free: %d\n", h->size);

  // Put it back into the free list.
  // Sort it by address so that merging is easier.
  // Hunch: I only have to check prev and next to see if they are mergable.

  u64 address = (u64)h;

  memory_header* current = malloc_free_list;
  memory_header* prev = nullptr;
  while (current != nullptr) {
    // TODO: having the address in here is waste because we already know it.
    if ((u64)current < (u64)address) {
      prev = current;
      current = current->next;
      continue;
    }
    break;
  }

  // What is this state?
  if (current == nullptr) {
  } else {
    // Where do I get that object from? The memory. Do I create this again
    // inside the free memory? I could call malloc now, but then I would have
    // some overhead from the header. Can I reuse the header somehow?
    // First thought: but then I can't free the header.
    // Second thought: doesn't matter because it will just become a header again
    // anyway.
    // It will be cheaper if I re-use the headers because otherwise I write down
    // the size of the thing again.

    // I don't even need a pointer in them because I can calculate it. I how how
    // far the next item is away. But I cannot decide between free and used
    // memory this way. I could certainly find the next header but wouldn't know
    // what to do with it.

    // I am unhappy that I skimmed the book because it robbed me of the
    // opportunity to think.

    // Let's do without merging for now.

    // Now current is larger than address.
    // Convert the header to a free list element.
    prev->next = h;
    h->next = current;

    // Set head of list to smallest address. I assume the big free block is
    // always at the end because we cut it from the front.
    if (h < malloc_free_list) {
      malloc_free_list = h;
    }
  }

  // printf("free: current free list\n");
  // for (memory_header* m = malloc_free_list; m != nullptr; m = m->next) {
  //   printf("free: %x %d\n", m, m->size);
  // }
}
// Memory end

// Task start

// forward declaration
typedef struct message message;

enum task_state { running, blocked, finished };
typedef enum task_state task_state;

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
};

task* task_first = NULL;
task* task_current = NULL;

void trampoline();

extern void switch_task(task* current, task* next);
extern void task_replace(task* task);

// My guess is that we probably want to 'lose' the initial kernel loader task.
// Which means we create a new stack and switch to a new task that we define
// here. What happens with the initial kernel stack? Can we clean it up somehow?
// It won't be needed anymore because we jump out of kmain.
task* task_scheduler = nullptr;
task* task_idle = nullptr;

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

void task_mark_finished(task* task) {
  task->state = finished;
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

void trampoline() {
  printf("finished a task: %d\n", task_current->id);
  task_mark_finished(task_current);
  // Before we called hlt here which wastes time, but it also resulted in a
  // crash because after a hardware interrupt we would jump behind the hlt.
  switch_task(task_current, task_scheduler);
}

void idle_task() {
  while (1) {
    // printf("idle_task: going to idle\n");
    __asm__ volatile("hlt");
    // printf("idle_task: woke up\n");
  }
}
// Task end

// Task declarations

task* service_mouse = nullptr;
task* service_keyboard = nullptr;
task* service_network = nullptr;
task* service_dhcp = nullptr;
task* service_dns = nullptr;

// Message start

// This probably cannot be an enum for long. Message types are probably pretty
// dynamic.
enum message_type {
  message_type_ps2_byte,
  mouse_byte,
  network_data,
  dns_request,
  dns_response,
};
typedef enum message_type message_type_t;
const u8 message_type_count = 2;

typedef struct message message;
struct message {
  message_type_t type;
  void* data;
  message* next;
};

// The interrupt handler should either start this task with high priority OR
// this task already exists and waits for the interrupt blocking. If this is
// late then we will see new data/lose old data. Hence it's important how often
// the OS calls this handler. If it's too slow I could imagine the mouse pointer
// jumping around.
//
// Many applications may wait for mouse/keyboard input, how is that handled? The
// OS/driver could read state from the input device and has a list of whom to
// notify. Maybe in our case we don't notify, yet, we just update globals and
// call display?
//
// The ps/2 controller sends one interrupt per data byte. The mouse sends 3
// bytes so there will be 3 interrupts. Just reading three bytes here does not
// guarantee they are from the mouse, could be a keyboard byte too. We rely on
// the IRQ to know which device the data comes from. Therefore we need to track
// how many bytes we read so far.
// ref: https://forum.osdev.org/viewtopic.php?t=36691&start=15
// ref: https://www.reddit.com/r/osdev/comments/ld8ril/comment/gm6rfx5/
//
// Actually we can see if a byte came from the keyboard or the mouse by checking
// the PS/2 controller output port by sending the 0xD0 command.
//
// Bit 4 is set if the data came from port 1 (kbd)
// Bit 5 is set if the data came from port 2 (mouse)
//
// The first byte we receive has overflow bits. If they are set we should
// probably ignore the corresponding packet or check if the mouse is still
// usable.

// Message passing
//
// Message
// Receiver (?) I don't think I need this. All receivers are registered.
// Type
// Data
//
// handle = Register(type) ?
// message = Wait(handle) ?
// Why did I randomly add handle here? Because there needs to be some connection
// between registering and waiting
//
// Send(type, data)
//

// message* message_first = nullptr;

// TODO: maybe it's time to abstract a queue..

// message_peek returns true if there are messages waiting.
// TODO: what's the naming here? queue, head, first?
bool message_peek(message* head) {
  // if length of queue > 0 return true
  if (head != nullptr) {
    return true;
  } else {
    return false;
  }
}

// Let's do point to point first.
// Let's replace 'wait_for_mouse_data'.
void message_send(message** head, message_type_t type, void* data) {
  // printf("message_send\n");
  // printf("message_send %d\n", message_peek(*head));
  //  alloc message or get from pool.
  //  add to queue
  message* msg = malloc(sizeof(*msg));
  msg->next = nullptr;
  msg->type = type;
  msg->data = data;

  if (*head == nullptr) {
    // TODO: here we probably need pointer to pointer.
    *head = msg;
    return;
  }

  // TODO: keep a pointer to tail for faster append.
  message* m = *head;
  for (; m->next != nullptr; m = m->next) {
  }
  m->next = msg;
}

// mssage_receive checks if there is a message and if not sets the task to a
// waiting state and calls the scheduler. The scheduler wakes it up in case
// there is data waiting, which will restart the loop.
void message_receive(message** head, message* dst) {
  while (true) {
    // TODO: there is a risk to dereference a null pointer here.
    if (*head != nullptr) {
      message* m = *head;
      *head = m->next;
      dst->type = m->type;
      dst->data = m->data;
      free(m);
      return;
    }
    task_current->state = blocked;
    switch_task(task_current, task_scheduler);
  }
}

message* message_type_registry[2];

void message_register(message_type_t type, message* queue) {}

// Message end

// Network start

// TODO: can this be async?
// We probably get an interrupt that tells us TX was done.
// BUG:
// race:
// task 1 calls net_transmit, and calls the first outl,
// network_current_tx_descriptor is 1 task 1 gets pre-empted task 2 calls
// net_transmit, and calls the first outl for a different packet
//
// And many more possibilities.
// Packets need to be queued up and one process puts them onto the network card,
// or we need a lock.
// While I can solve it with the message queue maybe now I can implement a lock?
//
// acquire
// blocks until acquired
// so what is a lock actually that we can wake on it changing?
// simplest we could just sleep 100ms or and try again.
//
// release
//
// most brute force solution is to disable interrupts
void net_transmit(void* data, u32 length) {
  //__asm__("cli");
  // set address to descriptor
  // set size
  // set 0 to own
  // printf("network: tx: using descriptor %d\n",
  // network_current_tx_descriptor);
  outl(base + 0x20 + network_current_tx_descriptor * 4, (u32)data);
  // u32 a = inl(base + 0x20);
  // printf("TX addr: %x\n", a);
  // bit 0-12 = size, bit 13 = own
  outl(base + 0x10 + network_current_tx_descriptor * 4, length);
  // wait for TOK
  while (inl(base + 0x10 + network_current_tx_descriptor * 4) &
	 (1 << 15) == 0) {
  }

  network_current_tx_descriptor = ++network_current_tx_descriptor % 4;
  //__asm__("sti");
}

// Merge
#define NET_ETHERTYPE_IPV4 0x0800
#define NET_ETHERTYPE_ARP 0x0806

#define NET_ARP_OP_REQUEST 1
#define NET_ARP_OP_REPLY 2

#define NET_PORT_DNS 53
#define NET_PORT_DHCP_SERVER 67
#define NET_PORT_DHCP_CLIENT 68

// From ipv4 header
#define NET_PROTOCOL_ICMP 0x1
#define NET_PROTOCOL_UDP 0x11

#define DHCP_OP_REQUEST 1
#define DHCP_OP_REPLY 2

#define DHCP_HARDWARE_TYPE_ETHERNET 1
#define DHCP_HARDWARE_LENGTH_ETHERNET 6

#define DHCP_OPTION_PAD 0
#define DHCP_OPTION_END 255
#define DHCP_OPTION_MESSAGE_TYPE 53
#define DHCP_OPTION_LENGTH_MESSAGE_TYPE 1
#define DHCP_OPTION_MESSAGE_TYPE_DISCOVER 1
#define DHCP_OPTION_MESSAGE_TYPE_OFFER 2
#define DHCP_OPTION_MESSAGE_TYPE_REQUEST 3
#define DHCP_OPTION_MESSAGE_TYPE_DECLINE 4
#define DHCP_OPTION_MESSAGE_TYPE_ACK 5
#define DHCP_OPTION_MESSAGE_TYPE_NACK 6
#define DHCP_OPTION_IP_REQUEST 50
#define DHCP_OPTION_IP_REQUEST_LENGTH 4
#define DHCP_OPTION_SERVER 54
#define DHCP_OPTION_LENGTH_SERVER 4

struct net_device {
  u8 mac[6];
  u8 ip[4];
};

// needs source and dest port
// needs src dest address and protocol ipv4
// all of this is probably on the connection? is there a connection in udp?
// is it on the 'socket'? Is it a request? Request is from to and what kind of
// protocol to use? from / to also decided if it's ipv4 or ipv6?
// I like connection better but udp has no connections.
struct net_request {
  // ipv6 would be longer.
  u8 source_mac[6];
  u8 source_address[4];
  u32 source_port;
  u8 destination_mac[6];
  u8 destination_address[4];
  u32 destination_port;
  u32 ether_type;
  u8 protocol;
};
typedef struct net_request net_request;

struct buffer {
  u8 packet[1518];
  u32 bytes_written_body;
  u32 bytes_written_header;
};
typedef struct buffer buffer;

// Ethernet header 18 bytes + 1500 bytes content.
// ref: https://en.wikipedia.org/wiki/Ethernet_frame
// Ethernet header 18 bytes.
// IP header is min 20 bytes and max 60 bytes.
// UDP header is 8 bytes.
// TCP header is min 20 bytes and max 60 bytes.
// We can reserve 138 bytes.
const u32 buffer_header_offset = 138;

// need to write content
// then need to write all headers backwards
// difficult to know size of ip header
// individual write_header functions?
// ipv4 header size depends on how many options there are, we can precompute
// given we know the options.
void buffer_write_body(buffer* buffer, void* bytes, size_t len) {
  memcpy(bytes,
	 buffer->packet + buffer_header_offset + buffer->bytes_written_body,
	 len);
  buffer->bytes_written_body += len;
}

void buffer_write_byte_body(buffer* buffer, u8 byte) {
  buffer_write_body(buffer, &byte, sizeof(byte));
}

void buffer_write_header(buffer* buffer, void* bytes, size_t len) {
  buffer->bytes_written_header += len;
  memcpy(bytes,
	 buffer->packet + buffer_header_offset - buffer->bytes_written_header,
	 len);
}

void* buffer_packet(buffer* buffer) {
  return buffer->packet + buffer_header_offset - buffer->bytes_written_header;
}

void* buffer_body(buffer* buffer) {
  return buffer->packet + buffer_header_offset;
}

u32 buffer_length(buffer* buffer) {
  return buffer->bytes_written_body + buffer->bytes_written_header;
}

void net_write_ethernet_frame(net_request* req, buffer* buffer) {
  ethernet_frame frame = {0};
  memcpy(req->source_mac, frame.source_mac, 6);
  memcpy(req->destination_mac, frame.destination_mac, 6);
  if (req->ether_type != 0) {
    frame.ethertype = htons(req->ether_type);
  } else {
    frame.ethertype = htons(NET_ETHERTYPE_IPV4);
  }
  buffer_write_header(buffer, &frame, sizeof(frame));
}

void net_write_broadcast_ethernet_frame(
    /*sender address _t*/ buffer* buffer) {
  ethernet_frame frame = {0};
  memcpy(mac, frame.source_mac, 6);
  memcpy(broadcast_mac, frame.destination_mac, 6);
  frame.ethertype = htons(NET_ETHERTYPE_IPV4);
  buffer_write_header(buffer, &frame, sizeof(frame));
}

void net_write_ipv4_header(net_request* req, buffer* buffer) {
  ipv4_header header = {0};
  header.version = 4;
  header.ihl = 5;  // no options
  header.ttl = 100;
  if (req->protocol != 0) {
    header.protocol = req->protocol;
  } else {
    header.protocol = NET_PROTOCOL_UDP;
  }
  header.source_address = htonl_bytes(req->source_address);
  header.destination_address = htonl_bytes(req->destination_address);

  // For one moment I thought we could just access the udp header here and get
  // the length, but it's not feasible for this code to know about other
  // headers.
  header.length = htons(header.ihl * 4 + buffer->bytes_written_header +
			buffer->bytes_written_body);

  header.checksum = ipv4_checksum((u16*)&header, header.ihl * 4);
  buffer_write_header(buffer, &header, sizeof(header));
}

// Checksum is the 16-bit one's complement of the one's complement sum of a
// pseudo header of information from the IP header, the UDP header, and the
// data, padded with zero octets at the end (if necessary) to make a multiple of
// two octets.
// ref: https://datatracker.ietf.org/doc/html/rfc768
// ref: http://www.faqs.org/rfcs/rfc768.html

u16 udp_checksum(net_request* req, udp_header* udph, u16* addr, size_t length) {
  udp_pseudo_ip_header ps = {0};
  ps.source_address = htonl_bytes(req->source_address);
  ps.destination_address = htonl_bytes(req->destination_address);
  ps.protocol = NET_PROTOCOL_UDP;
  ps.udp_length = length;

  /* Compute Internet Checksum for "count" bytes
   *         beginning at location "addr".
   */
  u32 sum = 0;

  u16* first = (u16*)(&ps);
  for (u32 i = 0; i < sizeof(ps) / 2; i++) {
    sum += *first++;
  }

  first = (u16*)(udph);

  for (u32 i = 0; i < sizeof(*udph) / 2; i++) {
    sum += *first++;
  }

  u16 count = ntohs(length);

  while (count > 1) {
    /*  This is the inner loop */
    sum += *addr++;
    count -= 2;
  }

  /*  Add left-over byte, if any */
  if (count > 0) {
    sum += *(u8*)addr;
  }

  /*  Fold 32-bit sum to 16 bits */
  while (sum >> 16) {
    sum = (sum & 0xffff) + (sum >> 16);
  }

  return ~sum;
}

void net_write_udp_header(struct net_request* req, buffer* buffer) {
  udp_header header = {0};
  header.source_port = htons(req->source_port);            // htons(68);
  header.destination_port = htons(req->destination_port);  // htons(67);
  header.length = htons(sizeof(header) + buffer->bytes_written_body);

  // Tricky. We need the header.
  // Accessor for content would be good.
  header.checksum =
      udp_checksum(req, &header, (u16*)(buffer->packet + buffer_header_offset),
		   header.length);

  buffer_write_header(buffer, &header, sizeof(header));
}

// TODO: take a net_device as first parameter for access to mac and ip
// Should this move the FP or return how many bytes written?
void net_write_dhcp_discover_message(u32 xid, buffer* buffer) {
  dhcp_message msg = {0};

  msg.op = DHCP_OP_REQUEST;
  msg.htype = DHCP_HARDWARE_TYPE_ETHERNET;
  msg.hlen = DHCP_HARDWARE_LENGTH_ETHERNET;
  msg.hops = 0x00;
  // Used to match return messages
  msg.xid = xid;
  msg.secs = 0;
  msg.flags = 0;  // Set broadcast bit?
  // MAC
  msg.chaddr[0] = (mac[3] << 24) | (mac[2] << 16) | (mac[1] << 8) | mac[0];
  msg.chaddr[1] = (mac[5] << 8) | mac[4];
  // TODO: make this a constant? It's the options header.
  // ref: https://datatracker.ietf.org/doc/html/rfc2131#section-3
  msg.magic[0] = 0x63;
  msg.magic[1] = 0x82;
  msg.magic[2] = 0x53;
  msg.magic[3] = 0x63;

  buffer_write_body(buffer, &msg, sizeof(msg));

  // net_write_dhcp_option(option, buffer)
  buffer_write_byte_body(buffer, DHCP_OPTION_MESSAGE_TYPE);
  buffer_write_byte_body(buffer, DHCP_OPTION_LENGTH_MESSAGE_TYPE);
  buffer_write_byte_body(buffer, DHCP_OPTION_MESSAGE_TYPE_DISCOVER);
  buffer_write_byte_body(buffer, DHCP_OPTION_END);
}

void send_dhcp_discover() {
  printf("dhcp: sending DHCPDISCOVER\n");
  u32 xid = (u32)get_global_timer_value();

  net_request req = {
      .source_address = {0},
      .source_port = NET_PORT_DHCP_CLIENT,
      .destination_address = {broadcast_ip[0], broadcast_ip[1], broadcast_ip[2],
			      broadcast_ip[3]},
      .destination_port = NET_PORT_DHCP_SERVER,
  };

  buffer buffer = {0};
  net_write_dhcp_discover_message(xid, &buffer);
  net_write_udp_header(&req, &buffer);
  net_write_ipv4_header(&req, &buffer);
  net_write_broadcast_ethernet_frame(&buffer);

  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));
}

void net_write_dhcp_request_message(u32 xid, void* buffer) {
  dhcp_message msg = {0};
  msg.op = DHCP_OP_REQUEST;
  msg.htype = DHCP_HARDWARE_TYPE_ETHERNET;
  msg.hlen = DHCP_HARDWARE_LENGTH_ETHERNET;
  msg.hops = 0x00;
  // TODO: there is some state per request.
  msg.xid = xid;
  msg.secs = 0;
  msg.flags = 0;
  msg.chaddr[0] = (mac[3] << 24) | (mac[2] << 16) | (mac[1] << 8) | mac[0];
  msg.chaddr[1] = (mac[5] << 8) | mac[4];
  msg.siaddr = (dhcp_router[3] << 24) | (dhcp_router[2] << 16) |
	       (dhcp_router[1] << 8) |
	       dhcp_router[0];  // htonl_bytes(dhcp_router);
  msg.magic[0] = 0x63;
  msg.magic[1] = 0x82;
  msg.magic[2] = 0x53;
  msg.magic[3] = 0x63;

  buffer_write_body(buffer, &msg, sizeof(msg));

  buffer_write_byte_body(buffer, DHCP_OPTION_MESSAGE_TYPE);
  buffer_write_byte_body(buffer, DHCP_OPTION_LENGTH_MESSAGE_TYPE);
  buffer_write_byte_body(buffer, DHCP_OPTION_MESSAGE_TYPE_REQUEST);
  buffer_write_byte_body(buffer, DHCP_OPTION_IP_REQUEST);
  buffer_write_byte_body(buffer, DHCP_OPTION_IP_REQUEST_LENGTH);
  buffer_write_byte_body(buffer, ip[0]);
  buffer_write_byte_body(buffer, ip[1]);
  buffer_write_byte_body(buffer, ip[2]);
  buffer_write_byte_body(buffer, ip[3]);
  buffer_write_byte_body(buffer, DHCP_OPTION_SERVER);
  buffer_write_byte_body(buffer, DHCP_OPTION_LENGTH_SERVER);
  buffer_write_byte_body(buffer, dhcp_router[0]);
  buffer_write_byte_body(buffer, dhcp_router[1]);
  buffer_write_byte_body(buffer, dhcp_router[2]);
  buffer_write_byte_body(buffer, dhcp_router[3]);
  buffer_write_byte_body(buffer, 0xff);
}

void send_dhcp_request() {
  printf("dhcp: sending DHCPREQUEST\n");
  // we have the DHCPOFFER reply parameters in the global variables
  u32 xid = dhcp_offer_xid;
  net_request req = {
      //    .source_address = {ip[0], ip[1], ip[2], ip[4]},
      .source_address = {0},
      .source_port = NET_PORT_DHCP_CLIENT,
      .destination_address = {broadcast_ip[0], broadcast_ip[1], broadcast_ip[2],
			      broadcast_ip[3]},
      .destination_port = NET_PORT_DHCP_SERVER,
  };

  buffer buffer = {0};
  net_write_dhcp_request_message(xid, &buffer);
  net_write_udp_header(&req, &buffer);
  net_write_ipv4_header(&req, &buffer);
  net_write_broadcast_ethernet_frame(&buffer);

  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));
}

#define DNS_FLAG_QUERY 0
#define DNS_FLAG_RESPONSE 1
#define DNS_NULL_LABEL 0

// Inject hostname
size_t net_write_dns_query(u16 id, buffer* buffer) {
  dns_header header = {0};

  // transaction id
  header.id = htons(id);
  // number of questions
  header.qdcount = htons(1);
  header.qr = DNS_FLAG_QUERY;
  // recursion desired
  header.rd = 1;

  buffer_write_body(buffer, &header, sizeof(header));

  buffer_write_byte_body(buffer, 6);
  buffer_write_byte_body(buffer, 'g');
  buffer_write_byte_body(buffer, 'o');
  buffer_write_byte_body(buffer, 'o');
  buffer_write_byte_body(buffer, 'g');
  buffer_write_byte_body(buffer, 'l');
  buffer_write_byte_body(buffer, 'e');
  buffer_write_byte_body(buffer, 3);
  buffer_write_byte_body(buffer, 'c');
  buffer_write_byte_body(buffer, 'o');
  buffer_write_byte_body(buffer, 'm');
  buffer_write_byte_body(buffer, DNS_NULL_LABEL);
  // Type A
  buffer_write_byte_body(buffer, 0);  // type byte
  buffer_write_byte_body(buffer, 1);  // type byte
  // Class Internet
  buffer_write_byte_body(buffer, 0);  // class byte
  buffer_write_byte_body(buffer, 1);  // class byte
}

#define NET_PORT_DNS 53

void send_dns_request(u16 id) {
  // How do we usually know the mac address?
  // Is the mac address the one from the router always?
  net_request req = {
      .source_mac = {mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]},
      .source_address = {ip[0], ip[1], ip[2], ip[3]},
      .source_port = NET_PORT_DNS,
      .destination_mac = {dhcp_router_mac[0], dhcp_router_mac[1],
			  dhcp_router_mac[2], dhcp_router_mac[3],
			  dhcp_router_mac[4], dhcp_router_mac[5]},
      .destination_address = {dhcp_dns[0], dhcp_dns[1], dhcp_dns[2],
			      dhcp_dns[3]},
      .destination_port = NET_PORT_DNS,
  };

  buffer buffer = {0};
  net_write_dns_query(id, &buffer);
  net_write_udp_header(&req, &buffer);
  net_write_ipv4_header(&req, &buffer);
  net_write_ethernet_frame(&req, &buffer);

  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));
}

void net_write_arp_response(net_request* req, buffer* buffer) {
  arp_message msg = {0};
  msg.htype = htons(1);  // ethernet
  msg.hlen = 6;
  msg.ptype = htons(NET_ETHERTYPE_IPV4);
  msg.plen = 4;
  msg.oper = htons(2);  // reply
  memcpy(req->source_mac, &msg.sender_mac_1, 6);
  memcpy(req->source_address, &msg.sender_protocol_address_1, 4);
  memcpy(req->destination_mac, &msg.target_mac_1, 6);
  memcpy(req->destination_address, &msg.target_protocol_address_1, 4);
  buffer_write_body(buffer, &msg, sizeof(msg));
}

void send_arp_response(u8* sender_mac, u8* sender_ip) {
  net_request req = {
      .source_mac = {mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]},
      .source_address = {ip[0], ip[1], ip[2], ip[3]},
      .source_port = NET_PORT_DHCP_CLIENT,
      .destination_mac = {sender_mac[0], sender_mac[1], sender_mac[2],
			  sender_mac[3], sender_mac[4], sender_mac[5]},
      .destination_address = {sender_ip[0], sender_ip[1], sender_ip[2],
			      sender_ip[3]},
      .destination_port = NET_PORT_DHCP_SERVER,
      .ether_type = NET_ETHERTYPE_ARP,
  };
  buffer buffer = {0};
  net_write_arp_response(&req, &buffer);
  net_write_ethernet_frame(&req, &buffer);

  //  Can this be async?
  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));

  //  TODO: free packet because it's on the network card buffer now.
}

// TODO: split into header and message
void net_write_icmp_echo(buffer* buffer) {
  u16 packet[4] = {0};
  icmp_header* h = (icmp_header*)packet;
  h->type = 8;
  h->code = 0;

  icmp_echo_message* m = (icmp_echo_message*)(h + 1);
  m->identifier = 0x1;
  m->sequence_number = 0;

  h->checksum =
      ipv4_checksum(packet, sizeof(icmp_header) + sizeof(icmp_echo_message));
  buffer_write_body(buffer, packet,
		    sizeof(icmp_header) + sizeof(icmp_echo_message));
}

struct dns_request {
  u8 name[63];
  message** sender;
};

// TODO: add ttl
struct dns_response {
  u16 id;
  u8 name[63];
  u8 address[4];
};

struct net_address {
  u8 address[4];
};

// TODO: make this work async by doing the dns query.
// Imagine:
// send_echo()
// - build package with hostname
// - send
// -- do we know this hostname?
// --- no: resolve and block
// --- yes: fetch from cache
// -- fill IP
// Question: how to do timeouts? The blocking command needs a timeout.
struct net_address net_resolve_hostname(u8 hostname[63]) {
  // What to do here?
  // Let's assume there is no cache. We always have to resolve the addresses.
  //

  // how do we wait for it's response?
  // we want to block here until the response is here.
  message msg = {0};

  message_send(&service_dns->queue, dns_request, nullptr);

  printf("resolve_hostname: waiting for response\n");
  message_receive(&service_dhcp->queue, &msg);

  struct dns_response* resp = msg.data;
  printf("resolve_hostname: %d.%d.%d.%d\n", resp->address[0], resp->address[1],
	 resp->address[2], resp->address[3]);

  struct net_address addr = {.address[0] = resp->address[0],
			     .address[1] = resp->address[1],
			     .address[2] = resp->address[2],
			     .address[3] = resp->address[3]};

  return addr;

  // alloc object that holds address.
  // put into data
  //  msg.data =
  //  message_send(message **head, message_type_t type, void *data);

  // what's the difference between asking synchronously vs asynchronously?
  // synchronous implies no task change?

  // Let's assume there is a dns server and it's listening for messages.
}

void send_echo() {
  // TODO: resolve via dns query first.
  // so we need to block until it's resolved. Interesting.
  struct net_address google_address = net_resolve_hostname(nullptr);
  net_request req = {
      .source_mac = {mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]},
      .source_address = {ip[0], ip[1], ip[2], ip[3]},

      .destination_mac = {dhcp_router_mac[0], dhcp_router_mac[1],
			  dhcp_router_mac[2], dhcp_router_mac[3],
			  dhcp_router_mac[4], dhcp_router_mac[5]},
      .destination_address = {google_address.address[0],
			      google_address.address[1],
			      google_address.address[2],
			      google_address.address[3]},
      .protocol = NET_PROTOCOL_ICMP,
  };

  buffer buffer = {0};
  net_write_icmp_echo(&buffer);
  net_write_ipv4_header(&req, &buffer);
  net_write_ethernet_frame(&req, &buffer);

  // if (memcmp(ef, buffer_packet(&buffer), buffer_length(&buffer)) == false) {
  //   printf("[net_write] did not match\n");
  //   __asm__("cli");
  //   __asm__("hlt");
  // }

  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));
}

// net_handle_arp handles the address resolution protocol.
// ARP is used to find a MAC address that corresponds to an IP address.
// ARP can be used to test if an IP address is free to use by sending a probing
// packet.
// ref: https://datatracker.ietf.org/doc/html/rfc826
void net_handle_arp(arp_message* msg) {
  // printf("arp: htype: %x, ptype: %x, hlen: %x, plen: %x, oper: %x\n",
  //	 ntohs(msg->htype), ntohs(msg->ptype), msg->hlen, msg->plen,
  //	 ntohs(msg->oper));

  // TODO: memcmp so I don't have to copy?
  if (ntohs(msg->oper) == NET_ARP_OP_REQUEST) {
    // check if our IP matches
    u8* target_ip = (u8*)&msg->target_protocol_address_1;
    // printf("arp: target IP: %d.%d.%d.%d\n", target_ip[0], target_ip[1],
    //	   target_ip[2], target_ip[3]);

    if (memcmp(target_ip, ip, 4)) {
      // Can this be typed somehow?
      u8* sender_mac = (u8*)&msg->sender_mac_1;
      u8* sender_ip = (u8*)&msg->sender_protocol_address_1;
      send_arp_response(sender_mac, sender_ip);
    }
  }
}

// I want to save the domain -> ip mapping.
// Maybe I need a map after all.
void* net_dns_parse_question(void* buffer) {
  u8* p = buffer;
  u8 domain[5][63] = {0};
  u8 label = 0;
  while (*p != 0) {
    u8 length = *p;
    p++;
    for (u8 i = 0; i < length; i++) {
      domain[label][i] = *p;
      p++;
    }

    printf("label: %s\n", domain[label]);
    label++;
  }
  // skip 0-TERMINATOR QTYPE QCLASS
  return p + 1 + 2 + 2;
}

// This gets trickier beacause of message compression. Need access to the whole
// message.
// TODO: this is super brittle, refactor out the label reading etc.
// TODO: return answer structs. domain name is limited to 63.
// Also, it would probably be good to have some kind of reader.
// ReadyByte, ReadyByte, ReadInt and it converts to LE
void* net_dns_parse_answer(void* message,
			   void* answers,
			   struct dns_response* response) {
  u8* p = answers;
  u8 domain[5][63] = {0};
  u8 label = 0;
  while (*p != 0) {
    // first 2 bits signal offset
    if ((*p & 0b11000000) == 0b11000000) {
      u16 offset = ((*p & ~0b11000000) >> 8) | *(p + 1);
      p += 2;

      u8* offsetptr = message + offset;
      while (*offsetptr != 0) {
	u8 length = *offsetptr;
	offsetptr++;
	for (u8 i = 0; i < length; i++) {
	  domain[label][i] = *offsetptr;
	  offsetptr++;
	}
	printf("answer offset label: %s\n", domain[label]);
	label++;
      }
    } else {
      u8 length = *p;
      p++;
      for (u8 i = 0; i < length; i++) {
	domain[label][i] = *p;
	p++;
      }
      printf("answer label: %s\n", domain[label]);
      label++;
    }
  }

  // skip type, class, ttl
  p += 2 + 2 + 4;

  //  u16* rdlength = p;
  p += 2;

  u8 address[4] = {0};
  for (u8 i = 0; i < 4; i++) {
    address[i] = *p;
    response->address[i] = *p;
    p++;
  }

  printf("address: %d.%d.%d.%d\n", address[0], address[1], address[2],
	 address[3]);

  return p;
}

void net_handle_dns(dns_header* h) {
  printf("dns: id: %x, opcode: %x, qr: %x acount: %x\n", ntohs(h->id),
	 h->opcode, h->qr, ntohs(h->ancount));

  if (h->qr != DNS_FLAG_RESPONSE) {
    return;
  }

  // read questions (are repeated in response messages)
  void* p = h + 1;
  for (int i = 0; i < ntohs(h->qdcount); i++) {
    p = net_dns_parse_question(p);
  }
  // read answers
  struct dns_response* resp = malloc(sizeof(*resp));
  resp->id = ntohs(h->id);
  for (int i = 0; i < ntohs(h->ancount); i++) {
    p = net_dns_parse_answer(h, p, resp);
  }
  message_send(&service_dns->queue, dns_response, resp);
}

enum dhcp_state {
  dhcp_discover,
  dhcp_offer,
  dhcp_request,
  dhcp_ack,
  dhcp_done
};

enum dhcp_state dhcp_state_current = dhcp_discover;

void net_handle_dhcp(ethernet_frame* ef, dhcp_message* m) {
  printf("dhcp: op: %x htype: %x hlen: %x hops: %x xid: %x\n", m->op, m->htype,
	 m->hlen, m->hops, m->xid);

  if (m->op != DHCP_OP_REPLY) {
    printf("dhcp: received a dhcp request\n");
    return;
  }

  u8* o = (u8*)(m + 1);  // end of dhcp message for options

  // TODO: double check that we don't go over length of message.

  u8 dhcp_type = 0;

  bool done = false;
  while (!done) {
    switch (*o++) {
      case 1:  // subnet mask
	if (*o++ != 4) {
	  // panic
	}
	memcpy(o, dhcp_subnet_mask, 4);
	o += 4;
	break;
      case 3:  // router
	if (*o++ != 4) {
	  // panic
	}
	memcpy(o, dhcp_router, 4);
	o += 4;
	break;
      case 6:  // dns
	if (*o++ != 4) {
	  // panic
	}
	memcpy(o, dhcp_dns, 4);
	o += 4;
	break;
      case 53:  // type
	if (*o++ != 1) {
	  // panic
	}
	dhcp_type = *o++;
	break;
      case 54:            // server identifier (dhcp server ip)
	if (*o++ != 4) {  // what about ipv6?
			  // panic
	}
	memcpy(o, dhcp_identifier, 4);
	o += 4;
	break;
      case 51:  // lease time (2 days, so low priority to implement)
	break;
      case 0xff:
	done = true;
	break;
    }
  }

  if (dhcp_state_current == dhcp_offer &&
      dhcp_type != DHCP_OPTION_MESSAGE_TYPE_OFFER) {
    printf("dhcp: we are waiting for an offer, but was %x\n", dhcp_type);
    return;
  }

  if (dhcp_state_current == dhcp_ack &&
      dhcp_type != DHCP_OPTION_MESSAGE_TYPE_ACK) {
    printf("dhcp: we are waiting for an ack, but was %x\n", dhcp_type);
    return;
  }

  if (dhcp_type == DHCP_OPTION_MESSAGE_TYPE_OFFER) {
    printf("dhcp: received DHCPOFFER\n");
    printf("dhcp: new state: request\n");
    dhcp_state_current = dhcp_request;

    memcpy(m->yiaddr, ip, 4);
    /* for (int i = 0; i < 4; i++) { */
    /*   ip[i] = m->yiaddr[i]; */
    /* } */
    printf("dhcp: assigned IP %d.%d.%d.%d\n", ip[0], ip[1], ip[2], ip[3]);
    printf("dhcp: router IP %d.%d.%d.%d\n", dhcp_router[0], dhcp_router[1],
	   dhcp_router[2], dhcp_router[3]);
    printf("dhcp: dns IP %d.%d.%d.%d\n", dhcp_dns[0], dhcp_dns[1], dhcp_dns[2],
	   dhcp_dns[3]);
    printf("dhcp: subnet mask %d.%d.%d.%d\n", dhcp_subnet_mask[0],
	   dhcp_subnet_mask[1], dhcp_subnet_mask[2], dhcp_subnet_mask[3]);

    dhcp_offer_xid = m->xid;

  } else if (dhcp_type == DHCP_OPTION_MESSAGE_TYPE_ACK) {
    printf("dhcp: received DHCPACK\n");
    dhcp_state_current = dhcp_done;
    printf("dhcp: new state: done %d\n", dhcp_state_current);

    printf("dhcp: target_mac: %x:%x:%x:%x:%x:%x\n", ef->destination_mac[0],
	   ef->destination_mac[1], ef->destination_mac[2],
	   ef->destination_mac[3], ef->destination_mac[4],
	   ef->destination_mac[5]);
    printf("dhcp: source_mac: %x:%x:%x:%x:%x:%x\n", ef->source_mac[0],
	   ef->source_mac[1], ef->source_mac[2], ef->source_mac[3],
	   ef->source_mac[4], ef->source_mac[5]);
    memcpy(ef->source_mac, dhcp_router_mac, 6);
  }
}

void net_handle_udp(ethernet_frame* frame, udp_header* header) {
  printf("udp: src port: %d dst port: %d\n", ntohs(header->source_port),
	 ntohs(header->destination_port));

  if (ntohs(header->destination_port) == NET_PORT_DNS) {  // DNS
    net_handle_dns((dns_header*)(header + 1));
  } else if (ntohs(header->source_port) == 67 &&
	     ntohs(header->destination_port) == 68) {  // DHCP
    net_handle_dhcp(frame, (dhcp_message*)(header + 1));
  }
}

void net_handle_ipv4(ethernet_frame* frame, ipv4_header* header) {
  printf("ipv4: version: %x ihl: %x\n", header->version, header->ihl);

  // printf("ipv4: protocol: %x\n", iph->protocol);

  if (header->protocol == NET_PROTOCOL_ICMP) {
  } else if (header->protocol == NET_PROTOCOL_UDP) {
    net_handle_udp(frame, (udp_header*)(header + 1));
  }
}

void task1(u8 id) {
  while (1) {
    printf("running task 1 %d\n", id);
    sleep(2000);
  }
}

void task2(u8 id) {
  printf("running task 2 %d\n", id);
}

void task_network() {
  // what do I want to do here?
  // I want to improve memory management and task management.
  // Using the network stack can help.

  u8 naddr[4];
  // This will cause a bunch of network requests. We cannot do this before we
  // got a network address ourselves so I need to run a dhcp task.
  //  dns_resolve("google.com", addr);
  // printf("google.com IP=%d.%d.%d.%d\n");
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

// Is this the network driver? No. The network driver would fetch data from the
// card and pass it up the network stack.
// This is huge. Maybe I should start with the mouse driver?
void handle_network_interrupt() {
  /*
    packet header from network card
    ref:
    https://www.cs.usfca.edu/~cruse/cs326f04/RTL8139_ProgrammersGuide.pdf

    Bit R/W Symbol Description

    15 R MAR Multicast Address Received: Set to 1 indicates that a multicast
    packet is received.

    14 R PAM Physical Address Matched: Set to 1 indicates that the
    destination address of this packet matches the value written in ID
    registers.

    13 R BAR Broadcast Address Received: Set to 1 indicates that a broadcast
    packet is received. BAR, MAR bit will not be set simultaneously. 12-6 -
    - Reserved

    5 R ISE Invalid Symbol Error: (100BASE-TX only) An invalid symbol was
    encountered during the reception of this packet if this bit set to 1.

    4 R RUNT Runt Packet Received: Set to 1 indicates that the received
    packet length is smaller than 64 bytes ( i.e. media header + data + CRC
    < 64 bytes )

    3 R LONG Long Packet: Set to 1 indicates that the size of the received
    packet exceeds 4k bytes.

    2 R CRC CRC Error: When set, indicates that a CRC error occurred on the
    received packet.

    1 R FAE Frame Alignment Error: When set, indicates that a frame
    alignment error occurred on this received packet.

    0 R ROK Receive OK: When set, indicates that a good packet is received.
   */

  /*
    The receive path of RTL8139(A/B) is designed as a ring buffer. This ring
    buffer is in a physical continuous memory. Data coming from line is
    first stored in a Receive FIFO in the chip, and then move to the receive
    buffer when the early receive threshold is met. The register CBA keeps
    the current address of data moved to buffer. CAPR is the read pointer
    which keeps the address of data that driver had read. The status of
    receiving a packet is stored in front of the packet(packet header).
   */

  u16 isr = inw(base + 0x3e);
  // printf("isr: %x\n", isr);
  // although the docs say we only need to read, we actually need to write
  // to reset
  //   printf("resetting\n");
  outw(base + 0x3e, isr);

  if (isr & 0x1) {
    // printf("isr: Rx OK\n");
  } else if (isr & 0x4) {
    // printf("isr: Tx OK");
    goto eth_return;
  } else {
    goto eth_return;
  }

#define NET_BIT_BUFFER_EMPTY 0x1
  while (true) {
    u8 cmd = inb(base + 0x37);
    if (cmd & NET_BIT_BUFFER_EMPTY) {  // Buffer Empty = 1
      break;
    }

    u16 capr = inw(base + 0x38);
    // printf("capr: %x\n", capr);

    u16 cbr = inw(base + 0x3a);
    // printf("cbr: %x\n", cbr);

    // RX buffer content
    // packet header | packet length | ethernet frame
    // 2 bytes         | 2 bytes        |

    // ethernet frame
    // MAC dest | MAC src | Tag (optional) | EtherType / length | Payload |
    // CRC/FCS 6 bytes   | 6 bytes | 4 bytes          | 2 bytes | 42–1500  |
    // 4 bytes

    // ether type
    // 0x0800	Internet Protocol version 4 (IPv4)
    // 0x86DD	Internet Protocol Version 6 (IPv6)
    // 0x0806      ARP

    // we get 0x45 = 0b0100 0101 in big endian / network byte order
    // I assume we get protocol ipv4 so the left part is the 4 and right is
    // 5 which is the header size of a header without options.

    // Note: network byte order

    // TODO: print 16 bytes before this to see what the card puts there. Since
    // we emulate it's probably empty.

    u16* packet_status = (u16*)(network_rx_buffer + network_rx_buffer_index);

    //  printf("network interrupt: num: %d, status: %x, buffer index: %x\n",
    //	     num_packets++, *packet_status, network_rx_buffer_index);

    // TODO: signal this error to network service.
    // CRC, RUNT, LONG, FAE, BAD SYMBOL errors
    if (*packet_status & (1 << 1) || *packet_status & (1 << 2) ||
	*packet_status & (1 << 3) || *packet_status & (1 << 4) ||
	*packet_status & (1 << 5)) {
      break;
    }

    u16* length = (u16*)(network_rx_buffer + network_rx_buffer_index +
			 2);  // first two bytes are the rx header
    //  printf("length: %d\n", *length);

    // TODO: pull length and etheretype from this.
    // first two bytes are the rx header followed by 2 bytes for length
    ethernet_frame* ef =
	(ethernet_frame*)(network_rx_buffer + network_rx_buffer_index + 4);

    // Copy packet and send it to network service.
    // The network card appends a 4 byte CRC at the end which we ignore.
    void* buffer = malloc(*length - 4);
    memcpy(ef, buffer, *length - 4);
    message_send(&service_network->queue, network_data, buffer);

    //     printf("network: idx before: %x\n", network_rx_buffer_index);

    // length seems to not include the header and size which are 2 bytes each
    network_rx_buffer_index += *length + 4;

    // wrap
    if (network_rx_buffer_index > RX_BUFFER_SIZE) {
      // the controller expects us to not reset it to 0 for some reason.
      network_rx_buffer_index -= RX_BUFFER_SIZE;
    }

    // it seems we need to align on dword boundaries.
    // this means the first 2 bits should be 0, because 1 2 or 3 would not
    // be 4 which is dword. just cutting them off would mean we inside are
    // the packet we just read. is this bad? the programming guide adds 3 to
    // make sure we are outside.

    // according to qemu code there is a 4byte checksum
    // https://github.com/qemu/qemu/blob/master/hw/net/rtl8139.c#L1161-L1165

    // ~ is the complement meaning all 1s except the first 2 bits.
    network_rx_buffer_index = (network_rx_buffer_index + 3) & ~3;
    // Result: network_rx_buffer_index == cbr in the first run. Later cbr
    // grows much faster. After looping until BUFFER_EMPTY == 1, CRB always
    // matches with network_rx_buffer_index. The - 0x10 adjustment was also
    // necessary. Otherwise BUFFER_EMPTY never gets set.
    // ref: https://github.com/qemu/qemu/blob/master/hw/net/rtl8139.c#L1384

    // printf("new buffer index: %x %d\n", network_rx_buffer_index,
    //	     network_rx_buffer_index);

    // the doc says to subtract 0x10 to avoid overflow. also, given we use
    // network_rx_buffer_index which is now bigger, won't this break us
    // reading packets? for some reason this is added back
    // https://github.com/qemu/qemu/blob/master/hw/net/rtl8139.c#L2522 so if
    // if we don't subtract the numbers don't match. who knows why this is
    // done.
    // Thinking and researching more about this I came to the conclusion that
    // the network card must somehow reserve 16 byte for some kind of header.
    outw(base + 0x38, network_rx_buffer_index - 0x10);
  }
eth_return:
  return;
}

void network_service() {
  // Why do I call this service? I imagine the OS always keeps the network stack
  // in shape. Does DHCP stuff, etc. Can the kernel do this directly? Well it
  // needs to do many network calls so it blocks.. Hence modeling it as a task
  // or service seems to make sense?

  // Reply to ARP messages?

  // -> dhcp discover
  // <- dhcp offer
  // -> dhcp request
  while (true) {
    message msg;
    message_receive(&service_network->queue, &msg);

    ethernet_frame* frame = (ethernet_frame*)msg.data;

    u16 ether_type = ntohs(frame->ethertype);

    if (ether_type == NET_ETHERTYPE_ARP) {
      net_handle_arp((arp_message*)(frame + 1));
    } else if (ether_type == NET_ETHERTYPE_IPV4) {
      net_handle_ipv4(frame, (ipv4_header*)(frame + 1));
    }

    free(msg.data);
  }
}

// I want to model DHCP in case I miss a package or something I should retry.
// But now we suddenly need timers and a better msg queue.
// I imagine I have another service, a dhcp service that is a state machine.
// could I do this here in the network_service directly?
// We could do it super primitive. On every network message check how long we
// waited.

u64 dhcp_last_request_ts = {0};
// RACE: dhcp_state_current
// The receiver task changes it because it handles dhcp respondes.
// This task handles it.
// Maybe this task should be the only handler and the other task just parses
// messages and puts them on a queue for the state machine.
// For now we can simplify once more and make the dhcp_service global and put
// onto its queue.
//
// How to handle race conditions?
// Easiest, only access in the same task/thread.
// Lock.
//
// Thought: wouldn't it be easier to code this in a req/response style?
// Send discover and wait for response or timeout, then proceed or retry.
// Interestingly this is similar to having to block until dns resolution.
enum dhcp_state dhcp_state_last = dhcp_discover;
void dhcp_service() {
  while (true) {
    printf("dhcp: loop state %d\n", dhcp_state_current);
    if (dhcp_state_current == dhcp_done) {
      printf("sending echo\n");
      send_echo();
      goto sleep;
    }

    // handle dhcp state machine if last request was a long time ago.
    u64 now = get_global_timer_value();
    if (now <= dhcp_last_request_ts + _1s) {
      goto sleep;
    }

    // // If nothing changed for one second let's go back one state.
    // // E.g. we did not get an offer then resend discovery.
    // if (dhcp_state_last == dhcp_state_current) {
    //   dhcp_state_current = max(0, dhcp_state_current - 1);
    // }

    // What do we have to do?
    // Send some request to get to the next state.
    // Then we wait for the response to come for some time.
    // If it doesn't come we repeat the last message.
    switch (dhcp_state_current) {
      case dhcp_discover:
	dhcp_last_request_ts = now;
	dhcp_state_current = dhcp_offer;

	printf("dhcp: new state: offer\n");
	send_dhcp_discover();
	break;
      case dhcp_request:
	dhcp_last_request_ts = now;
	dhcp_state_current = dhcp_ack;

	printf("dhcp: new state: ack\n");
	// Possible race here.
	send_dhcp_request();
	break;
    }
  sleep:
    // dhcp_state_last = dhcp_state_current;
    sleep(1000);
  }
}

// we can have an array of tx ids circular. max N requests.
// we can attach the receiver pointer

message** dns_requests[10];
u8 dns_request_idx = 0;
// -1 because we count from 0
u8 dns_request_count = 10 - 1;

void dns_service() {
  while (true) {
    message msg;
    message_receive(&service_dns->queue, &msg);

    switch (msg.type) {
      case dns_request:
	// dns_request could have the hostname on it
	// it should also carry something so we can send the answer back.
	// what is that?
	// is it a pointer to a queue?
	// if I use the tasks queue then other tasks than dns_service may send
	// something there. how do I differentiate?
	// For now let's assume the sender only waits for one reply.

	// inject hostname
	u8 id = dns_request_idx;
	dns_request_idx = (dns_request_idx + 1) % dns_request_count;

	// This should be pulled out of the message
	dns_requests[id] = &service_dhcp->queue;

	// note that we are waiting for a response.
	// printf("dns_service: sending dns request with id %d\n", id);
	send_dns_request(id);
	break;
      case dns_response:
	// We got a response from the network stack.
	struct dns_response* resp = msg.data;
	// printf("dns_service: response for id %d address %d.%d.%d.%d\n",
	//        resp->id, resp->address[0], resp->address[1],
	//        resp->address[2], resp->address[3]);
	message** queue = dns_requests[resp->id];
	if (queue != nullptr) {
	  message_send(queue, dns_response, resp);
	}

	// Check tx id if we waited for it. There will always be something
	// in the tx array, so we don't know if it was the same question, we
	// may reply with the wrong IP. Important: if the array is full we
	// forget requests and the caller is waiting for a reply. Needs a
	// timeout, or we have an unlimited queue, or both. Find if someone
	// asked for it and answer.
	break;
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
      printf("schedule: cleaning up finished task\n");
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
	if (message_peek(t->queue) == true) {
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
	}
	continue;
      }

      // This task is sleeping
      // TODO: integrate this somehow with state blocked.
      if (t->sleep_until > now) {
	//	printf("scheduler: task is sleeping until: %d now: %d\n",
	//	       t->sleep_until, now);
	continue;
      }

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
  volatile u32* local_apic_eoi = (volatile u32*)0xfee000b0;
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
  idt_set_gate(11, 0, 0x08, 0x0E);
  idt_set_gate(12, 0, 0x08, 0x0E);
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

void gdt_setup() {
  gdt.limit = sizeof(gdt_entry_t) * 5 - 1;
  gdt.base = (u32)&gdt_entries;

  gdt_set_gate(0, 0, 0, 0, 0);
  gdt_set_gate(1, 0, 0xffffffff, 0x9a, 0xcf);  // kernel mode code segment
  gdt_set_gate(2, 0, 0xffffffff, 0x92, 0xcf);  // kernel mode data segment
  gdt_set_gate(3, 0, 0xffffffff, 0xfa, 0xcf);  // user mode code segment
  gdt_set_gate(4, 0, 0xffffffff, 0xf2, 0xcf);  // user mode data segment
  gdt_set_gate(5, (u32)&tss, sizeof(tss), 0x89,
	       0x40);  // cpu1 task switching segment

  gdt_update(&gdt);
}

void gdt_set_gate(u32 entry, u32 base, u32 limit, u8 access, u8 flags) {
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
  u32 volatile* ioregsel = (u32 volatile*)IOAPIC_IOREGSEL;
  ioregsel[0] = reg;
  return ioregsel[4];  // IOAPIC_IOREGSEL+10h (4*4 byte = 16 byte) =
  // IOAPIC_IOWIN
}

void ioapic_write_register(u32 reg, ioapic_redirection_register_t* r) {
  u32 volatile* ioregsel = (u32 volatile*)IOAPIC_IOREGSEL;
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

  // Bug: here the problems start. After enabling interrupts we somehow receive
  // the response to PS2_COMMAND_READ_CONFIG_BYTE via keyboard interrupt.
  // Output:
  // scancode: 0x3
  // config byte 6: 0x3
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

// TODO: pull entries out as 'first' into sdt struct.
// sdt: system description table
struct acpi_sdt_header2 {
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
typedef struct acpi_sdt_header2 acpi_sdt_header2_t;

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
  u32 entries;  // length - (sizeof(header) - 4 byte)
} __attribute__((packed));
typedef struct acpi_sdt_header acpi_sdt_header_t;

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
  u64* start = (u64*)0xE0000;
  u64* end = (u64*)0xFFFFF;

  // "RSD PTR "
  for (; start < end; start += 2) {
    u8* s = (u8*)start;
    if (s[0] == 'R' && s[1] == 'S' && s[2] == 'D' && s[3] == ' ' &&
	s[4] == 'P' && s[5] == 'T' && s[6] == 'R' && s[7] == ' ') {
      return (acpi_rsdp_t*)start;
    }
  }
  return nullptr;
}

struct acpi_madt {
  acpi_sdt_header2_t header;
  u32 local_interrupt_controller_address;
  u32 flags;
  u32 first;
} __attribute__((packed));
typedef struct acpi_madt acpi_madt_t;

// ics = interrupt controller structure
struct acpi_ics_header {
  u8 type;
  u8 length;
} __attribute__((packed));
typedef struct acpi_ics_header acpi_ics_header_t;

struct acpi_ics_ioapic {
  acpi_ics_header_t header;
  u8 id;
  u8 reserved;
  u32 address;
  u32 global_system_interrupt_base;
} __attribute__((packed));
typedef struct acpi_ics_ioapic acpi_ics_ioapic_t;

struct acpi_ics_input_source_override {
  acpi_ics_header_t header;
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
  u32 event_timer_block_id;
  acpi_generic_address_structure_t base_address;
  u8 hpet_number;
  u16 main_counter_minimum_clock_tick;
  u8 page_attribution;
} __attribute__((packed));
typedef struct acpi_hpet_header acpi_hpet_header_t;

// note: take care when taking references of a pointer.
void list_tables(acpi_sdt_header_t* rsdt) {
  int count =
      (rsdt->length - sizeof(acpi_sdt_header_t) + sizeof(u32)) / sizeof(u32);
  printf("rsdt: entry count: %d\n", count);
  for (int i = 0; i < count; i++) {
    // &entries to get the first entry.
    // +i uses size of type which is u32.
    // * because it's a pointer to some place.
    acpi_sdt_header_t* h = (acpi_sdt_header_t*)(*(&rsdt->entries + i));
    printf("table %d: %.*s\n", i, 4, h->signature);
    if (strncmp(h->signature, "APIC", 4)) {
      acpi_madt_t* madt = (acpi_madt_t*)h;
      printf("configuring acpi: %x\n",
	     madt->local_interrupt_controller_address);
      // how many? madt->length?
      // first
      int j = 0;
      // TODO: refactor this pointer arithmetic.
      for (acpi_ics_header_t* h = (acpi_ics_header_t*)(&(madt->first));;
	   h = (acpi_ics_header_t*)((char*)h + h->length)) {
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
	j++;
	if (j == 10) {
	  break;
	}
      }
    } else if (strncmp(h->signature, "HPET", 4)) {
      // note: without u8 case here we go too far.
      acpi_hpet_header_t* hpet =
	  (acpi_hpet_header_t*)((u8*)h + sizeof(acpi_sdt_header2_t));
      printf("HPET: hpet number: %d\n", hpet->hpet_number);
      printf("HPET: address space: %d base: %x\n",
	     hpet->base_address.address_space_id, hpet->base_address.address);
    }
  }
}

#define PCI_CONFIG_ADDRESS 0xcf8
#define PCI_CONFIG_DATA 0xcfc

typedef union {
  u32 raw;
  struct {
    u32 offset : 8;
    u32 function : 3;
    u32 device : 5;
    u32 bus : 8;
    u32 reserved : 7;
    u32 enabled : 1;
  } bits;
} pci_config_address_t;

typedef union {
  u32 raw;
  struct {
    u16 vendor_id;
    u16 device_id;
  } __attribute__((packed)) fields;
} pci_config_register_0_t;

typedef union {
  u32 raw;
  struct {
    union {
      u16 command;
      struct {
	u16 io_space : 1;
	u16 memory_space : 1;
	u16 bus_master : 1;
	u16 reserved : 13;
      } command_bits;
    };
    union {
      u16 status;
      struct {
	u16 reserved : 16;
      } status_bits;
    };
  } __attribute__((packed)) fields;
} pci_config_register_1_t;

typedef union {
  u32 raw;
  struct {
    u8 revision_id;
    u8 prog_if;
    u8 subclass;
    u8 class;
  } __attribute__((packed)) fields;
} pci_config_register_2_t;

typedef union {
  u32 raw;
  struct {
    u8 cache_line_size;
    u8 latency_timer;
    u8 header_type;
    u8 bist;
  } __attribute__((packed)) fields;
} pci_config_register_3_t;

typedef union {
  u32 raw;
  struct {
    u32 is_io_space : 1;
    u32 type : 2;
    u32 prefetchable : 1;
    u32 address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    u32 is_io_space : 1;
    u32 reserved : 1;
    u32 address : 30;
  } __attribute__((packed)) io_space;
} pci_config_register_4_t;

typedef union {
  u32 raw;
  struct {
    u32 is_io_space : 1;
    u32 type : 2;
    u32 prefetchable : 1;
    u32 address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    u32 is_io_space : 1;
    u32 reserved : 1;
    u32 io_size : 6;
    u32 address : 24;
  } __attribute__((packed)) io_space;
} pci_config_register_4_rtl8139_t;

typedef union {
  u32 raw;
  struct {
    u8 interrupt_line;
    u8 interrupt_pin;
    u8 min_grant;
    u8 max_latency;
  } __attribute__((packed)) fields;
} pci_config_register_f_t;

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
	base = h4.raw & 0xFFFFFFFC;
	//	printf("pci header: address 1: %x\n", base);
	if (h4.io_space.is_io_space) {
	  // printf("pci header: io space: address: %x, base: %x\n",
	  //	 h4.io_space.address, base);
	  //	  outb(base + 0x52, 0x0);  // start?

	  mac[0] = inb(base + 0x00);
	  mac[1] = inb(base + 0x01);
	  mac[2] = inb(base + 0x02);
	  mac[3] = inb(base + 0x03);
	  mac[4] = inb(base + 0x04);
	  mac[5] = inb(base + 0x05);

	  printf("ethernet: mac: %x:%x:%x:%x:%x:%x\n", mac[0], mac[1], mac[2],
		 mac[3], mac[4], mac[5]);

	  //	  u8 config_1 = inb(base + RTL8139_CONFIG_1);
	  //	  printf("ethernet: config 1: %x\n", config_1);

	  //	  u8 config_3 = inb(base + 0x59);
	  //	  printf("ethernet: config 3: %x\n", config_3);

	  //	  u8 config_4 = inb(base + 0x5A);
	  //	  printf("ethernet: config 4: %x\n", config_4);

	  u8 cmd = inb(base + RTL8139_CMD);
	  // here reset is 1 as written on osdev. qemu bug.
	  printf("ethernet: cmd: %x\n", cmd);

	  // soft reset
	  outb(base + RTL8139_CMD, 0x10);
	  while ((inb(base + RTL8139_CMD) & 0x10) != 0) {
	  }
	  printf("ethernet: reset successful \n");

	  // 0x30 is a 4 byte receive buffer start address register.
	  outl(base + 0x30, (u32)network_rx_buffer);

	  // Interrupt Mask Register
	  // 0x3c 16 bit
	  // bit 0: rx OK
	  // bit 2: tx OK
	  // note: it is important to read / write the right size, i.e.
	  // outb/outw/outl. using the wrong one results in no action.
	  outw(base + 0x3c, 0x5);

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

	  // 0xf is promiscuous mode, 0xe is normal.
	  outl(base + 0x44,
	       0xe | (1 << 7));  // wrap bit and rx flags

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

int kmain(multiboot2_information_t* mbd, u32 magic) {
  printf("kernel start=%x end=%x size=%x\n", &_kernel_start, &_kernel_end,
	 (u64)&_kernel_end - (u64)&_kernel_start);

  if (magic != 0x36d76289) {
    printf("multiboot error: %x\n", magic);
    __asm__ volatile("hlt");
  }

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

  // apic_setup();
  ioapic_setup();
  acpi_rsdp_t* rsdp = locate_rsdp();
  //  if (rsdp == nullptr) {
  //    // panic
  //  }
  //  printf("rsdp: revision: %d, rsdt_addr: %x\n", rsdp->revision,
  //  rsdp->rsdt);
  acpi_sdt_header_t* rsdt = (acpi_sdt_header_t*)rsdp->rsdt;
  //  printf("rsdt: %.*s", 4, rsdt->signature);
  //  list_tables(rsdt);
  pci_enumerate();

  setup_hpet();

  // locate_pcmp();  // This told us that bus 0 device X (ethernet) is mapped
  // to.
  //  TODO: check delivery mode. IRQ 11 (0xB).

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
  // 2. extract ps2/keyboard driver
  // 2.1. we will need some way to communicate new data to the driver task
  //
  // Extracted the mouse and keyboard driver somehow.
  // Issues:
  //
  // Next:
  // - extract similarities to ps/2 driver
  // - extract network handling
  //
  // 3. tasks/processes that have their own address space
  // 4. ?
  // 5. keyboard commands?
  // 6. two displays? kernel log and keyboard command console?
  // 7. window system?
  // 8. enable more cpus
  //
  // What's the bigger goal?
  // Network stack is extremely brittle and works with 1 packet.
  // Want to refactor it by passing packets through layers.
  // Also want to handle it outside of the interrupt handler.
  // This implies there is a task that waits for data.

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

	// type
	// 1: available RAM
	// 3: usable memory containing ACPI information
	// 4: reserved memory (needs to be preserved during hibernation)
	// 5: bad RAM
	// others: reserved area
	printf("entry %d: type = %d\n", i, e->type);
	if (e->type == 1) {
	  printf("base = %x length = %x\n", e->base_addr, e->length);
	  memory_init(e->base_addr, e->length);
	}
      }
    }
    // Tags are 8-bytes aligned.
    // ref:
    // https://www.gnu.org/software/grub/manual/multiboot2/multiboot.html#Boot-information-format-1
    h = (multiboot2_tag_header_t*)((uintptr_t)h + ((h->size + 7) & ~7));
  }

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

  // TODO: could add some inline tests here if I know available memory or
  // something.

  // Testing the message_ functions
  message* test_head = nullptr;
  u8* c6 = malloc(1);
  *c6 = 234;
  message_send(&test_head, message_type_ps2_byte, c6);
  if (test_head == nullptr) {
    return -1;
  }
  if (message_peek(test_head) == false) {
    return -1;
  }
  message test_message;
  message_receive(&test_head, &test_message);
  u8* c7 = test_message.data;
  if (*c7 != 234) {
    return -1;
  }

  if (test_head != nullptr) {
    return -1;
  }
  free(c6);

  // task_scheduler = 0x2
  // task_idle = 0x6
  // service_mouse = 0xA
  // t1 = 0xE
  // t2 = 0x12
  // TODO: also mentioned above, we probably don't want to rely on having
  // pointers to service_mouse and service_keyboard, etc.
  task_current = task_scheduler = task_new_malloc((u64)schedule);
  task_idle = task_new_malloc((u64)idle_task);
  service_mouse = task_new_malloc((u64)mouse_service);
  service_keyboard = task_new_malloc((u64)keyboard_service);
  service_network = task_new_malloc((u64)network_service);
  service_dhcp = task_new_malloc((u64)dhcp_service);
  service_dns = task_new_malloc((u64)dns_service);
  task_new_malloc((u64)task1);
  task_new_malloc((u64)task2);

  // How do we start the schedule task here? Call 'switch_task'? We will lose
  // the current stack. Can we reclaim it, or we don't care because it's so
  // small? I guess I could at least preserve it? Or the schedule task could
  // actually use it and stay the 'kernel' task, then we would just call
  // schedule here.
  printf("switching to scheduler task\n");
  task_replace(task_scheduler);

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
