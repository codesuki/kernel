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
uint8_t* videoram = (uint8_t*)0xb8000;

int xpos = 0;
int ypos = 0;

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
uint8_t terminal_buffer[num_cols * num_rows * 2 * num_pages] = {
    0};  // 100 pages

// ring buffer start. once we are at the end it needs to wrap. should be aligned
// to start of line I guess.
uint16_t terminal_buffer_start = 0;
uint16_t terminal_buffer_row_index = 0;
uint16_t terminal_buffer_column_index = 0;
uint16_t terminal_cursor_index = 0;

int printf(const char* format, ...);

// We need to wrap around and look at the end of the ring buffer if we are on
// the first screen. This is the adjusted_cursor_index.
void display() {
  for (int y = 0; y < num_rows; y++) {
    for (int x = 0; x < num_cols; x++) {
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
  for (uint32_t i = 0; i < num_rows * num_cols * 2; i++) {
    uint32_t idx =
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
      uint16_t idx = terminal_buffer_row_index * num_cols * 2 +
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
  uint64 r15, r14, r13, r12, r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx,
      rax;
  uint64 int_no, err_code;
  uint64 eip, cs, eflags, rsp, ss;  // pushed by cpu after interrupt
} __attribute__((packed));
typedef struct interrupt_registers interrupt_registers_t;

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

void memcpy(void* src, void* dst, size_t len) {
  for (int i = 0; i < len; i++) {
    ((uint8_t*)dst)[i] = ((uint8_t*)src)[i];
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
uint8_t network_rx_buffer[RX_BUFFER_SIZE + 16 + 1500]
    __attribute__((aligned(4))) = {0};
uint16_t network_rx_buffer_index = {0};

uint8_t network_packet[1518] = {0};

uint8_t network_current_tx_descriptor = 0;

uint16_t num_packets = 0;

// Now it's starting to get super dirty.
uint32_t base = 0;

uint8_t mac[6] = {0};
uint8_t broadcast_mac[6] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
uint8_t ip[4] = {0};
uint8_t broadcast_ip[4] = {0xff, 0xff, 0xff, 0xff};

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
// the address fields down as uint16_t. Because I noticed that the only thing
// that made it fail were the addresses I thought it had something to do with
// endianess. I wondered why the above resulted in the same memory layout, but
// taking a step back it became obvious. I laid out the little endian uint32_t
// value ip_2 such that as a little endian value it would be laid out like the
// address in big endian. Not sure why I was confused :)

uint8_t dhcp_identifier[4];
uint8_t dhcp_subnet_mask[4];
uint8_t dhcp_router[4];
uint8_t dhcp_router_mac[6];
uint8_t dhcp_dns[4];
uint32_t dhcp_offer_xid;

uint16_t ntohs(uint16_t netshort) {
  return (netshort >> 8) | (netshort << 8);
}

struct ethernet_frame {
  uint8_t destination_mac[6];
  uint8_t source_mac[6];
  uint16_t ethertype;  // network byte order
  // payload
  // crc
} __attribute__((packed));
typedef struct ethernet_frame ethernet_frame_t;

struct ipv4_header {
  uint8_t ihl : 4;
  uint8_t version : 4;
  uint8_t ecn : 2;   // congestion notification
  uint8_t dscp : 6;  // type of service. so routers can see what to prioritize
  // minimum is 20 with is just the ipv4 header.
  uint16_t length;                // network byte order
  uint16_t identification;        // network byte order
  uint16_t fragment_offset : 13;  // network byte order
  uint16_t flags : 3;             // network byte order
  uint8_t ttl;
  uint8_t protocol;  // ref:
  // https://en.wikipedia.org/wiki/List_of_IP_protocol_numbers
  uint16_t checksum;             // network byte order?
  uint32_t source_address;       // network byte order
  uint32_t destination_address;  // network byte order
} __attribute__((packed));
typedef struct ipv4_header ipv4_header_t;

struct udp_header {
  uint16_t source_port;       // network byte order
  uint16_t destination_port;  // network byte order
  uint16_t length;            // network byte order
  uint16_t checksum;          // network byte order
} __attribute__((packed));
typedef struct udp_header udp_header_t;

// ref: http://www.faqs.org/rfcs/rfc768.html
struct udp_pseudo_ip_header {
  uint32_t source_address;       // network byte order
  uint32_t destination_address;  // network byte order
  uint8_t zero;
  uint8_t protocol;
  uint16_t udp_length;  // network byte order
} __attribute__((packed));
typedef struct udp_pseudo_ip_header udp_pseudo_ip_header_t;

struct dhcp_message {
  uint8_t op;
  uint8_t htype;
  uint8_t hlen;
  uint8_t hops;
  uint32_t xid;        // network byte order
  uint16_t secs;       // network byte order
  uint16_t flags;      // network byte order
  uint32_t ciaddr;     // network byte order
  uint8_t yiaddr[4];   // network byte order
  uint32_t siaddr;     // network byte order
  uint32_t giaddr;     // network byte order
  uint32_t chaddr[4];  // network byte order
  uint8_t reserved[192];
  uint8_t magic[4];
} __attribute__((packed));
typedef struct dhcp_message dhcp_message_t;

struct dns_header {
  uint16_t id;
  uint8_t rd : 1;  // recursion desired
  uint8_t tc : 1;
  uint8_t aa : 1;
  // standard query (0), inverse query (1), server status request (2)
  uint8_t opcode : 4;
  uint8_t qr : 1;  // query (0), response (1)

  // response code: no error (0), format error (1), server failure (2), name
  // error (3), not implemented (4), refused (5)
  uint8_t rcode : 4;
  uint8_t z : 3;
  uint8_t ra : 1;  // recusion available

  uint16_t qdcount;
  uint16_t ancount;
  uint16_t nscount;
  uint16_t arcount;
} __attribute__((packed));
typedef struct dns_header dns_header_t;

struct dns_question {
  uint16_t qname;  // this is actually flex. so could be pretty long.
  uint16_t qtype;
  uint16_t qclass;
} __attribute__((packed));
typedef struct dns_question dns_question_t;

struct dns_resource {
  uint16_t name;
  uint16_t type;
  uint16_t class;
  uint16_t ttl;
  uint16_t rdlength;
  uint16_t rdata;
} __attribute__((packed));
typedef struct dns_resource dns_resource_t;

struct arp_message {
  uint16_t htype;  // 1 = ethernet, 0x0800 IPv4
  uint16_t ptype;
  uint8_t hlen;
  uint8_t plen;  // Protocol length in octets. IPv4 = 4
  uint16_t oper;
  uint16_t sender_mac_1;
  uint16_t sender_mac_2;
  uint16_t sender_mac_3;
  uint16_t sender_protocol_address_1;
  uint16_t sender_protocol_address_2;
  uint16_t target_mac_1;
  uint16_t target_mac_2;
  uint16_t target_mac_3;
  uint16_t target_protocol_address_1;
  uint16_t target_protocol_address_2;
} __attribute__((packed));
typedef struct arp_message arp_message_t;

struct icmp_header {
  uint8_t type;  // 8 = echo, 0 = echo reply
  uint8_t code;  // 0 = echo(?)
  uint16_t checksum;
};
typedef struct icmp_header icmp_header_t;

struct icmp_echo_message {
  uint16_t identifier;
  uint16_t sequence_number;
};
typedef struct icmp_echo_message icmp_echo_message_t;

uint16_t htons(uint16_t hostshort) {
  return (hostshort >> 8) | (hostshort << 8);
}

uint32_t htonl(uint32_t hostlong) {
  return ((hostlong & 0x000000ff) << 24) | ((hostlong & 0x0000ff00) << 8) |
	 ((hostlong & 0x00ff0000) >> 8) | ((hostlong & 0xff000000) >> 24);
}

bool strncmp(char* s1, char* s2, int n) {
  for (int i = 0; i < n; i++) {
    if (s1[i] != s2[i]) {
      return false;
    }
  }
  return true;
}

// source: https://datatracker.ietf.org/doc/html/rfc1071#section-4.1
uint16_t ipv4_checksum(uint16_t* addr, uint8_t count) {
  /* Compute Internet Checksum for "count" bytes
   *         beginning at location "addr".
   */
  uint32_t sum = 0;

  while (count > 1) {
    /*  This is the inner loop */
    sum += *addr++;
    count -= 2;
  }

  /*  Add left-over byte, if any */
  if (count > 0) {
    sum += *(uint8_t*)addr;
  }

  /*  Fold 32-bit sum to 16 bits */
  while (sum >> 16) {
    sum = (sum & 0xffff) + (sum >> 16);
  }

  return ~sum;
}

// ref: http://www.faqs.org/rfcs/rfc768.html
uint16_t udp_checksum(ipv4_header_t* ipv4h, uint16_t* addr) {
  // build pseudo header
  udp_header_t* udph = (udp_header_t*)addr;

  udp_pseudo_ip_header_t ps = {0};
  ps.source_address = ipv4h->source_address;
  ps.destination_address = ipv4h->destination_address;
  ps.protocol = ipv4h->protocol;
  ps.udp_length = udph->length;

  /* Compute Internet Checksum for "count" bytes
   *         beginning at location "addr".
   */
  uint32_t sum = 0;

  uint16_t* first = (uint16_t*)(&ps);
  for (int i = 0; i < sizeof(udp_pseudo_ip_header_t) / 2; i++) {
    sum += *first++;
  }

  uint16_t count = ntohs(udph->length);

  while (count > 1) {
    /*  This is the inner loop */
    sum += *addr++;
    count -= 2;
  }

  /*  Add left-over byte, if any */
  if (count > 0) {
    sum += *(uint8_t*)addr;
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
const uint64_t _1ms = 100000;  // we need this many timer periods to have 1 ms
const uint64_t _1s = _1ms * 1000;

void set_timer0() {
  // main counter 0xf0
  uint64_t* counter_value = (uint64_t*)(HPET_BASE + HPET_MAIN_COUNTER);
  // TODO: crash because of large number
  // printf("hpet: counter: %d", *counter_value);

  // comparator timer 0 0x108
  // printf("hpet: setting timer to %x %d\n", _1s, _1s);
  uint64_t* comparator_0 = (uint64_t*)(HPET_BASE + 0x108);
  // TODO: how is wrapping handled? By GPE :D
  *comparator_0 = *counter_value + _1ms;

  // TODO: this crashes when wrapping 64bit value. Probably formatting code
  // wrong.
  // printf("hpet: set timer to %x %d\n", *comparator_0, *comparator_0);
}

uint64_t get_global_timer_value() {
  return *(uint64_t*)(HPET_BASE + HPET_MAIN_COUNTER);
}

void setup_hpet() {
  // Timer 0: 100h – 107h, Timer 1: 120h – 127h, Timer 2: 140h – 147h
  uint32_t* available_interrupts = (uint32_t*)(HPET_BASE + 0x104);
  printf("hpet: interrupts timer 0: %x\n", *available_interrupts);

  uint32_t* timer_0 =
      (uint32_t*)(HPET_BASE + 0x100);  // set bit 2 and maybe 9-13
  printf("hpet: configured interrupt: %x\n", ((*timer_0) >> 9) & 31);
  *timer_0 = (*timer_0) | (1 << 2) | (4 << 9);
  printf("hpet: configured interrupt: %x\n", ((*timer_0) >> 9) & 31);

  uint32_t* timer_period = (uint32_t*)(HPET_BASE + 0x4);
  printf("hpet: femto: %d\n", *timer_period);

  // main counter 0xf0
  uint64_t* counter_value = (uint64_t*)(HPET_BASE + HPET_MAIN_COUNTER);
  printf("hpet: counter: %d", *counter_value);

  // comparator timer 0 0x108
  printf("hpet: setting timer to %x %d\n", _1s, _1s);
  uint64_t* comparator_0 = (uint64_t*)(HPET_BASE + 0x108);
  *comparator_0 = _1s;
  printf("hpet: set timer to %x %d\n", *comparator_0, *comparator_0);

  // bit 0 is enable flag
  uint32_t* c = (uint32_t*)(HPET_BASE + HPET_CONFIG_REG);
  printf("hpet: config %x\n", *c);
  *c = (*c) | 0x1;
  printf("hpet: config %x\n", *c);
  printf("hpet: counter: %d\n", *counter_value);
}

void net_transmit(void* data, uint32_t length) {
  // set address to descriptor
  // set size
  // set 0 to own
  // printf("network: tx: using descriptor %d\n",
  // network_current_tx_descriptor);
  outl(base + 0x20 + network_current_tx_descriptor * 4, (uint32_t)data);
  // uint32_t a = inl(base + 0x20);
  // printf("TX addr: %x\n", a);
  // bit 0-12 = size, bit 13 = own
  outl(base + 0x10 + network_current_tx_descriptor * 4, length);
  // wait for TOK
  while (inl(base + 0x10 + network_current_tx_descriptor * 4) &
	 (1 << 15) == 0) {
  }

  network_current_tx_descriptor = ++network_current_tx_descriptor % 4;
}

uint8_t packet[1024] = {0};

void send_dhcp_discover() {
  // build a DHCP packet and send it.
  for (int i = 0; i < 1024; i++) {
    packet[i] = 0;
  }

  // need memcpy
  ethernet_frame_t* ef = (ethernet_frame_t*)packet;
  memcpy(mac, ef->source_mac, 6);
  memcpy(broadcast_mac, ef->destination_mac, 6);

  ef->ethertype = htons(0x0800);

  ipv4_header_t* iph = (ipv4_header_t*)(packet + sizeof(ethernet_frame_t));
  iph->version = 4;
  iph->ihl = 5;  // no options
  iph->ttl = 100;
  iph->protocol = 0x11;                   // UDP
  iph->source_address = 0;                // should be network byte order
  iph->destination_address = 0xffffffff;  // should be network byte order

  udp_header_t* udph =
      (udp_header_t*)(packet + sizeof(ethernet_frame_t) + iph->ihl * 4);
  udph->source_port = htons(68);
  udph->destination_port = htons(67);

  dhcp_message_t* dhcpm =
      (dhcp_message_t*)(packet + sizeof(ethernet_frame_t) + iph->ihl * 4 +
			sizeof(udp_header_t));

  // DHCPDISCOVER
  dhcpm->op = 0x01;
  dhcpm->htype = 0x01;
  dhcpm->hlen = 0x06;
  dhcpm->hops = 0x00;
  dhcpm->xid = (uint32_t)get_global_timer_value();
  dhcpm->secs = 0;
  dhcpm->flags = 0;
  // MAC
  dhcpm->chaddr[0] = (mac[3] << 24) | (mac[2] << 16) | (mac[1] << 8) | mac[0];
  dhcpm->chaddr[1] = (mac[5] << 8) | mac[4];
  dhcpm->magic[0] = 0x63;
  dhcpm->magic[1] = 0x82;
  dhcpm->magic[2] = 0x53;
  dhcpm->magic[3] = 0x63;

  uint8_t* options = packet + sizeof(ethernet_frame_t) + iph->ihl * 4 +
		     sizeof(udp_header_t) + sizeof(dhcp_message_t);

  options[0] = 53;
  options[1] = 1;
  options[2] = 1;
  options[3] = 0xff;

  udph->length = htons(sizeof(udp_header_t) + sizeof(dhcp_message_t) +
		       4 /* options */);  // minimum, only header.

  // TODO: this is not correct
  // Checksum is the 16-bit one's complement of the one's complement sum of a
  // pseudo header of information from the IP header, the UDP header, and the
  // data, padded with zero octets at the end (if necessary) to make a multiple
  // of two octets.
  // ref: https://datatracker.ietf.org/doc/html/rfc768
  udph->checksum = udp_checksum(iph, (uint16_t*)udph);

  iph->length = htons(iph->ihl * 4 + ntohs(udph->length));

  iph->checksum = ipv4_checksum((uint16_t*)iph, iph->ihl * 4);

  net_transmit(packet, sizeof(ethernet_frame_t) + ntohs(iph->length));
}

void send_dhcp_request() {
  // we have the DHCPOFFER reply parameters in the global variables

  // TODO: now lots of duplication starts. refactor.
  // build a DHCP packet and send it.
  for (int i = 0; i < 1024; i++) {
    packet[i] = 0;
  }

  // TODO: duplicated
  ethernet_frame_t* ef = (ethernet_frame_t*)packet;
  memcpy(mac, ef->source_mac, 6);
  memcpy(broadcast_mac, ef->destination_mac, 6);

  ef->ethertype = htons(0x0800);

  ipv4_header_t* iph = (ipv4_header_t*)(packet + sizeof(ethernet_frame_t));
  iph->version = 4;
  iph->ihl = 5;  // no options
  iph->ttl = 100;
  iph->protocol = 0x11;     // UDP
  iph->source_address = 0;  // 0xfeffffff;       // should be network byte order
  iph->destination_address = 0xffffffff;  // should be network byte order

  udp_header_t* udph =
      (udp_header_t*)(packet + sizeof(ethernet_frame_t) + iph->ihl * 4);
  udph->source_port = htons(68);
  udph->destination_port = htons(67);

  dhcp_message_t* dhcpm =
      (dhcp_message_t*)(packet + sizeof(ethernet_frame_t) + iph->ihl * 4 +
			sizeof(udp_header_t));

  // DHCPDISCOVER
  dhcpm->op = 0x03;  // DIFFERENT
  dhcpm->htype = 0x01;
  dhcpm->hlen = 0x06;
  dhcpm->hops = 0x00;
  dhcpm->xid = dhcp_offer_xid;
  dhcpm->secs = 0;
  dhcpm->flags = 0;
  dhcpm->chaddr[0] = (mac[3] << 24) | (mac[2] << 16) | (mac[1] << 8) | mac[0];
  dhcpm->chaddr[1] = (mac[5] << 8) | mac[4];
  dhcpm->siaddr = (dhcp_router[3] << 24) | (dhcp_router[2] << 16) |
		  (dhcp_router[1] << 8) | dhcp_router[0];
  //(dhcp_router[3] << 24) | (dhcp_router[2] << 16) |
  // (dhcp_router[1] << 8) | dhcp_router[0];
  dhcpm->magic[0] = 0x63;
  dhcpm->magic[1] = 0x82;
  dhcpm->magic[2] = 0x53;
  dhcpm->magic[3] = 0x63;

  uint8_t* options = packet + sizeof(ethernet_frame_t) + iph->ihl * 4 +
		     sizeof(udp_header_t) + sizeof(dhcp_message_t);

  options[0] = 53;  // DHCPREQUEST
  options[1] = 1;
  options[2] = 3;
  options[3] = 50;  // ip request
  options[4] = 4;
  options[5] = ip[0];
  options[6] = ip[1];
  options[7] = ip[2];
  options[8] = ip[3];
  options[9] = 54;  // dhcp server
  options[10] = 4;
  options[11] = dhcp_router[0];
  options[12] = dhcp_router[1];
  options[13] = dhcp_router[2];
  options[14] = dhcp_router[3];
  options[15] = 0xff;

  udph->length = htons(sizeof(udp_header_t) + sizeof(dhcp_message_t) +
		       16 /* options */);  // minimum, only header.

  // TODO: this is not correct
  // Checksum is the 16-bit one's complement of the one's complement sum of a
  // pseudo header of information from the IP header, the UDP header, and the
  // data, padded with zero octets at the end (if necessary) to make a multiple
  // of two octets.
  // ref: https://datatracker.ietf.org/doc/html/rfc768
  udph->checksum = udp_checksum(iph, (uint16_t*)udph);

  iph->length = htons(iph->ihl * 4 + ntohs(udph->length));

  iph->checksum = ipv4_checksum((uint16_t*)iph, iph->ihl * 4);

  net_transmit(packet, sizeof(ethernet_frame_t) + ntohs(iph->length));
}

#define ETHERTYPE_IP4 0x0800
#define ETHERTYPE_ARP 0x0806
void send_dns_request() {
  // TODO: more duplication!
  for (int i = 0; i < 1024; i++) {
    packet[i] = 0;
  }

  // TODO: duplicated
  ethernet_frame_t* ef = (ethernet_frame_t*)packet;
  memcpy(mac, ef->source_mac, 6);
  memcpy(dhcp_router_mac, ef->destination_mac, 6);

  ef->ethertype = htons(ETHERTYPE_IP4);

  // TODO: factory function
  ipv4_header_t* iph = (ipv4_header_t*)(packet + sizeof(ethernet_frame_t));
  iph->version = 4;
  iph->ihl = 5;  // no options
  iph->ttl = 100;
  iph->protocol = 0x11;  // UDP
  iph->source_address = (ip[3] << 24) | (ip[2] << 16) | (ip[1] << 8) | ip[0];
  iph->destination_address = (dhcp_dns[3] << 24) | (dhcp_dns[2] << 16) |
			     (dhcp_dns[1] << 8) | dhcp_dns[0];

  udp_header_t* udph =
      (udp_header_t*)(packet + sizeof(ethernet_frame_t) + iph->ihl * 4);
  udph->source_port = htons(53);
  udph->destination_port = htons(53);

  dns_header_t* dnsh = (dns_header_t*)(packet + sizeof(ethernet_frame_t) +
				       iph->ihl * 4 + sizeof(udp_header_t));

  dnsh->id = htons(5);
  dnsh->qdcount = htons(1);
  dnsh->rd = 1;

  uint8_t* q = (uint8_t*)(dnsh + 1);
  q[0] = 6;
  q[1] = 'g';
  q[2] = 'o';
  q[3] = 'o';
  q[4] = 'g';
  q[5] = 'l';
  q[6] = 'e';
  q[7] = 3;
  q[8] = 'c';
  q[9] = 'o';
  q[10] = 'm';
  q[11] = 0;
  q[12] = 0;
  q[13] = 1;
  q[14] = 0;
  q[15] = 1;

  udph->length = htons(sizeof(udp_header_t) + sizeof(dns_header_t) + 16);

  // Checksum is the 16-bit one's complement of the one's complement sum of a
  // pseudo header of information from the IP header, the UDP header, and the
  // data, padded with zero octets at the end (if necessary) to make a multiple
  // of two octets.
  // ref: https://datatracker.ietf.org/doc/html/rfc768
  udph->checksum = udp_checksum(iph, (uint16_t*)udph);

  iph->length = htons(iph->ihl * 4 + ntohs(udph->length));

  iph->checksum = ipv4_checksum((uint16_t*)iph, iph->ihl * 4);

  net_transmit(packet, sizeof(ethernet_frame_t) + ntohs(iph->length));
}

void send_arp_response(uint8_t sender_mac[6], uint8_t sender_ip[4]) {
  // TODO: more duplication!
  for (int i = 0; i < 1024; i++) {
    packet[i] = 0;
  }

  // TODO: duplicated
  ethernet_frame_t* ef = (ethernet_frame_t*)packet;
  memcpy(mac, ef->source_mac, 6);
  memcpy(sender_mac, ef->destination_mac, 6);

  ef->ethertype = htons(ETHERTYPE_ARP);

  // TODO: factory function
  arp_message_t* a = (arp_message_t*)(packet + sizeof(ethernet_frame_t));
  a->htype = htons(1);  // ethernet
  a->hlen = 6;
  a->ptype = htons(ETHERTYPE_IP4);
  a->plen = 4;
  a->oper = htons(2);  // reply
  memcpy(mac, &a->sender_mac_1, 6);
  memcpy(ip, &a->sender_protocol_address_1, 4);
  memcpy(sender_mac, &a->target_mac_1, 6);
  memcpy(sender_ip, &a->target_protocol_address_1, 4);

  net_transmit(packet, sizeof(ethernet_frame_t) + sizeof(arp_message_t));
}

void send_echo() {
  // TODO: more duplication!
  for (int i = 0; i < 1024; i++) {
    packet[i] = 0;
  }

  // TODO: duplicated
  ethernet_frame_t* ef = (ethernet_frame_t*)packet;
  memcpy(mac, ef->source_mac, 6);
  memcpy(dhcp_router_mac, ef->destination_mac, 6);

  ef->ethertype = htons(ETHERTYPE_IP4);

  uint8_t google_ip[4] = {142, 250, 207, 46};
  // TODO: factory function
  ipv4_header_t* iph = (ipv4_header_t*)(packet + sizeof(ethernet_frame_t));
  iph->version = 4;
  iph->ihl = 5;  // no options
  iph->ttl = 100;
  iph->protocol = 0x1;  // ICMP
  iph->source_address = (ip[3] << 24) | (ip[2] << 16) | (ip[1] << 8) | ip[0];
  //  iph->destination_address = 0xffffffff;  // should be network byte order
  iph->destination_address = (google_ip[3] << 24) | (google_ip[2] << 16) |
			     (google_ip[1] << 8) | google_ip[0];

  icmp_header_t* h =
      (icmp_header_t*)(packet + sizeof(ethernet_frame_t) + iph->ihl * 4);
  h->type = 8;
  h->code = 0;

  icmp_echo_message_t* m = (icmp_echo_message_t*)(h + 1);
  m->identifier = 0x1;
  m->sequence_number = 0;

  h->checksum = ipv4_checksum(
      (uint16_t*)h, sizeof(icmp_header_t) + sizeof(icmp_echo_message_t));

  iph->length =
      htons(iph->ihl * 4 + sizeof(icmp_header_t) + sizeof(icmp_echo_message_t));

  iph->checksum = ipv4_checksum((uint16_t*)iph, iph->ihl * 4);

  net_transmit(packet, sizeof(ethernet_frame_t) + ntohs(iph->length));
}

void net_handle_arp(arp_message_t* a) {
  printf("arp: htype: %x, ptype: %x, hlen: %x, plen: %x, oper: %x\n",
	 ntohs(a->htype), ntohs(a->ptype), a->hlen, a->plen, ntohs(a->oper));

  if (ntohs(a->oper) == 1) {  // request
    // check if our IP matches
    uint8_t target_ip[4] = {0};
    memcpy(&a->target_protocol_address_1, target_ip, 4);
    printf("arp: target IP: %d.%d.%d.%d\n", target_ip[0], target_ip[1],
	   target_ip[2], target_ip[3]);

    // because my strncmp works different than stdlib this works
    uint8_t sender_mac[6] = {0};
    memcpy(&a->sender_mac_1, sender_mac, 6);
    uint8_t sender_ip[4] = {0};
    memcpy(&a->sender_protocol_address_1, sender_ip, 4);
    if (strncmp(target_ip, ip, 4)) {
      send_arp_response(sender_mac, sender_ip);
    }
  }
}

void net_handle_dns(dns_header_t* h) {
  printf("dns: id: %x, opcode: %x, qr: %x acount: %x\n", ntohs(h->id),
	 h->opcode, h->qr, ntohs(h->ancount));

  // read questions (are repeated in response messages)
  for (int i = 0; i < ntohs(h->qdcount); i++) {
  }
  // read answers
  for (int i = 0; i < ntohs(h->ancount); i++) {
  }
  send_echo();
}

void net_handle_dhcp(ethernet_frame_t* ef, dhcp_message_t* m) {
  printf("dhcp: op: %x htype: %x hlen: %x hops: %x xid: %x\n", m->op, m->htype,
	 m->hlen, m->hops, m->xid);

  if (m->op != 2) {  // reply
    return;
  }

  uint8_t* o = (uint8_t*)(m + 1);  // end of dhcp message for options

  // TODO: double check that we don't go over length of message.

  uint8 dhcp_type = 0;

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

#define DHCP_OFFER 2
#define DHCP_ACK 5
  if (dhcp_type == DHCP_OFFER) {
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

    printf("dhcp: sending DHCPREQUEST\n");
    dhcp_offer_xid = m->xid;
    send_dhcp_request();
  } else if (dhcp_type == DHCP_ACK) {
    printf("dhcp: received DHCPACK\n");
    printf("dhcp: target_mac: %x:%x:%x:%x:%x:%x\n", ef->destination_mac[0],
	   ef->destination_mac[1], ef->destination_mac[2],
	   ef->destination_mac[3], ef->destination_mac[4],
	   ef->destination_mac[5]);
    printf("dhcp: source_mac: %x:%x:%x:%x:%x:%x\n", ef->source_mac[0],
	   ef->source_mac[1], ef->source_mac[2], ef->source_mac[3],
	   ef->source_mac[4], ef->source_mac[5]);
    memcpy(ef->source_mac, dhcp_router_mac, 6);
    send_dns_request();
  }
}

void net_handle_udp(ethernet_frame_t* ef, udp_header_t* udph) {
  printf("udp: src port: %d dst port: %d\n", ntohs(udph->source_port),
	 ntohs(udph->destination_port));

  if (ntohs(udph->destination_port) == 53) {  // DNS
    net_handle_dns((dns_header_t*)(udph + 1));
  } else if (ntohs(udph->source_port) == 67 &&
	     ntohs(udph->destination_port) == 68) {  // DHCP
    net_handle_dhcp(ef, (dhcp_message_t*)(udph + 1));
  }
}

#define NET_PROTOCOL_ICMP 0x1
#define NET_PROTOCOL_UDP 0x11

void net_handle_ipv4(ethernet_frame_t* ef, ipv4_header_t* iph) {
  printf("ipv4: version: %x ihl: %x\n", iph->version, iph->ihl);

  // printf("ipv4: protocol: %x\n", iph->protocol);

  if (iph->protocol == NET_PROTOCOL_ICMP) {
  } else if (iph->protocol == NET_PROTOCOL_UDP) {
    net_handle_udp(ef, (udp_header_t*)(iph + 1));
  }
}

typedef struct task task_t;
struct task {
  uint8_t id;
  uint64_t rsp;
  uint64_t eip;
  uint64_t rax;
  uint64_t rbx;
  uint64_t rcx;
  uint64_t rdx;
  uint64_t rsi;
  uint64_t rdi;
  uint64_t rbp;
  uint64_t r8;
  uint64_t r9;
  uint64_t r10;
  uint64_t r11;
  uint64_t r12;
  uint64_t r13;
  uint64_t r14;
  uint64_t r15;
  task_t* next;
  uint64_t sleep_until;
};

task_t* task_first = 0;
task_t* task_current = 0;

void trampoline();

extern void switch_task(task_t* current, task_t* next);

// TODO: added task prefix for namespacing. Check C best practices.
void task_setup_stack(uint8_t* stack) {
  // TODO: we could null it, but maybe that's too much work, also, how big is it
  // even?
  uint64_t* s = (uint64_t*)stack;
  s[0] = (uint64_t)&trampoline;
}

// Defined in linker script.
// Need to take the address, because the address is the value in this case.
// ref: https://sourceware.org/binutils/docs/ld/Source-Code-Reference.html
extern uint64_t _kernel_start;
extern uint64_t _kernel_end;

const uint32_t page_size = 0x200000;  // 2mb

typedef struct memory memory_t;
struct memory {
  uint64_t address;
  // uint32_t size;  // 4kb, 2mb, etc.
  memory_t* next;
  memory_t* prev;
};

// Current thought, keep two lists. Free and used memory.
// When we malloc we take from the free list and put it into the used list.
// When we free we search the address in the used list and move it back.
// What other way is there to implement free?
memory_t* memory_free_first = NULL;
memory_t* memory_used_first = NULL;

// memory_init creates the first memory_ts in physical memory, because we cannot
// call malloc.
void memory_init(uint64_t base_address, uint64_t length) {
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
  // uint64_t fixed it. This caused the memory_ts to be allocated on kernel
  // memory which caused memory corruption.
  uint64_t base_address_after_kernel =
      base_address + ((uint64_t)&_kernel_end - (uint64_t)&_kernel_start);

  printf("memory_init: available memory with base %x and length %x\n",
	 base_address, length);

  printf("memory_init: kernel located between %x - %x\n", &_kernel_start,
	 &_kernel_end);

  // TODO: I think this overcounts. I.e. the last page is too big for the
  // available physical memory.
  // Maybe for now we don't track the memory_ts themselves.
  // How many do we need?
  uint64_t page_count = length / page_size;
  uint64_t memory_size = page_count * sizeof(memory_t);

  // After our memory_ts
  // Aligned on page_size.
  uint64_t usable_memory =
      ((base_address_after_kernel + memory_size + (page_size - 1)) &
       ~(page_size - 1));

  printf("memory_init: %d 2mb pages available\n", page_count);
  printf("memory_init: usable memory starts from %x\n", usable_memory);

  memory_t* current = (memory_t*)base_address_after_kernel;
  memory_t* prev = NULL;

  memory_free_first = current;

  for (uint64_t i = 0; i < page_count; i++) {
    current->address = usable_memory;
    current->next = NULL;
    current->prev = prev;

    if (prev != NULL) {
      prev->next = current;
    }

    // Nice in Bochs because we only have 30mb there.
    // printf("memory_init: assigned %x\n", m->address);

    usable_memory += page_size;
    prev = current;
    current++;
  }
  for (memory_t* m = memory_free_first; m != NULL; m = m->next) {
    printf("memory_init: %x %x %x\n", m->address, m->next, m->prev);
  }
}

void memory_add(uint64_t address) {
  if (memory_free_first == NULL) {
    memory_used_first = NULL;
    // Ouch! We want a new memory_t. Now do we malloc one? Haha.. Maybe the
    // kernel needs to reserve some memory beforehand for some kind of
    // bootstrap?
    // So when we read available memory, we remove the kernel area.
    // Then we know the real start of the memory.
    // Then we can just do memory_t * m = start of memory;
    // And so on.
  }
}

memory_t* memory_remove() {
  if (memory_free_first == NULL) {
    return NULL;
  }
  memory_t* memory = memory_free_first;
  memory_free_first = memory->next;
  memory_free_first->prev = memory->prev;
  return memory;
}

void* malloc(uint64_t size) {
  // What does malloc do? It probably does different things depending on whether
  // it's in the kernel or not.
  // Currently we only have kernel.

  // Let's assume we know all of physical memory that is available. We need to
  // pick a slot that is big enough to fit size. Currently our pages are all
  // 2mb, but it doesn't matter here. Let's start simple and waste memory. When
  // we request, we just return a 2mb block or multiple if needed.
  // it's important that the memory is contiguous.

  memory_t* memory = memory_remove();
  if (memory == NULL) {
    printf("malloc: OOM\n");
  }
  printf("malloc: %x\n", (void*)(memory->address));
  return (void*)(memory->address);
}

// TODO: actually we want to allocate a new stack/task now..
void task_new(uint64_t entry_point, uint8_t* stack, task_t* task) {
  printf("New task %x\n", entry_point);
  // memset to 0
  task->eip = entry_point;
  task->rsp = (uint64_t)stack;
  task_setup_stack(stack);

  if (task_first == NULL) {
    // First task
    task_first = task;
    task_first->next = task_first;
  } else {
    // Add to end of list
    task_t* t = task_first;
    for (; t->next != task_first; t = t->next) {
    }
    t->next = task;
    task->next = task_first;
  }
}

task_t* task_new_malloc(uint64_t entry_point) {
  task_t* task = (task_t*)malloc(sizeof(task_t));
  uint8_t* stack = (uint8_t*)malloc(8192);
  task_new(entry_point, stack, task);
  return task;
}

void task_remove(task_t* task) {
  printf("Removing task %d\n", task->id);

  // Case 1: It's the first task
  if (task_first == task) {
    task_first = task->next;
  }

  // Case 2: It's in the middle or the last task. Behavior is the same.
  task_t* t = task_first;
  for (; t->next != NULL; t = t->next) {
    if (t->next == task) {
      t->next = task->next;
      return;
    }
  }
}

void sleep(uint64_t ms) {
  // how do I find the task that this should apply
  // let's assume it's current
  task_current->sleep_until = get_global_timer_value() + ms * _1ms;
  // We want to reschedule after the sleep because the current task becomes
  // blocking.
  // reschedule()
  // let's call hlt for now
  asm("HLT");
  // what if the schedule timer fires while we reschedule?
}

void print_task(task_t* task) {
  printf(
      "task: rsp = %x, eip = %x\nrax = %x, rcx = %x, rdx = %x\nrsi = %x, rdi = "
      "%x, r8 = %x\nr9 = %x, r10 = %x, r11 = %x\n",
      task->rsp, task->eip, task->rax, task->rcx, task->rdx, task->rsi,
      task->rdi, task->r8, task->r9, task->r10, task->r11);
}

void print_regs(interrupt_registers_t* regs) {
  printf(
      "regs:  rsp = %x, eip = %x\nrax = %x, rcx = %x, rdx = %x\nrsi = %x, rdi "
      "= %x, r8 = %x\nr9 = %x, r10 = %x, r11 = %x\n",
      regs->rsp, regs->eip, regs->rax, regs->rcx, regs->rdx, regs->rsi,
      regs->rdi, regs->r8, regs->r9, regs->r10, regs->r11);
}

void task_update_context(task_t* task, interrupt_registers_t* regs) {
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

  // printf("task after\n");
  // print_task(task);
}

// I guess this should be normalized and just one function, from -> to.
void update_regs_from_task(task_t* task, interrupt_registers_t* regs) {
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

  // printf("regs after\n");
  // print_regs(regs);
}

uint8_t kernel_task_stack[8192] = {};
uint8_t t1_stack[8192] = {};
uint8_t t2_stack[8192] = {};

void task1(uint8_t id);
void task2(uint8_t id);
void kernel_task();

// My guess is that we probably want to 'lose' the initial kernel loader task.
// Which means we create a new stack and switch to a new task that we define
// here. What happens with the initial kernel stack? Can we clean it up somehow?
// It won't be needed anymore because we jump out of kmain.
task_t kernel = {
    .id = 123,
    .rsp = (uint64_t)&kernel_task_stack,
    .eip = (uint64_t)kernel_task,
    .rax = 0,
    .rcx = 0,
    .rdx = 0,
    .rsi = 0,
    .rdi = 0,
    .r8 = 0,
    .r9 = 0,
    .r10 = 0,
    .r11 = 0,
};

task_t t1 = {
    .id = 15,
    .rsp = (uint64_t)&t1_stack,
    .eip = (uint64_t)task1,
    .rax = 0,
    .rcx = 0,
    .rdx = 0,
    .rsi = 0,
    .rdi = 0,
    .r8 = 0,
    .r9 = 0,
    .r10 = 0,
    .r11 = 0,
};

task_t t2 = {
    .id = 20,
    .rsp = (uint64_t)&t2_stack,
    .eip = (uint64_t)task2,
    .rax = 0,
    .rcx = 0,
    .rdx = 0,
    .rsi = 0,
    .rdi = 0,
    .r8 = 0,
    .r9 = 0,
    .r10 = 0,
    .r11 = 0,
};

void trampoline() {
  printf("finished a task: %d\n", task_current->id);
  // TODO
  // remove task
  // switch to scheduler
  // Do we switch directly or just hlt and wait for timer interrupt?
  //  task_current =
  //  switch_task(&t2, &kernel);

  task_remove(task_current);
  asm("HLT");
}

void kernel_task() {
  printf("kernel task\n");
  asm("HLT");
}

void task1(uint8_t id) {
  while (1) {
    //	for (uint32_t i = 0; i < 1000; i++) {

    // TODO: there seems to be a bug. if we write tons of lines it just turns
    // black.
    printf("running task 1 %d\n", id);
    sleep(1000);
  }
  // switch_task(&t1, &t2);
}

void task2(uint8_t id) {
  printf("running task 2 %d\n", id);
  //    switch_task(&t1, &t2);

  // TODO: I had a bug where the kernel task still had task 1 as next which
  // was finished. It tried to change to the finished task which caused a
  // double fault.
  //  kernel.next = &kernel;
}

void dns_resolve(char* host, uint8_t addr[4]) {}

void task_network() {
  // what do I want to do here?
  // I want to improve memory management and task management.
  // Using the network stack can help.

  uint8_t addr[4];
  // This will cause a bunch of network requests. We cannot do this before we
  // got a network address ourselves so I need to run a dhcp task.
  dns_resolve("google.com", addr);
  printf("google.com IP=%d.%d.%d.%d\n");
}

// Is this the network driver? No. The network driver would fetch data from the
// card and pass it up the network stack.
// This is huge. Maybe I should start with the mouse driver?
void handle_network_interrupt() {}

void service_network() {
  // Why do I call this service? I imagine the OS always keeps the network stack
  // in shape. Does DHCP stuff, etc. Can the kernel do this directly? Well it
  // needs to do many network calls so it blocks.. Hence modeling it as a task
  // or service seems to make sense?

  // Reply to ARP messages?

  // -> dhcp discover
  // <- dhcp offer
  // -> dhcp request
}

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
void mouse_handle_interrupt() {
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

  // Now that we read all data, let's send it up. How? Do we create a struct and
  // put in this data? It needs to live somewhere until the scheduler runs a
  // task that consumes it. Do we enqueue the task here? Or maybe it's the mouse
  // service that blocks on some queue? event queue?
  // Should probably have a type, like 'mouse_data' and a struct.

  // the 9 bit two's complements relative x,y values come in 2 pieces.
  // an 8 bit value and a sign bit.
  // wikipedia says to subtract the sign bit. extract and subtract.
}

void mouse_service() {
  // somehow we need to run this or this needs to run in the background waiting
  // for mouse data.

  // x,y,data is what we need.
  /*
    int16_t rel_x = x - ((data << 4) & 0x100);

    int16_t rel_y = -(y - ((data << 3) & 0x100));

    mouse_x = min(max(0, mouse_x + rel_x), 79);
    mouse_y = min(max(1, mouse_y + rel_y), 24);
  */
  /*
  ypos = mouse_y;
  xpos = mouse_x;
  print_character_color('o', 0x04);

  cll(0);
  ypos = 0;
  xpos = 0;
  printf("data: %x x: %d, y: %d, rel_x: %d, rel_y: %d", data, mouse_x, mouse_y,
  rel_x, rel_y);
  */
}

void local_apic_eoi() {
  volatile uint32_t* local_apic_eoi = (volatile uint32_t*)0xfee000b0;
  *local_apic_eoi = 0;
}

// regs is passed via rdi
void interrupt_handler(interrupt_registers_t* regs) {
  // Timer IRQ
  if (regs->int_no == 0x34) {
    // TODO: this crashes if task_current == NULL.

    // printf("timer\n");
    // This is our schedule timer. We have all registers on the stack inside of
    // `regs`. To switch the task we want to update the current tasks context
    // with `regs`.

    // TODO: when we remove a task it's not in the queue anymore but we still
    // write here.
    // Speaks to cleaning it up async once we really changed away from it.

    // We need to know the current task, let's assume we have it in a variable.
    task_update_context(task_current, regs);
    // Now change to the next task. Assume it's in task.next. The interrupt
    // handler will pop all these registers and call iret. To actually switch
    // the tasks we need to change the stack to contain the registers of the
    // next task.
    // We can actually change the regs that are on the stack via `regs`. Why
    // does it take a few minutes that I even see this?

    // Instead of just picking the next task lets iterate until we find a good
    // task, because tasks may sleep now.
    // We should probably now loop around to the beginning.
    uint64_t now = get_global_timer_value();
    task_t* task = task_current->next;
    // I expect that the idle task will always be ready.
    for (; task != task_current; task = task->next) {
      // This task is sleeping
      if (task->sleep_until > now) {
	continue;
      }
      break;
    }
    task_current = task;

    update_regs_from_task(task_current, regs);

    // printf("switched task to %d\n", task_current->id);

    set_timer0();

    // Come to think of it.. we could unconditinally switch to the scheduler
    // task and the scheduler task could then switch to other tasks. But we
    // would still need to save the context.

    //    __asm__ volatile("hlt" : :);

    // TODO: is there a better way to do this?
    // The interrupt handler should do this. Maybe we don't always do this?
    volatile uint32_t* local_apic_eoi = (volatile uint32_t*)0xfee000b0;
    *local_apic_eoi = 0;
    return;
  }
  if (regs->int_no == 0x33) {  // network IRQ
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

    uint16_t isr = inw(base + 0x3e);
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

    while (true) {
      uint8_t cmd = inb(base + 0x37);
      if (cmd & 0x1) {  // Buffer Empty = 1
	break;
      }

      uint16_t capr = inw(base + 0x38);
      // printf("capr: %x\n", capr);

      uint16_t cbr = inw(base + 0x3a);
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

      uint16_t* packet_status =
	  (uint16_t*)(network_rx_buffer + network_rx_buffer_index);

      //  printf("network interrupt: num: %d, status: %x, buffer index: %x\n",
      //	     num_packets++, *packet_status, network_rx_buffer_index);

      // CRC, RUNT, LONG, FAE, BAD SYMBOL errors
      if (*packet_status & (1 << 1) || *packet_status & (1 << 2) ||
	  *packet_status & (1 << 3) || *packet_status & (1 << 4) ||
	  *packet_status & (1 << 5)) {
	break;
      }

      // TODO: pull length and etheretype from this.
      ethernet_frame_t* ef =
	  (ethernet_frame_t*)(network_rx_buffer + network_rx_buffer_index +
			      4);  // first two bytes are the rx header followed
      // by 2 bytes for length

      uint16_t* length =
	  (uint16_t*)(network_rx_buffer + network_rx_buffer_index +
		      2);  // first two bytes are the rx header
      //  printf("length: %d\n", *length);

      uint8_t* p = (uint8_t*)(network_rx_buffer + network_rx_buffer_index +
			      4);  // points at destination MAC

      //  uint8_t destination_mac[6] = {0};
      //   memcpy(p, destination_mac, 6);
      p += 6;

      //   uint8_t source_mac[6] = {0};
      //  memcpy(p, source_mac, 6);
      p += 6;

      //     printf("network: idx before: %x\n", network_rx_buffer_index);

      network_rx_buffer_index +=
	  *length + 4;  // length seems to not include the header and size
      // which are 2 bytes each

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

      uint16_t ether_type = ntohs(*(uint16_t*)p);

      // printf("ethertype: host byte order: %x network byte order: %x\n",
      //	     ether_type, *(uint16_t*)p);

      p += 2;  // protocol header

      if (ether_type == ETHERTYPE_ARP) {
	net_handle_arp((arp_message_t*)p);
      } else if (ether_type == ETHERTYPE_IP4) {
	net_handle_ipv4(ef, (ipv4_header_t*)p);
      }

      // the doc says to subtract 0x10 to avoid overflow. also, given we use
      // network_rx_buffer_index which is now bigger, won't this break us
      // reading packets? for some reason this is added back
      // https://github.com/qemu/qemu/blob/master/hw/net/rtl8139.c#L2522 so if
      // if we don't subtract the numbers don't match. who knows why this is
      // done.
      outw(base + 0x38, network_rx_buffer_index - 0x10);
    }
  eth_return:
    volatile uint32_t* local_apic_eoi = (volatile uint32_t*)0xfee000b0;
    *local_apic_eoi = 0;
    return;
  }

  if (regs->int_no == 0x32) {  // mouse IRQ

    // How do we notify the driver? Think about it. Can a device have multiple
    // drivers? Maybe, but I cannot see why, they would probably interfere. So
    // assume 1 driver per device. Every device has an interrupt. The driver can
    // be connected to this interrupt. 1:1 mapping.
    // So here we need to do what? Wake the driver that has a specific
    // interrupt? Where do they run?

    // Good points about why we probably should read data here. The mouse /
    // keyboard has limited buffer size and if we don't read the data inside
    // this handler it will be overridden and we lose data. For mouse position
    // it may not mean much, but for keyboard interrupts we may lose keys. So
    // given there are many different mice. A driver needs some function that is
    // called inside this handler. There's no special data we get from this
    // interrupt. Only that it's 0x32 from the mouse.

    // A lambda task would seem nice here. Bind the values to it and run it
    // deferred. Can I have a task that has parameters?
    // Doing one parameter which is a pointer to a struct is simple.
    // Many.. seems annoying.

    mouse_handle_interrupt();
    volatile uint32_t* local_apic_eoi = (volatile uint32_t*)0xfee000b0;
    *local_apic_eoi = 0;
    return;
  }
  if (regs->int_no == 0x31) {  // keyboard IRQ
    // ref: https://wiki.osdev.org/%228042%22_PS/2_Controller
    // read from port 0x60? this is the data port
    // 0x64 is the status register if read and command register if written.
    uint8 scancode = inb(0x60);

    printf("scancode: %x\n", scancode);
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
    volatile uint32_t* local_apic_eoi = (volatile uint32_t*)0xfee000b0;
    *local_apic_eoi = 0;
    return;
  }

  if (regs->int_no < 15) {
    printf("interrupt: %s\n", interrupt_names[regs->int_no]);
  } else {
    printf("interrupt: %x\n", regs->int_no);
  }
  printf("eflags: %d, ss: %d, cs: %d\n", regs->eflags, regs->ss, regs->cs);
  printf("rsp: %x, ip: %x\n", regs->rsp, regs->eip);

  if (regs->int_no == EXCEPTION_PAGE_FAULT) {
    page_fault_error_t* e = (page_fault_error_t*)&regs->err_code;
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
  idt_set_gate(3, (uint64_t)isr3, 0x08, 0x8E);
  idt_set_gate(4, (uint64_t)isr4, 0x08, 0x8E);
  idt_set_gate(5, 0, 0x08, 0x0E);
  idt_set_gate(6, 0, 0x08, 0x0E);
  idt_set_gate(7, 0, 0x08, 0x0E);
  idt_set_gate(8, (uint64_t)isr8, 0x08, 0x8E);
  idt_set_gate(9, 0, 0x08, 0x0E);
  idt_set_gate(10, 0, 0x08, 0x0E);
  idt_set_gate(11, 0, 0x08, 0x0E);
  idt_set_gate(12, 0, 0x08, 0x0E);
  idt_set_gate(13, (uint64_t)isr13, 0x08, 0x8E);
  idt_set_gate(14, (uint64_t)isr14, 0x08, 0x8E);
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
  idt_set_gate(32, (uint64_t)isr32, 0x08, 0x8E);
  idt_set_gate(0x31, (uint64_t)isr0x31, 0x08, 0x8E);  // keyboard
  idt_set_gate(0x32, (uint64_t)isr0x32, 0x08, 0x8E);  // mouse
  idt_set_gate(0x33, (uint64_t)isr0x33, 0x08, 0x8e);  // ethernet
  idt_set_gate(0x34, (uint64_t)isr0x34, 0x08, 0x8e);  // timer

  idt_update(&idt);
}

void idt_set_gate(uint32 interrupt,
		  uint64_t offset,
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

  ioapic_redirection_register_t r4 = {0};
  r4.upper_bits.destination = regs->local_apic_id >> 24;  // apic id
  r4.lower_bits.interrupt_vector = 0x34;
  ioapic_write_register(IOAPIC_IRQ4, &r4);
  printf("ioapic irq 4: %x\n", ioapic_read_register(IOAPIC_IRQ4));

  // Setup ps2 controller, mouse and keyboard.
  // Sometimes ps2_wait_data can hang until a key is pressed. Not sure why.
  // do we need to enable ps2 port 2? mouse
  // ps2 controller spec:
  // https://web.archive.org/web/20210417040153/http://www.diakom.ru/el/elfirms/datashts/Smsc/42w11.pdf
  // write command to
  // command port 0x64
  // and then read from
  // data port 0x60
  uint8_t status = inb(0x64);
  printf("configuring ps2: %x\n", status);

  /* ps2_wait_ready(); */
  /* outb(0x64, 0x20);  // 0x20 = read config byte */
  /* status = inb(0x64); */
  /* ps2_wait_data(); */
  uint8 config;  // = inb(0x60); */
  /* printf("config byte 0: %x\n", config); */
  /* ps2_wait_ready(); */

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
  //  printf("config byte 4: %x\n", config);

  ps2_wait_ready();
  outb(0x64, 0xA8);  // enable second port

  ps2_wait_ready();
  outb(0x64, 0x20);  // 0x20 = read config byte
  ps2_wait_data();
  config = inb(0x60);
  //  printf("config byte 5: %x\n", config);

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
  // ps2_wait_data();   // this hangs with qemu
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
  uint64_t* start = (uint64_t*)0xE0000;
  uint64_t* end = (uint64_t*)0xFFFFF;

  // "RSD PTR "
  for (; start < end; start += 2) {
    uint8_t* s = (uint8_t*)start;
    if (s[0] == 'R' && s[1] == 'S' && s[2] == 'D' && s[3] == ' ' &&
	s[4] == 'P' && s[5] == 'T' && s[6] == 'R' && s[7] == ' ') {
      return (acpi_rsdp_t*)start;
    }
  }
  return NULL;
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

struct acpi_generic_address_structure {
  uint8_t address_space_id;
  uint8_t register_bit_width;
  uint8_t register_bit_offset;
  uint8_t reserved;
  uint64_t address;
} __attribute__((packed));
typedef struct acpi_generic_address_structure acpi_generic_address_structure_t;

struct acpi_hpet_header {
  uint32_t event_timer_block_id;
  acpi_generic_address_structure_t base_address;
  uint8_t hpet_number;
  uint16_t main_counter_minimum_clock_tick;
  uint8_t page_attribution;
} __attribute__((packed));
typedef struct acpi_hpet_header acpi_hpet_header_t;

// note: take care when taking references of a pointer.
void list_tables(acpi_sdt_header_t* rsdt) {
  int count = (rsdt->length - sizeof(acpi_sdt_header_t) + sizeof(uint32_t)) /
	      sizeof(uint32_t);
  printf("rsdt: entry count: %d\n", count);
  for (int i = 0; i < count; i++) {
    // &entries to get the first entry.
    // +i uses size of type which is uint32_t.
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
      // note: without uint8_t case here we go too far.
      acpi_hpet_header_t* hpet =
	  (acpi_hpet_header_t*)((uint8_t*)h + sizeof(acpi_sdt_header2_t));
      printf("HPET: hpet number: %d\n", hpet->hpet_number);
      printf("HPET: address space: %d base: %x\n",
	     hpet->base_address.address_space_id, hpet->base_address.address);
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
	  outl(base + 0x30, (uint32_t)network_rx_buffer);

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
  uint8_t* start = (uint8_t*)0xE0000;
  uint8_t* end = (uint8_t*)0xFFFFF;

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
  uint32_t total_size;
  uint32_t reserved;
} __attribute__((packed));
typedef struct multiboot2_information multiboot2_information_t;

struct multiboot2_tag_header {
  uint32_t type;
  uint32_t size;
} __attribute__((packed));
typedef struct multiboot2_tag_header multiboot2_tag_header_t;

struct multiboot2_tag_memory_map_header {
  uint32_t type;
  uint32_t size;
  uint32_t entry_size;
  uint32_t entry_version;
} __attribute__((packed));
typedef struct multiboot2_tag_memory_map_header
    multiboot2_tag_memory_map_header_t;

struct multiboot2_tag_memory_map_entry {
  uint64_t base_addr;
  uint64_t length;
  uint32_t type;
  uint32_t reserved;
} __attribute__((packed));
typedef struct multiboot2_tag_memory_map_entry
    multiboot2_tag_memory_map_entry_t;

#define MULTIBOOT2_TAG_END 0
#define MULTIBOOT2_TAG_MEMORY_MAP 6

int kmain(multiboot2_information_t* mbd, uint32_t magic) {
  printf("kernel start=%x end=%x size=%x\n", &_kernel_start, &_kernel_end,
	 (uint64_t)&_kernel_end - (uint64_t)&_kernel_start);

  if (magic != 0x36d76289) {
    printf("multiboot error: %x\n", magic);
    asm volatile("hlt");
  }

  // set pit 0 to one shot mode
  // bit 4-5 = access mode
  // bit 2-3 = mode
  // ref: https://www.diamondsystems.com/files/binaries/har82c54.pdf
  outb(0x43, 0b110010);

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

  // timer is already running

  // How to get this running?
  // We need to pre-fill the stack or register with the argument like id.
  // We need to save all important registers on the kernel stack(?) and
  // restore them later. How do we give it virtual memory? seems with cr3
  // thing to give page table.

  // What to do next?
  // Remove is done in the trampoline or the trampoline just sets the task
  // state and we remove it when we iterate over it. The timer / scheduler
  // just goes through the linked list. If it's empty it goes to the idle
  // task.
  //
  // 1. iterate on the clean up in the trampoline?
  // 2. reschedule immediately after sleep?
  // 3. memory allocation?
  //
  // What's the bigger goal?
  // Network stack is extremely brittle and works with 1 packet.
  // Want to refactor it by passing packets through layers.
  // Also want to handle it outside of the interrupt handler.
  // This implies there is a task that waits for data.

  // I found the smallest increment I can make is by allocating the stack and
  // task in my task_new.

  // we need to store all available memory somewhere. the below gives usable
  // memory, we have to subtract the kernel size from it.
  // The kernel currently is identity mappes to 2mb pages.

  multiboot2_tag_header_t* h =
      (multiboot2_tag_header_t*)((uintptr_t)mbd +
				 sizeof(multiboot2_information_t));

  while (h->type != MULTIBOOT2_TAG_END) {
    //   printf("header type: %x size: %x\n", h->type, h->size);
    if (h->type == MULTIBOOT2_TAG_MEMORY_MAP) {
      multiboot2_tag_memory_map_header_t* mh =
	  (multiboot2_tag_memory_map_header_t*)h;
      uint32_t num_entries = mh->size / mh->entry_size;
      //   printf("memory map: entries = %d\n", num_entries);
      multiboot2_tag_memory_map_entry_t* e =
	  (multiboot2_tag_memory_map_entry_t*)((uintptr_t)h +
					       sizeof(
						   multiboot2_tag_memory_map_header_t));
      for (uint8_t i = 0; i < num_entries; i++) {
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

  //  task_new((uint64_t)kernel_task, kernel_task_stack, &kernel);
  //  task_new((uint64_t)task1, t1_stack, &t1);
  //  task_new((uint64_t)task2, t2_stack, &t2);

  task_current = task_new_malloc((uint64_t)kernel_task);
  task_new_malloc((uint64_t)task1);
  task_new_malloc((uint64_t)task2);

  // TODO: somehow replace the current kmain with the idle task.
  // task_current = &kernel;

  while (1) {
    // printf("kernel HLT: stack: %x eip: %x\n", kernel.rsp, kernel.eip);
    asm("HLT");
  }

  // send_dhcp_discover();
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
