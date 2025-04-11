#include "print.h"

#include "io.h"
#include "lib.h"
#include "memory.h"
#include "types.h"

#define va_start(v, l) __builtin_va_start(v, l)
#define va_arg(v, l) __builtin_va_arg(v, l)
#define va_end(v) __builtin_va_end(v)
#define va_copy(d, s) __builtin_va_copy(d, s)
typedef __builtin_va_list va_list;

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

// We need to wrap around and look at the end of the ring buffer if we are on
// the first screen. This is the adjusted_cursor_index.
void display() {
  videoram = physical2virtual((void*)0xb8000);
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

// add serial io here
// ref:
// https://web.archive.org/web/20200224102106/http://minuszerodegrees.net/oa/OA%20-%20IBM%20PC%20AT%20Serial_Parallel%20Adapter.pdf
//
// address
//
// 0x3fb
// Line Control Register
// Bit 7 Divisor Latch Access Bit
//
// 0x3fd
// Line Status Register
// Bit 5 is 1 if the TX register is empty, i.e. we can send data.
//
// 0x3f8
// TX, RX buffer, depending on whether we write or read
// If the Divisor Latch Access Bit (bit 7 in LCR) in the Line Control Register
// is set it can access byte 1 of the Baud Rate Generator.
//
// 0x3f9
//
// Baud rate generator has to 8bit latches to configure a 16bit divisor.
// 0xf8 0xf9
// I think I have to divide the mhz by my desired baud rate to get the divisor.
// I found this to be 192.
// I wanted 9600 baud rate, but turns out I got it wrong. 192 would result in a
// baud rate of 600.
// ref:
// https://en.wikibooks.org/wiki/Serial_Programming/8250_UART_Programming#Divisor_Latch_Bytes
//
// I am probably wrong but I cannot see this just by looking at the spec sheet
//
// "Programmable Baud-Rate Generator: The controller has a programmable
// baud-rate generator that can divide the clock input (1.8432 MHz) by any
// divisor from 1 to 655,535 or 2^(16-1). The output frequency of the baud-rate
// generator is the baud rate multiplied by 16. Two 8-bit latches store the
// divisor in a 16-bit binary format. These divisor latches must be loaded
// during setup to ensure desired operation of the baud-rate generator. When
// either of the divisor latches is loaded, a 16-bit baud counter is immediately
// loaded. This prevents long counts on the first load."
//
// Plan
// Read LCR
// Set bit 7
// Maybe set bit 0,1 to 1 1 to enable 8 bit data (1 byte)
// Write LCR
// Disable bit 7 in LCR again
#define SERIAL_IO_1 0x3f8
#define SERIAL_IO_2 0x3f9
#define SERIAL_LINE_CONTROL_REGISTER 0x3fb
#define SERIAL_LINE_STATUS_REGISTER 0x3fd
void serial_init() {
  u8 data = inb(SERIAL_LINE_CONTROL_REGISTER);
  // Enable bit 0,1,7
  data = data | 0b10000011;
  outb(SERIAL_LINE_CONTROL_REGISTER, data);
  outb(SERIAL_IO_1, 192);
  outb(SERIAL_IO_2, 0);
  data = inb(SERIAL_LINE_CONTROL_REGISTER);
  data = data & ~0b10000000;
  outb(SERIAL_LINE_CONTROL_REGISTER, data);
}

void print_character(char c) {
  print_character_color(c, 0x07);
  // Wait for TX buffer to be empty
  while ((inb(SERIAL_LINE_STATUS_REGISTER) & 0b00100000) != 0b00100000) {
  }
  outb(0x3f8, c);
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

void print_hex(u64 d) {
  char number_buffer[19];
  ltoa(d, number_buffer, 16);
  // char number_buffer[11];
  // itoa(d, number_buffer, 16);
  print_string(number_buffer);
}

// memset with vectorized implementation can be faster
void cls() {
  videoram = physical2virtual((void*)0xb8000);
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
	  s32 d = va_arg(args, int);
	  print_integer(d);
	  break;
	case 'c':
	  // bug?
	  char c = va_arg(args, int);
	  print_character(c);
	  break;
	case 's':
	  char* s = va_arg(args, char*);
	  print_string_n(s, length);
	  break;
	case 'x':
	  u64 h = va_arg(args, u64);
	  print_hex(h);
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
