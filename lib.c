#include "lib.h"

#include "print.h"
#include "types.h"

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

// To be the same as strncmp it should stop after finding a 0 character.
bool strncmp(char* s1, char* s2, int n) {
  for (int i = 0; i < n; i++) {
    if (s1[i] != s2[i]) {
      return false;
    }
  }
  return true;
}

u32 strlen(u8* str) {
  u32 len = 0;
  while (*str++ != '\0') {
    len++;
  }
  return len;
}

bool memcmp(void* s1, void* s2, usize n) {
  for (usize i = 0; i < n; i++) {
    if (((u8*)s1)[i] != ((u8*)s2)[i]) {
      printf("i: %d s1: %d s2: %d\n", i, ((u8*)s1)[i], ((u8*)s2)[i]);
      return false;
    }
  }
  return true;
}

// TODO: optimize by copying bigger blocks at once.
void memcpy(void* src, void* dst, usize len) {
  for (int i = 0; i < len; i++) {
    ((u8*)dst)[i] = ((u8*)src)[i];
  }
}

void memset(void* dst, u8 byte, usize len) {
  for (usize i = 0; i < len; i++) {
    ((u8*)dst)[i] = byte;
  }
}

u8* reverse(u8* buffer, u32 length) {
  u8* start = buffer;
  u8* end = buffer + length - 1;
  while (start < end) {
    u8 tmp = *start;
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

u8* ltoa(u64 value, u8* buffer, u32 base) {
  u8* s = buffer;
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
    // printf("print: base %d, value %d\n", base, value);
    u64 remainder = value % base;
    value /= base;
    *s++ = digit(remainder);
  }
  *s++ = digit(value);
  if (base == 16) {
    *s++ = 'x';
    *s++ = '0';
  } else if (base == 2) {
    *s++ = 'b';
    *s++ = '0';
  }
  *s = 0;
  reverse(buffer, s - buffer);
  return buffer;
}

void panic(u8* msg) {
  printf("panic: %s\n", msg);
  __asm__("cli; hlt;");
}
