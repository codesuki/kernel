#pragma once

#include "types.h"

int max(int x, int y);
int min(int x, int y);
bool strncmp(char* s1, char* s2, int n);
u32 strlen(u8* str);
bool memcmp(void* s1, void* s2, usize n);
void memcpy(void* src, void* dst, usize len);
void memset(void* dst, u8 byte, usize len);
char* itoa(int value, char* buffer, unsigned int base);
u8* ltoa(u64 value, u8* buffer, u32 base);
void panic(u8* msg);
