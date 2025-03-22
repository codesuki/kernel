#pragma once

#include "types.h"

int max(int x, int y);
int min(int x, int y);
bool strncmp(char* s1, char* s2, int n);
bool memcmp(void* s1, void* s2, usize n);
void memcpy(void* src, void* dst, usize len);
void memset(void* dst, u8 byte, usize len);
