#pragma once

void display();
int printf(const char* format, ...);
void print_string(char* s);
void cls();

// TODO move to different file
char* itoa(int value, char* buffer, unsigned int base);
