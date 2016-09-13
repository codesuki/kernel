TARGET = kernel.bin

ASM_SOURCES = $(wildcard *.s)
ASM_OBJECTS = $(ASM_SOURCES:.s=.o)

C_SOURCES = $(wildcard *.c)
C_OBJECTS = $(C_SOURCES:.c=.o)

LD = x86_64-elf-ld
LDFLAGS = -T linker.ld -melf_i386

CC = gcc

COMPILER_NAME = $(shell gcc --version 2>1 | grep -E -o '(clang|gcc)')
ifeq ($(COMPILER_NAME), clang)
	CFLAGS = --target=i686-pc-none-elf -march=i686 -Wall -Wextra -nostdlib -nostdinc -ffreestanding -fno-builtin
else
	CFLAGS = --target=i686-pc-none-elf -Wall -Wextra -nostdlib -nostartfiles -nodefaultlibs -m32
endif

AS = nasm
ASFLAGS = -f elf32

.PHONY: all clean run debug

all: $(TARGET)
	cp kernel.bin bootdisk/

clean:
	@rm *.o $(TARGET)

run:
	qemu-system-x86_64 -kernel kernel.bin -s

debug:
	qemu-system-x86_64 -kernel kernel.bin -s -S

kernel.bin: $(ASM_OBJECTS) $(C_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^

%.o: %.s
	$(AS) $(ASFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@

link: loader.o kernel.o
	$(LD) $(LDFLAGS) -o kernel.bin $(SOURCES)
