KERNEL = kernel.bin
ISO = kernel.iso

ASM_SOURCES = $(wildcard *.s)
OBJECTS = $(ASM_SOURCES:.s=.o)

C_SOURCES = $(wildcard *.c)
OBJECTS += $(C_SOURCES:.c=.o)

LD = x86_64-elf-ld
LDFLAGS = -T linker.ld -melf_i386

CC = gcc

COMPILER_NAME = $(shell gcc --version 2>&1 | grep -E -o '(clang|gcc)')
ifeq ($(COMPILER_NAME), clang)
	CFLAGS = --target=i686-pc-none-elf -march=i686 -Wall -Wextra -nostdlib -nostdinc -ffreestanding -fno-builtin
else
	CFLAGS = --target=i686-pc-none-elf -Wall -Wextra -nostdlib -nostartfiles -nodefaultlibs -m32
endif

AS = nasm
ASFLAGS = -f elf32

.PHONY: all clean run debug

all: $(ISO)

clean:
	@rm $(OBJECTS) $(KERNEL) $(ISO)
	@rm -r dist/

run:
	qemu-system-x86_64 -cdrom $(ISO) -s

debug:
	qemu-system-x86_64 -cdrom $(ISO) -s -S

$(ISO): $(KERNEL)
	mkdir -p dist/boot/grub
	cp grub.cfg dist/boot/grub
	cp $(KERNEL) dist/boot/
	grub-mkrescue -o $(ISO) dist/

$(KERNEL): $(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^

%.o: %.s
	$(AS) $(ASFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@
