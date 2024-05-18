KERNEL = kernel.bin
ISO = kernel.iso

ASM_SOURCES = $(wildcard *.s)
OBJECTS = $(ASM_SOURCES:.s=.o)

C_SOURCES = $(wildcard *.c)
OBJECTS += $(C_SOURCES:.c=.o)

LD = x86_64-elf-ld
LDFLAGS = -T linker.ld -nostdlib -z max-page-size=0x1000

CC = x86_64-elf-gcc
CFLAGS = -g -Wall -Wextra -ffreestanding -mno-red-zone -mgeneral-regs-only

AS = nasm
ASFLAGS = -f elf64 -g -F dwarf

.PHONY: all clean run debug bochs

all: $(ISO)

clean:
	@rm $(OBJECTS) $(KERNEL) $(ISO)
	@rm -r dist/

run:
#-d int is
# show interrupts/exceptions in short format
# -s is
# shorthand for -gdb tcp::1234
# -S is
# freeze CPU at startup (use 'c' to start execution)
	qemu-system-x86_64 -d int -no-reboot -cdrom $(ISO) -s -no-shutdown -monitor stdio

debug:
	qemu-system-x86_64 -d int -no-reboot -cdrom $(ISO) -s -S

run-bochs:
	bochs -f bochs.rc

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
