KERNEL = kernel.bin
ISO = kernel.iso

ASM_SOURCES = $(wildcard *.s)
OBJECTS = $(ASM_SOURCES:.s=.o)

C_SOURCES = $(wildcard *.c)
OBJECTS += $(C_SOURCES:.c=.o)

# ref: https://make.mad-scientist.net/papers/advanced-auto-dependency-generation/
DEPDIR := .deps
DEPFLAGS = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.d

LD = x86_64-elf-ld
LDFLAGS = -T linker.ld -nostdlib -z max-page-size=0x1000

CC = x86_64-elf-gcc
# -mgeneral-regs-only disables usage of sse registers, etc.
# We would have to restore them on every context switch. For now I delay this.
# ref: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=70738
CFLAGS = -g -Wall -Wextra -ffreestanding -mno-red-zone -std=c23 -mgeneral-regs-only -m64 -mcmodel=kernel

AS = nasm
ASFLAGS = -f elf64 -g -F dwarf

.PHONY: all clean run debug bochs lldb

all: $(ISO)

clean:
	@rm -f $(OBJECTS) $(KERNEL) $(ISO)
	@rm -rf dist/

run: $(ISO)
#	-netdev vmnet-bridged,id=vmnet,ifname=en0 -device rtl8139,netdev=vmnet
#-d int is
# show interrupts/exceptions in short format
# -s is
# shorthand for -gdb tcp::1234
# -S is
# freeze CPU at startup (use 'c' to start execution)
# -monitor stdio \
# -serial stdio

	sudo qemu-system-x86_64 -d int \
	-no-reboot -cdrom $(ISO) -s -no-shutdown -serial stdio \
	-netdev vmnet-bridged,id=vmnet,ifname=en0 -device rtl8139,netdev=vmnet -object filter-dump,id=f1,netdev=vmnet,file=dump.dat
	#-netdev vmnet-shared,id=vmnet -device rtl8139,netdev=vmnet

debug: $(ISO)
	# -monitor stdio
	sudo qemu-system-x86_64 -d int -no-reboot -cdrom $(ISO) -s -serial stdio -S \
	-netdev vmnet-bridged,id=vmnet,ifname=en0 -device rtl8139,netdev=vmnet -object filter-dump,id=f1,netdev=vmnet,file=dump.dat

run-bochs: $(ISO)
	bochs -f bochs.rc

lldb:
	lldb -o "gdb-remote 1234" --file kernel.bin

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
%.o: %.c $(DEPDIR)/%.d | $(DEPDIR)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(DEPFLAGS) $< -o $@

$(DEPDIR): ; @mkdir -p $@

DEPFILES := $(C_SOURCES:%.c=$(DEPDIR)/%.d)
$(DEPFILES):
include $(wildcard $(DEPFILES))
