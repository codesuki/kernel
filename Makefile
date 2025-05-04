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
LDFLAGS = -T linker.ld -nostdlib -z max-page-size=0x1000 -Map kernel.map

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
	@rm -f $(OBJECTS) $(KERNEL) $(ISO) hdd.img
	@rm -rf dist/

#-netdev vmnet-shared,id=vmnet -device rtl8139,netdev=vmnet
#-netdev vmnet-bridged,id=vmnet,ifname=en0 -device rtl8139,netdev=vmnet
#-d int is
# show interrupts/exceptions in short format
# -s is
# shorthand for -gdb tcp::1234
# -S is
# freeze CPU at startup (use 'c' to start execution)
# -monitor stdio
# -serial stdio

run: $(ISO)
	sudo qemu-system-x86_64 -d int -s \
	-cdrom $(ISO) \
	-boot d \
	-no-reboot -no-shutdown \
	-netdev vmnet-bridged,id=vmnet,ifname=en0 -device rtl8139,netdev=vmnet \
	-object filter-dump,id=f1,netdev=vmnet,file=dump.dat \
	-hda hdd.img \
	-serial stdio
#	-monitor stdio
	#-drive id=disk,file=hdd.img,if=none -device ahci,id=ahci -device ide-hd,drive=disk,bus=ahci.0 \
# -machine q35 # for PCIe


debug: $(ISO)
	sudo qemu-system-x86_64 -d int -s -S -no-reboot \
	-cdrom $(ISO) \
	-boot d \
	-netdev vmnet-bridged,id=vmnet,ifname=en0 -device rtl8139,netdev=vmnet \
	-object filter-dump,id=f1,netdev=vmnet,file=dump.dat \
	-serial stdio \
	-hda hdd.img

run-bochs: $(ISO)
	bochs -f bochs.rc

lldb:
	lldb -o "gdb-remote 1234" --file kernel.bin

$(ISO): $(KERNEL) hdd.img
	mkdir -p dist/boot/grub
	cp grub.cfg dist/boot/grub
	cp $(KERNEL) dist/boot/
	grub-mkrescue -o $(ISO) dist/

$(KERNEL): $(OBJECTS) linker.ld
	$(LD) $(LDFLAGS) -o $@ $(OBJECTS)

$(KERNEL): linker.ld

%.o: %.s
	$(AS) $(ASFLAGS) $< -o $@

%.o: %.c
%.o: %.c $(DEPDIR)/%.d | $(DEPDIR)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(DEPFLAGS) $< -o $@

# use mtools to build a fat32 disk image
hdd.img: README.md
	dd if=/dev/zero of=hdd.img bs=1M count=20
	mformat -F -i hdd.img ::
	mcopy -i hdd.img README.md ::
	mcopy -i hdd.img test_app ::

$(DEPDIR): ; @mkdir -p $@

DEPFILES := $(C_SOURCES:%.c=$(DEPDIR)/%.d)
$(DEPFILES):
include $(wildcard $(DEPFILES))
