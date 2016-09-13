SOURCES=loader.o gdt.o idt.o interrupt_wrapper.o kernel.o

LD=x86_64-elf-ld

# clang --target=i686-pc-none-elf -march=i686 -o kernel.o -c kernel.c -Wall -Wextra -nostdlib -nostdinc -nostdinc++ -ffreestanding -fno-builtin

CFLAGS=--target=i686-pc-none-elf -Wall -Wextra -nostdlib -nostartfiles -nodefaultlibs -m32
LDFLAGS=-Tlinker.ld -melf_i386
ASMFLAGS=-felf

all: $(SOURCES) link
	cp kernel.bin bootdisk/

clean:
	rm *.o kernel.bin

link: loader.o kernel.o
	$(LD) $(LDFLAGS) -o kernel.bin $(SOURCES)

loader.o: loader.s
	nasm $(ASMFLAGS) -o loader.o loader.s

gdt.o: gdt.s
	nasm $(ASMFLAGS) -o gdt.o gdt.s

idt.o: idt.s
	nasm $(ASMFLAGS) -o idt.o idt.s

interrupt_wrapper.o: interrupt_wrapper.s
	nasm $(ASMFLAGS) -o interrupt_wrapper.o interrupt_wrapper.s

kernel.o: kernel.c
	gcc -o kernel.o -c kernel.c $(CFLAGS)
