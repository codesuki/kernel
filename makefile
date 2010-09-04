SOURCES=loader.o kernel.o

CFLAGS=-Wall -Wextra -nostdlib -nostartfiles -nodefaultlibs
LDFLAGS=-Tlinker.ld
ASMFLAGS=-felf

all: $(SOURCES) link 
	cp kernel.bin bootdisk/

clean: 
	rm *.o kernel.bin

link: loader.o kernel.o
	i586-elf-ld $(LDFLAGS) -o kernel.bin $(SOURCES)

loader.o: loader.s
	nasm $(ASMFLAGS) -o loader.o loader.s

kernel.o: kernel.c
	i586-elf-gcc -o kernel.o -c kernel.c $(CFLAGS)