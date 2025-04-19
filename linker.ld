VMA = 0xFFFFFFFF80000000;
LMA = 0x200000;

INPUT(ping.o)

ENTRY(loader)
SECTIONS {
    .binary 0x0 : AT(LMA + 2M) {
	   KEEP(ping.o(.text))
    }

    . = 1M;

    .boot : AT(0x100000) {
	*(.multiboot)
    }

    .loader : {
       loader.o(.*)
       trampoline_64.o(.*)
    }

    .text VMA : AT(LMA) {
	*(.text)
    }

    .rodata : {
	*(.rodata)
    }

    .data : {
	*(.data)
    }

    .bss : {
	*(.bss)
    }
}

_kernel_start = LOADADDR(.boot);
_kernel_end = LOADADDR(.bss) + SIZEOF(.bss);

_embed_start = LOADADDR(.binary);
_embed_end = LOADADDR(.binary) + SIZEOF(.binary);
EXTERN(main)
