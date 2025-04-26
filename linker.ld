VMA = 0xFFFFFFFF80000000;
LMA = 0x200000;

ENTRY(loader)
SECTIONS {
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
