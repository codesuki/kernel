VMA = 0xFFFFFFFF80000000;
LMA = 0x200000;

ENTRY(loader)
SECTIONS {
    . = 1M;

    _kernel_start = .;

    .boot : {
	*(.multiboot)
    }

    .loader : {
       loader.o(.*)
       trampoline_64.o(.*)
    }

    _loader_end = .;

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

    _kernel_end = .;
}
