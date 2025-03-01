ENTRY(loader)
SECTIONS {
    . = 1M;

    _kernel_start = .;

    .boot : {
	*(.multiboot)
    }

    .text : {
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
