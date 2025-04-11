VMA = 0xFFFFFFFF80000000;
LMA = 0x200000;

ENTRY(loader)
SECTIONS {
    /DISCARD/ : { *(.note.gnu.build-id) }
    . = 1M;

    .boot : {
	*(.multiboot)
    }

    .loader : {
       *loader.a(.*)
       *trampoline_64.a(.*)
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
