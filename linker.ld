ENTRY (loader)

SECTIONS{
    . = 0x00100000;

    .boot : {
        *(.multiboot)
    }

    .text : {
        *(.text)
    }

    .rodata : {
        *(.rodata)
    }

    .data ALIGN (0x1000) : {
        *(.data)
    }

    .bss : {
        sbss = .;
        *(COMMON)
        *(.bss)
        ebss = .;
    }
}
