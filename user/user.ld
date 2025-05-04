ENTRY(_main)
SECTIONS {
	 . = 0x600000;
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
}
