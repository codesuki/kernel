bits 32
;; ref:
;; https://www.gnu.org/software/grub/manual/multiboot2/multiboot.html#Header-layout
;; tag format:
;; u16 type
;; u16 flags
;; u32 size
magic_number: equ 0xE85250D6
align 8
section .multiboot
header_start:
	; magic
	dd magic_number
	; architecture
	; 0 = 32bit protected mode i386
	; 4 = 32bit mips
	dd 0
	; header_length
	dd header_end - header_start
	; checksum using two's complement
	dd 0x100000000 - (magic_number + 0 + (header_end - header_start))
;; uncomment this to get a framebuffer
;; need to read framebuffer address from structure in main
	; MULTIBOOT_TAG_TYPE_FRAMEBUFFER = 5
	; ref: https://www.gnu.org/software/grub/manual/multiboot2/multiboot.html#Console-header-tags
	; align 8
	; dw 5
	; dw 0
	; dd 20
	; dd 0			; width = 0 means no preference
	; dd 0			; height
	; dd 0			; depth
	; MULTIBOOT_TAG_TYPE_END = 0
	align 8
	dw 0
	dw 0
	dd 8
header_end:
