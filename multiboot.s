bits 32
; reference:
; http://git.savannah.gnu.org/cgit/grub.git/tree/doc/multiboot.texi?h=multiboot2
align 4
section .multiboot
header_start:
        ; magic
        dd 0xE85250D6
        ; architecture
        ; 0 = 32bit protected mode i386
        ; 4 = 32bit mips
        dd 0
        ; header_length
        dd header_end - header_start
        ; checksum using two's complement
        dd 0x100000000 - (0xe85250d6 + 0 + (header_end - header_start))
        ; MULTIBOOT_TAG_TYPE_END = 0
        dw 0
        dw 0
        dd 8
header_end:
