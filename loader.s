global loader           ; making entry point visible to linker
extern kmain            ; kmain is defined elsewhere

section .text
        ; reserve 16k initial kernel stack space
        STACKSIZE equ 0x4000

loader:
        ; set up the stack
        mov esp, stack+STACKSIZE
        push eax                           ; pass Multiboot magic number
        push ebx                           ; pass Multiboot info structure

        call  kmain                       ; call kernel proper

        cli
hang:
        hlt                                ; halt machine should kernel return
        jmp   hang

section .bss
stack:
        resb STACKSIZE                     ; reserve 16k stack on a doubleword boundary
