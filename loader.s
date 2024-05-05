bits 32

extern kmain

section .text
        ; reserve 16k initial kernel stack space
        STACKSIZE equ 0x4000

global loader
loader:
        call check_multiboot

        ; set up the stack. Could use label after stack instead of addition
        mov esp, stack+STACKSIZE

        push eax                           ; pass Multiboot magic number
        push ebx                           ; pass Multiboot info structure

        call  kmain                       ; call kernel proper

        cli
hang:
        hlt                                ; halt machine should kernel return
        jmp   hang

handle_error:
        mov dword [0xb8000], 0x04410450 ; print PANIC in red letters 0x04 <- red, 0x41 <- A
        mov dword [0xb8004], 0x0449044E
        mov dword [0xb8008], 0x04200443
        mov byte [0xb800D], 0x04
        mov byte [0xb800C], al	; print error code
        jmp hang

check_multiboot:
        cmp eax, 0x36d76289
        jne .error
        ret
.error:
        mov al, 48		; 48 = ascii 0
        jmp handle_error

section .bss
stack:
        resb STACKSIZE                     ; reserve 16k stack on a doubleword boundary
