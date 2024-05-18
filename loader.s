bits 32

section .text
        ; reserve 16k initial kernel stack space
        STACKSIZE equ 0x4000

extern asm_main
global loader
loader:
        call check_multiboot

        ; set up the stack. Could use label after stack instead of addition
        mov esp, stack+STACKSIZE

        call setup_paging
        lgdt [gdt64.pointer]

        jmp gdt64.code:asm_main ; long jump to set cs register

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

setup_paging:
;; what do we have to do? set up premilimary page tables in 32 bit mode before
;; switching to long mode. We can use 3 layers with 2mb pages instead of
;; going all the way to 4 layers and 4kb pages.

;; pml4 needs to be setup like so
;; bit 0 = 1 present
;; bit 1 = 1 writable
;; bit 12 onward = address of pdpt

;; why does this work? because addresses of the tables are aligned on 4096 which
;; is 0b1000000000000 which means the first 12 bits are free to use the first
;; table might be 0b1000000000000 the second 0b2000000000000, etc.

        mov eax, pdpt           ; take the address of pdpt
        or eax, 0b11            ; flip the first two bytes as described above
        mov [pml4], eax         ; write this to pml4

        mov eax, pd
        or eax, 0b11
        mov [pdpt], eax

;; now we need to fill pd with 512 pages of 2mb
        mov ecx, 0
.loop:                          ; . means local label. nested under previous label.
        mov eax, 0x200000       ; 2mib
        mul ecx                 ; multiplies with eax
        or eax, 0b10000011      ; present = 1, rw = 1, 2mb page = 1
        mov [pd+ecx*8], eax     ; every entry is 8 byte = 1 uint on 64 bit arch.

        inc ecx
        cmp ecx, 512
        jne .loop

;; enable paging
;; ref 10.8.5
;; If CR4.PAE = 1, IA32_EFER.LME = 1, and CR4.LA57 = 0, 4-level paging1 is used.
;; Source: 4.1.1 325384-sdm-vol-3abcd.pdf


;; enable CR4.PAE = bit 5
        mov eax, cr4
        or eax, 1 << 5
;; disable CR4.LA57 = bit 12
        and eax, ~(1 << 12)     ; ~ is the ones complement
        mov cr4, eax

;; pml4 address needs to be written to CR3
        mov eax, pml4
        mov cr3, eax

;; IA32_EFER.LME = bit 8
;; Address found here: https://wiki.osdev.org/CPU_Registers_x86-64#IA32_EFER
;; https://wiki.osdev.org/MSR
;; https://en.wikipedia.org/wiki/Model-specific_register#Using_MSRs
;; https://www.felixcloutier.com/x86/rdmsr
;; https://www.felixcloutier.com/x86/wrmsr
        mov ecx, 0xC0000080
        rdmsr                   ; edx has higher 32 and eax lower 32 bits
        or eax, 1 << 8
        wrmsr




;;  CR0.PG = 1 = bit 31
        mov eax, cr0
        or eax, 1 << 31
;; needs enabled protection
;;  CR0.PE = 1 = bit 0
        or eax, 0b1            ; maybe this is already on?
        mov cr0, eax

        ret

section .bss
align 4096                      ; according to the intel docs page tables need to be aligned.
pml4:
        resb 4096
pdpt:
        resb 4096
pd:
        resb 4096
stack:
        resb STACKSIZE                     ; reserve 16k stack on a doubleword boundary

;; the gdt setup could probably also be moved to bss and configured in code.
section .rodata
gdt64:
        dq 0 ; zero entry
;; ; 43 = executable, 44 = code segment, 47 = present, 53 = 64bit
.code equ $ - gdt64             ; offset of the code segment from gdt64 as constant
        dq (1<<43) | (1<<44) | (1<<47) | (1<<53)
.pointer:
        dw $ - gdt64 - 1
        dq gdt64
