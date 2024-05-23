section .text
bits 64

extern kmain

global asm_main
asm_main:
;; reset data segment registers. In long mode they are 0. If we don't reset them
;; here for example ss will be a weird value that will result in GP faults
;; during interrupt handling. This happened to me.
	mov ax, 0
	mov ss, ax
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax

	mov rax, 0x2f592f412f4b2f4f
	mov qword [0xb8000], rax

	call kmain                       ; call kernel proper

	; cli
hang:
	hlt                                ; halt machine should kernel return
	jmp hang
