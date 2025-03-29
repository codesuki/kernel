section .text
bits 64

;; We need to get to 64bit mode before we can jump to the higher half, because
;; the address is in 64bit range. asm_trampoline is linked with loader to the
;; lower half. So we jump twice, once to asm_trampoline to get into 64bit mode
;; and then to the kernel.

extern asm_main
global asm_trampoline
asm_trampoline:
;; ref:
	;; https://stackoverflow.com/questions/51832437/encoding-jmp-far-and-call-far-in-x86-64
	;; I had jmp [rax] here first, but what that means is jump to whatever
	;; is in memory at address rax which is asm_main, so I am reading the
	;; first few opcodes from asm_main.
	mov rax, asm_main
	jmp rax
