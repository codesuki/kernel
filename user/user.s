bits 64

extern main
global _main
_main:
	; _main wraps the user main to clean up after it ends
	call main

	mov rdi, 1
	syscall
