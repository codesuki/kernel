bits 64

global main
main:
	mov rdi, 1
	syscall

	mov rdi, 2
	syscall
