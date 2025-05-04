bits 64

global main
main:
	mov rdi, 2
	syscall

	mov rdi, 3
	syscall

	ret
