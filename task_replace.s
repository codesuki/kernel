	bits 64

	global task_replace
task_replace:
	; We get a pointer to a task_t. It has the id, stack pointer and the eip.
	; get stack_ptr
	mov qword rsp, [rdi+8*1]

	; get ip
	mov qword rax, [rdi+8*2]
	push rax

	mov qword rax, [rdi+8*3]
	mov qword rcx, [rdi+8*4]
	mov qword rdx, [rdi+8*5]
	mov qword rsi, [rdi+8*6]
	mov qword rdi, [rdi+8*7]
	mov qword r8, [rdi+8*8]
	mov qword r9, [rdi+8*9]
	mov qword r10, [rdi+8*10]
	mov qword r11, [rdi+8*11]
	mov qword r12, [rdi+8*12]
	mov qword r13, [rdi+8*13]
	mov qword r14, [rdi+8*14]
	mov qword r15, [rdi+8*15]

	; get id
	; mov byte rcx, [rsi+0]

	ret
