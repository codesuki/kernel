bits 64

global enable_syscalls
enable_syscalls:
	; write to model specific register for sysret
	mov eax, 0
	mov edx, 16 << 16
	mov ecx, 0xc0000081	; IA32_STAR
	wrmsr

	mov eax, edi
	shr rdi, 32
	mov edx, edi
	mov ecx, 0xc0000082	; IA32_LSTAR
	wrmsr

	ret

extern syscall_handler
global syscall_wrapper
syscall_wrapper:
	; switch stack
	push rcx		; contains rip

	call syscall_handler

	pop rcx
	; switch stack
	o64 sysret

global switch_task
switch_task:
	; Thought on tasks having their own memory. Does this mean I need to
	; reserve memory and copy all the code there? This would explain why
	; threading works the way it does in Linux. It probably copies the whole
	; binary there again.
	; What I have seems more like go routines.

	; TODO There's a bug. When we fill registers from
	; maybe rdi it fills in weird data.

	; We leave caller saved scratch registers on the stack.
	; We return after the call to switch_task
	mov qword [rdi+8*3], rax
	mov qword [rdi+8*4], rbx
	mov qword [rdi+8*5], rcx
	mov qword [rdi+8*6], rdx
	mov qword [rdi+8*7], rsi
	mov qword [rdi+8*8], rdi
	mov qword [rdi+8*9], rbp
	mov qword [rdi+8*10], r8
	mov qword [rdi+8*11], r9
	mov qword [rdi+8*12], r10
	mov qword [rdi+8*13], r11
	mov qword [rdi+8*14], r12
	mov qword [rdi+8*15], r13
	mov qword [rdi+8*16], r14
	mov qword [rdi+8*17], r15

	; save the flags register by pushing it to the stack, copying and
	; popping it off.
	pushfq
	; rax is already saved above so we can use it.
	pop rax
	mov qword [rdi+8*18], rax

	; eip is on stack for return
	; What does it point to?
	pop rdx
	mov qword [rdi+8*2], rdx	; 2 eip
	; move stack pointer now that stack is 'empty'
	mov qword [rdi+8*1], rsp	; 1 stack ptr

	; push cr3

	; We get a pointer to a task_t. It has the id, stack pointer and the eip.
	mov qword rax, [rsi+8*3]
	mov qword rbx, [rsi+8*4]
	mov qword rcx, [rsi+8*5]
	mov qword rdx, [rsi+8*6]
	mov qword rdi, [rsi+8*8]
	mov qword rbp, [rsi+8*9]
	mov qword r8, [rsi+8*10]
	mov qword r9, [rsi+8*11]
	mov qword r10, [rsi+8*12]
	mov qword r11, [rsi+8*13]
	mov qword r12, [rsi+8*14]
	mov qword r13, [rsi+8*15]
	mov qword r14, [rsi+8*16]
	mov qword r15, [rsi+8*17]

	; save rax, get rflags, push to stack, pop to rflags, restore rax
	; could probably be better.
	push rax
	mov qword rax, [rsi+8*18]
	push rax
	popfq
	pop rax

	; get id
	; mov byte rcx, [rsi+0]

	; get stack_ptr
	mov qword rsp, [rsi+8*1]

	; get ip TODO we lost rax here
	mov qword rax, [rsi+8*2]
	mov rcx, rax

	mov rax, [rsi+8*19]
	; set highest 4 bits to 0 bit 60 and 63 must be 0 the others we don't
	; need, yet.
	and rax, 0x0000ffffffffffff
	mov cr3, rax

	; last we can overwrite rsi
	mov qword rsi, [rsi+8*7]

	; The code initializes a task with the interrupt flag IF=0 which
	; disables interrupts. For now we enable them here manually.
	; Should maybe initialize differently.
	; Also, the new schedule code disables interrupts and we want to enable
	; them again.
	;sti
	o64 sysret

;; https://stackoverflow.com/a/48597025
;; nasm will optimize mov rax, 1. One has to be specific on the size.
;; mov eax, 1
;; mov rax, strict dword 1
;; mov rax, strict qword 1
