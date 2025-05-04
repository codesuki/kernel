bits 64

global enable_syscalls
enable_syscalls:
	; write to model specific register for sysret
	mov eax, 0
	mov edx, 16 << 16
	mov ecx, 0xc0000081	; IA32_STAR
	wrmsr

	; TODO: just put syscall_wrapper here instaed of edi
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
	mov qword [rdi+8*2], rax
	mov qword [rdi+8*3], rbx
	mov qword [rdi+8*4], rcx
	mov qword [rdi+8*5], rdx
	mov qword [rdi+8*6], rsi
	mov qword [rdi+8*7], rdi
	mov qword [rdi+8*8], rbp
	mov qword [rdi+8*9], r8
	mov qword [rdi+8*10], r9
	mov qword [rdi+8*11], r10
	mov qword [rdi+8*12], r11
	mov qword [rdi+8*13], r12
	mov qword [rdi+8*14], r13
	mov qword [rdi+8*15], r14
	mov qword [rdi+8*16], r15

	; rax is already saved above so we can use it.
	mov qword rax, cr3
	mov qword [rdi+8*18], rax
	mov qword [rdi+8*19], cs
	mov qword [rdi+8*20], ss

	; save the flags register by pushing it to the stack, copying and
	; popping it off.
	pushfq
	; rax is already saved above so we can use it.
	pop rax
	mov qword [rdi+8*17], rax

	; rip is on stack for return
	; What does it point to?
	; The instruction after switch_task
	pop rdx
	mov qword [rdi+8*1], rdx	; 2 rip
	; move stack pointer now that stack is 'empty'
	; TODO: I think this can be moved up
	mov qword [rdi+8*0], rsp	; 1 stack ptr

	; push cr3

	; We get a pointer to a task_t. It has the saved registers.
	; Skip rax because we use it below. Restore last.
	; mov qword rax, [rsi+8*2]
	mov qword rbx, [rsi+8*3]
	mov qword rcx, [rsi+8*4]
	mov qword rdx, [rsi+8*5]
	; Skip rsi because we are using it
	; mov qword rax, [rsi+8*6]
	mov qword rdi, [rsi+8*7]
	mov qword rbp, [rsi+8*8]
	mov qword r8, [rsi+8*9]
	mov qword r9, [rsi+8*10]
	mov qword r10, [rsi+8*11]
	mov qword r11, [rsi+8*12]
	mov qword r12, [rsi+8*13]
	mov qword r13, [rsi+8*14]
	mov qword r14, [rsi+8*15]
	mov qword r15, [rsi+8*16]

	; Order of pushes for retq
	; ss
	; rsp
	; rflags
	; cs
	; rip

	; ss
	mov qword rax, [rsi+8*20]
	push rax

	; rsp
	mov qword rax, [rsi+8*0]
	push rax

	; rflags
	mov qword rax, [rsi+8*17]
	push rax

	; cs
	; We have to set RPL = 3
	; ref: Vol 3 3.4.2 Segment Selectors
	; push 0x20 | 3
	mov qword rax, [rsi+8*19]
	push rax

	; rip
	mov qword rax, [rsi+8*1]
	push rax

	; Save rax to stack so we can access it after changing the page table
	mov qword rax, [rsi+8*2]
	push rax

	; cr3
	mov rax, [rsi+8*18]

	; last we can overwrite rsi
	; I do this here because once cr3 is changed we may not be able to
	; access this.
	mov qword rsi, [rsi+8*6]

	; set highest 4 bits to 0 bit 60 and 63 must be 0 the others we don't
	; need, yet.
	and rax, 0x0000ffffffffffff
	mov cr3, rax

	; restore rax
	pop rax

	; The code initializes a task with the interrupt flag IF=0 which
	; disables interrupts. For now we enable them here manually.
	; Should maybe initialize differently.
	; Also, the new schedule code disables interrupts and we want to enable
	; them again.
	;sti
	iretq

;; https://stackoverflow.com/a/48597025
;; nasm will optimize mov rax, 1. One has to be specific on the size.
;; mov eax, 1
;; mov rax, strict dword 1
;; mov rax, strict qword 1
