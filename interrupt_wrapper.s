bits 64

extern interrupt_handler

%macro ISR_NOERRCODE 1
global isr%1
isr%1:
	cli
	push 0		; in case no error was pushed, but the C code expects a value.
	push %1
	jmp isr_wrapper
%endmacro

%macro ISR_ERRCODE 1
global isr%1
isr%1:
	cli
	push %1
	jmp isr_wrapper
%endmacro

ISR_NOERRCODE 0
ISR_NOERRCODE 1
ISR_NOERRCODE 2
ISR_NOERRCODE 3
ISR_NOERRCODE 4
ISR_NOERRCODE 5
ISR_NOERRCODE 6
ISR_NOERRCODE 7
ISR_ERRCODE 8
ISR_NOERRCODE 9
ISR_ERRCODE 10
ISR_ERRCODE 11
ISR_ERRCODE 12
ISR_ERRCODE 13
ISR_ERRCODE 14
ISR_NOERRCODE 15
ISR_NOERRCODE 16
ISR_NOERRCODE 17
ISR_NOERRCODE 18
ISR_NOERRCODE 19
ISR_NOERRCODE 20
ISR_NOERRCODE 21
ISR_NOERRCODE 22
ISR_NOERRCODE 23
ISR_NOERRCODE 24
ISR_NOERRCODE 25
ISR_NOERRCODE 26
ISR_NOERRCODE 27
ISR_NOERRCODE 28
ISR_NOERRCODE 29
ISR_NOERRCODE 30
ISR_NOERRCODE 31
ISR_NOERRCODE 32
ISR_NOERRCODE 0x31
ISR_NOERRCODE 0x32
ISR_NOERRCODE 0x33
ISR_NOERRCODE 0x34

global isr_wrapper
isr_wrapper:
	; TODO: probably more registers to save.

	;; Save caller saved scratch registers.
	push rax
	push rcx
	push rdx
	push rsi
	push rdi
	push r8
	push r9
	push r10
	push r11

	; We put a datastructure onto the stack and pass it to interrupt_handler.
	mov rdi, rsp
	add rdi, 9*8		; 9 scratch registers * 8 byte

	call interrupt_handler

	; Restore caller saved scratch registers.
	pop r11
	pop r10
	pop r9
	pop r8
	pop rdi
	pop rsi
	pop rdx
	pop rcx
	pop rax

	;; Remove error code and interrupt number from the stack.
	add rsp, 16

	;; Re-enable hardware interrupts that we disabled with cli.
	sti
	iretq
