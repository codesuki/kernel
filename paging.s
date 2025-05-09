bits 64

global switch_cr3
switch_cr3:
	mov rax, cr3
	and rax, 0b000000000000 ; set the first 12 bits to 0
	or rax, rdi		; set address of new pml4
	mov cr3, rax

global switch_gdt
switch_gdt:
	lgdt [rsi]

	mov rax, 0xffff800000000000
	add rsp, rax
	add rbp, rax
	pop rax
	push 0x8
	push rax

	; mov ax, 0
	; mov ss, ax
	; mov ds, ax
	; mov es, ax
	; mov fs, ax
	; mov gs, ax

	retfq

global switch_tss
switch_tss:
	ltr di
	ret

extern pages_init
global pages_init_wrapper
pages_init_wrapper:
	call pages_init
	mov rax, 0xffff800000000000
	add rbp, rax
	ret
