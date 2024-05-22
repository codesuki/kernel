bits 64

global idt_update
idt_update:
;; x64 calling conventions pass the first pointer parameter in rdi. We can also
;; find this out by looking at the objdump output.
	lidt [rdi]        ; Load the new IDT pointer
	sti
	ret
