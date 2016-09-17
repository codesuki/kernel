bits 32

global idt_update
idt_update:
        mov eax, [esp+4]  ; Get the pointer to the IDT, passed as a parameter.
        lidt [eax]        ; Load the new IDT pointer
        ret
