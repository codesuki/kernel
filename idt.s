global idt_update    ; Allows the C code to call idt_update().

idt_update:
        mov eax, [esp+4]  ; Get the pointer to the IDT, passed as a parameter.
        lidt [eax]        ; Load the new IDT pointer
        ret