[BITS 16]
ORG     0
INT     0x18
TIMES   510-($-$$) DB 0
DW      0xAA55
