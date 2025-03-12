.model tiny
.code
org 100h

Start:
    mov bx, 1111h
    mov cx, 2022h
    mov dx, 3333h

    mov ah, 1h
    int 16h
    jnz Exit

    jmp Start

Exit:
    mov ah, 4ch
    xor al, al
    int 21h


END Start