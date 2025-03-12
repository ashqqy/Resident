.model tiny
.code
org 100h

Start:
    	mov bx, 1111h
    	mov cx, 2022h
    	mov dx, 3333h
    	mov di, 4444h
    	mov si, 5555h
    	push 6666h
	pop ds
	push 7777h
	pop ss
	push 8888h
	pop es
		

    	mov ah, 1h
    	int 16h
    	jnz Exit

    	jmp Start

Exit:
    	mov ah, 4ch
    	xor al, al
    	int 21h


END Start