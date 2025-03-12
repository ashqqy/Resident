.model tiny
.code
org 100h

VIDEOSEG equ 0b800h
COLOR 	 equ 00011011b

SCREENLEN    equ 80*2
SCREENHEIGHT equ 25
FRAMELEN     equ 12
FRAMEHIGH    equ 11
X	equ 67
Y	equ 1

ON_KEY	equ	57h ; f11
OFF_KEY	equ	58h	; F12

Main:
	mov ax, offset Resident_int_9
	mov bx, 09h	 ; Interrupt num
	mov di, offset Original_int_9
	call Hook_interrupt

	mov ax, offset Resident_int_8
	mov bx, 08h		; Interrupt num
	mov di, offset Original_int_8
	call Hook_interrupt

	jmp Exit

;---------------------------------
; Input: AX - new interrupt offset
;		 BX - interrupt number to hook
;		 DI - ptr to save sys interrupt (double word)
; Destr: BX, CX, DI, SI
;---------------------------------
Hook_interrupt	proc
	push ds

	xor cx, cx
	mov ds, cx
	shl bx, 2
	mov si, bx	; ds:[si] = 0000h:[n_interrupt * 4]

	cli
	movsw 		; copy offset (es:[di+=2] = ds:[si+=2])
	movsw		; copy segment

	mov ds:[bx], ax		; replace sys_int to new_int
	mov ds:[bx + 2], cs
	sti

	pop ds
	ret
Hook_interrupt endp
;---------------------------------

Resident_int_9	proc
		push	ax es

		in	al, 60h		; get current scan-code
		cmp	al, ON_KEY
		je	Turn_on
		cmp	al, OFF_KEY
		je	Turn_off

		pop	es ax
		db 0eah		; far jump
Original_int_9:
		dw 0		; offset
		dw 0		; segment

Turn_on:
		mov	al, 1
		mov	byte ptr cs:[Active], al
		jmp	Int_end
Turn_off:
		xor	al, al
		mov	byte ptr cs:[Active], al
		jmp	Int_end

Int_end:
		in	al, 61h
		mov	ah, al		; get keyboard control line 
		or	al, 80h		; enable keyboard
		out	61h, al		; send
		mov	al, ah
		out	61h, al		; send back original control port val

		mov	al, 20h
		out	20h, al		; end-of-interrupt
		pop	es ax
		iret
	
Resident_int_9 endp
;---------------------------------

Resident_int_8 proc
	push dx cx bx ax di si bp sp ds ss es

	mov byte ptr cs:[AX_REG + 4], ah 
	mov byte ptr cs:[AX_REG + 6], al 
	mov byte ptr cs:[BX_REG + 4], bh
	mov byte ptr cs:[BX_REG + 6], bl
	mov byte ptr cs:[CX_REG + 4], ch
	mov byte ptr cs:[CX_REG + 6], cl
	mov byte ptr cs:[DX_REG + 4], dh
	mov byte ptr cs:[DX_REG + 6], dl 

	xor al, al
	mov	al, byte ptr cs:[Active]
	cmp al, 0
	je	Resident_int_8_end		; if (Active == 0) return
	
	mov cx, 4h
	mov si, offset AX_REG
ChangeToAscii:
	add si, 4
	mov byte ptr bl, cs:[si]
	call GetAscii
	mov word ptr cs:[si], ax
	add si, 2
	mov byte ptr bl, cs:[si]
	call GetAscii
	mov word ptr cs:[si], ax
	add si, 2
	loop ChangeToAscii

	push cs
	pop	ds						; ds = cs

	call	DrawFrame
	call 	DrawRegs

Resident_int_8_end:	
	pop es ss ds sp bp si di ax bx cx dx
	db 0eah ; far jump
	Original_int_8 DD 0	; label (offset + segment)

Resident_int_8 endp

;---------------------------------
;Entry: AH - color,
;	DI - coords,
;	ES - videoseg (0b800h),
;	SI - framestyle array ptr,
; 
;Destr: AL - symbol, 
;	CX - counter & line lenght
;---------------------------------
DrawLine 	proc

		lodsb			; al = ds:[si++]
		stosw			; es:[di] = ax, di+=2

		mov CX, FRAMELEN - 2
		lodsb
		rep stosw
	
		lodsb
		stosw

		ret
		endp

;---------------------------------
;Entry:	None
;
;Destr: CX - counter,
;	AL - symbol,
;	AH - color,
;	DI - coords (screenlen * y + x * 2),
;	ES - videoseg (0b800h),
;	SI - framestyle array ptr
;---------------------------------
DrawFrame	proc

		mov ah, COLOR
		mov DI, SCREENLEN*Y+X*2
		mov bx, VIDEOSEG
		mov es, bx
		mov si, offset STYLE
		push cs
		pop ds
	
		call DrawLine
		sub DI, FRAMELEN * 2
		add DI, SCREENLEN
	
		mov CX, FRAMEHIGH - 2

StartFrameBody:
		cmp CX, 0	
		je EndFrameBody
		
		push CX
		call DrawLine
		pop CX
		dec CX

		sub SI, 3

		sub DI, FRAMELEN * 2
		add DI, SCREENLEN

		jmp StartFrameBody

EndFrameBody:	
		add SI, 3
		call DrawLine

		ret
		endp
;---------------------------------

CopyRegs proc

	mov	si, sp
	add	si, 8d * 2d	+ 1

	mov di, offset AX_REG

	push cs cs
	pop ds es

	mov cx, 4d
CopyRegsLoop:
	add di, 4d
	mov bl, ds:[si]
	call GetAscii
	stosw
	add si, 1d
	mov bl, ds:[si]
	call GetAscii
	stosw
	add si, 1d

	loop CopyRegsLoop

	ret
CopyRegs endp

;---------------------------------

DrawRegs proc

	push cs
	pop ds
	mov si, offset AX_REG

	mov ah, COLOR

	mov bx, VIDEOSEG
	mov es, bx

	mov cx, 8h
	mov di, SCREENLEN*(Y+1)+(X+1)*2
DRAW_AX:
	lodsb
	stosw
	loop DRAW_AX

	mov cx, 8h
	mov di, SCREENLEN*(Y+2)+(X+1)*2
DRAW_BX:
	lodsb
	stosw
	loop DRAW_BX

	mov cx, 8h
	mov di, SCREENLEN*(Y+3)+(X+1)*2
DRAW_CX:
	lodsb
	stosw
	loop DRAW_CX

	mov cx, 8h
	mov di, SCREENLEN*(Y+4)+(X+1)*2
DRAW_DX:
	lodsb
	stosw
	loop DRAW_DX

	ret
DrawRegs endp
;---------------------------------
;Input: bl - 1 byte of register
GetAscii	proc
		
		mov al, bl
		and al, 0Fh
		call HexToAscii
		mov ah, al

		mov al, bl
		shr al, 4
		call HexToAscii

		ret
		endp	
;---------------------------------

HexToAscii	proc
		
		cmp al, 9

		jle IfDigitBody
		jg  IfLetterBody

IfDigitBody: 	add al, '0'
		ret

IfLetterBody:   add al, 'a' - 10
		ret
		
		endp
;---------------------------------

Exit: mov ax, 3100h
	mov dx, offset EOP
	shr dx, 4 		; dx /= 16
	inc dx
	int 21h

Active db 0
STYLE	db 218, 196, 191, 179, , 179, 192, 196, 217 ; (Ú, Ä, ¿, ³, , ³, À, Ä, Ù)
AX_REG	db "AX: " 
dd 0
BX_REG	db "BX: "
dd 0
CX_REG	db "CX: " 
dd 0 
DX_REG	db "DX: " 
dd 0

HexTable: db "0123456789abcdef"

EOP:
end 	Main
