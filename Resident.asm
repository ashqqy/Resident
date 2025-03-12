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

N_PUSHED_REGS equ 9d

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
	mov si, bx			; ds:[si] = 0000h:[n_interrupt * 4]

	cli
	movsw 				; copy offset (es:[di+=2] = ds:[si+=2])
	movsw				; copy segment

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
		db 0eah			; far jump
Original_int_9:
		dw 0			; offset
		dw 0			; segment

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
	push ax bx cx dx di si ds ss es

	xor al, al
	mov	al, byte ptr cs:[Active]
	cmp al, 0						; if (Active == 0)
	je	Resident_int_8_end			;  return;

	mov si, sp
	add si, N_PUSHED_REGS * 2d - 1  ; 9 regs * 2 bytes - 1
	call CopyRegs

	push cs
	pop	ds							; ds = code segment
	mov bx, VIDEOSEG
	mov es, bx						; es = VIDEOSEG

	call DrawFrame
	call DrawRegs

Resident_int_8_end:	
	pop es ss ds si di dx cx bx ax
	db 0eah ; far jump
	Original_int_8 DD 0	; label (offset + segment)

Resident_int_8 endp

;---------------------------------
; Entry: si - stack pointer to first pushed reg
; Destr: di, ds, es, cx, si
;---------------------------------
CopyRegs proc
	mov di, offset REG_ARR

	push cs ss			; ds = stack segment
	pop ds es			; es = code segment

	mov cx, N_PUSHED_REGS
CopyRegsLoop:			;<-------
	add di, 4d			;		|
	std					;		|
	lodsb				;		|
	cld					;		|
	call GetAscii		;		|
	stosw				;		|
;						;		|
	std					;		|
	lodsb				;		|
	cld					;		|
	call GetAscii		;		|
	stosw				;		|
;						;		|
	loop CopyRegsLoop	;--------
	ret
CopyRegs endp

;---------------------------------
; Entry: es - VIDEOSEG,
;		 ds - code segment
; Destr: ax, cx, si, di
;---------------------------------
DrawRegs proc

	mov si, offset REG_ARR
	mov ah, COLOR

	mov cx, N_PUSHED_REGS
	mov di, SCREENLEN*(Y+1)+(X+1)*2
DrawRegsLoop:						;<-----
	push cx							;	  |
;									;	  |
	mov cx, 8h						;	  |
DrawRegLoop:						;<--  |
	lodsb							;  |  |
	stosw							;  |  |
	loop DrawRegLoop				;---  |
;									;	  |
	add di, SCREENLEN - 8h * 2h		;     |
	pop cx							;	  |
	loop DrawRegsLoop				;------

	ret
DrawRegs endp
;---------------------------------
; Input:  al - **1 byte** of register
; Output: ax - ascii symb of register byte
; Destr:  bl - temp buffer
;---------------------------------
GetAscii	proc
		
		mov bl, al 		; save byte
		and al, 0Fh		; change to ascii first hex digit (4 bits)
		call HexToAscii	
		mov ah, al		

		mov al, bl		; change to ascii second hex digit
		shr al, 4
		call HexToAscii

		ret
		endp	
;---------------------------------
; Input:  al - hex digit (4 bits)
; Output: al - ascii digit byte (8 bits)
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
;Entry:	ES - videoseg (0b800h)
;
;Destr: CX - counter,
;	AL - symbol,
;	AH - color,
;	DI - coords (screenlen * y + x * 2),
;	SI - framestyle array ptr
;---------------------------------
DrawFrame	proc

		mov ah, COLOR
		mov DI, SCREENLEN*Y+X*2
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
		stosw			; es:[di] = ax; di+=2

		mov CX, FRAMELEN - 2
		lodsb
		rep stosw
	
		lodsb
		stosw

		ret
		endp

;---------------------------------

Exit: mov ax, 3100h
	mov dx, offset EOP
	shr dx, 4 			; dx /= 16
	inc dx
	int 21h
;---------------------------------

Active db 0
STYLE	db 218, 196, 191, 179, , 179, 192, 196, 217 ; (┌, ─, ┐, │, , │, └, ─, ┘)

HexTable: db "0123456789abcdef"

REG_ARR	db "AX: "
dd 0
db "BX: "
dd 0
db "CX: " 
dd 0 
db "DX: " 
dd 0
db "DI: "
dd 0
db "SI: " 
dd 0
db "DS: " 
dd 0
db "SS: " 
dd 0
db "ES: " 
dd 0
db "CS: " 
dd 0
db "IP: " 
dd 0

EOP:
end 	Main
