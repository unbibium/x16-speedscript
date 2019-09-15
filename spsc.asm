; SpeedScript 3.1
; copied by hand out of the pdf at archive.org
;
	processor 6502

	org $0801

	BYTE $0B,$08,$0A,$00,158 
	TEXT "2061" 
	BYTE 0,0,0 

	; some macros
	mac COPY16
	lda {1}
	sta {2}
	lda {1}+1
	sta {2}+1
	endm

	mac COPY163
	lda {1}
	sta {2}
	sta {3}
	lda {1}+1
	sta {2}+1
	sta {3}+1
	endm

	mac add16
	clc
	lda {1}
	adc {2}
	sta {1}
	lda {1}+1
	adc {2}+1
	sta {1}+1
	endm

	MAC DO_PRMSG
	LDA #<{1}
	LDY #>{1}
	JSR PRMSG
	ENDM

	MAC top_prmsg
	jsr topclr
	do_prmsg {1}
	ENDM

	mac COPY16X
	ldx {1}
	stx {2}
	ldx {1}+1
	stx {2}+1
	endm

	mac DISPLAY_NUMBER
	ldy #55
	sta map
	jsr #BDCD ;BASIC number display
	ldy #54
	sty map
	endm


;Named constants not used in source.
;May be useful for future ports.
SCRMEM equ $0400
COLMEM equ $D800
COLUMNS equ 40
ROWS    equ 25
SPACE   equ 32

;Locations used by high-speed memory 
;move routines: 

FROML equ $26 
FROMH equ $27 
DESTL equ $9E 
DESTH equ $9F 
LLEN = $B4 
HLEN = $B5 

;CURR: Position of cursor within text 
;memory. SCR: used by the REFRESH 
;routine. 

CURR = $39 
SCR = $C3 

;TEX: An alternate location used in tan- 
;dem with CURR. COLR is used by RE- 
;FRESH. TEMP is used throughout as a 
;reusable scratchpad pointer. INDIR is 
;also a reusable indirect pointer. 
;UNDERCURS stores the value of the 
;character highlighted by the cursor. 

TEX = $FB 
COLR = $14 
TEMP = $3B 
INDIR = $FD 
UNDERCURS = $02 

;WINDCOLR: Color of command line 
;window supported by REFRESH. MAP 
;is the 6510's built-in I/O port, used for 
;mapping in and out ROMs from the 
;address space. RETCHAR is the screen- 
;code value of the return mark (a left- 
;pointing arrow). 

WINDCOLR = $0C 
MAP equ $01 
RETCHAR equ 31 

;Kernal Routines 
;(refer to the Commodore 64 Programmer's Reference ;Guide): 

CHROUT = $FFD2 
STOP = $FFE1 
SETLFS = $FFBA 
SETNAM = $FFBD 
CLALL = $FFE7 
OPEN = $FFCO 
CHRIN = $FFCF 
CHKIN = $FFC6 
CHKOUT = $FFC9 
GETIN - $FFE4 
CLRCHN = $FFCC 
CLOSE - $FFC3 
LOAD = $FFD5 
SAVE = $FFD8 
lOINTF = $FF84 

Called only when run from BASIC. It is 
;assumed that the author's initials (that 
;conveniently work out in hex) are not 
;normally present in memory. If they 
;are, we know that SpeedScript has been 
;run before, so we avoid the ERASE 
;routine to preserve the text in memory. 

BEGIN JSR INIT 
	LDA #$CB 
	CMP FIRSTRUN 
	STA FIRSTRUN 
	BEQ SKIPERAS 
	JSR ERASE 
SKIPERAS JSR INIT2 
	JMP MAIN 

;UMOVE is a high-speed memory move 
;routine. It gets its speed from self- 
;modifying code (the $0000's at 
;MOVLOOP are replaced by actual ad- 
;dresses when UMOVE is called). Some 
;assemblers may assemble this as a 
;zero-page mode, so you may want to 
;change the $0000's to $FFFF's. UMOVE 
;is used to move an overlapping range 
;of memory upward, so it is used to de- 
;lete. Set FROML/FROMH to point to 
;the source area of memory, 
;DESTL/DESTH to point to the destina- 
;tion, and LLEN/HLEN to hold the 
;length of the area being moved. 

UMOVE LDA FROML 
	STA MOVLOOP+1 
	LDA FROMH 
	STA MOVLOOP+2 
	LDA DESTL 
	STA MOVLOOP+4 
	LDA DESTH 
	STA MOVLOOP+5 
	LDX HLEN 
	BEQ SKIPMOV 
MOV1 	LDA #0 
MOV2 	STA ENDPOS 
	LDY #0 
MOVLOOP LDA $0000,Y 
	STA $0000,Y 
	INY 
	CPY ENDPOS 
	BNE MOVLOOP 
	INC MOVLOOP+2 
	INC MOVLOOP+5 
	CPX #0 
	BEQ OUT
	DEX
	BNE MOV1
SKIPMOV LDA LLEN
	BNE MOV2
OUT	RTS
;DMOVE uses the same variables as UMOVE, but is used to move an
;overlapping block of memory down ward, so it is used to insert. If the block
;of memory to be moved does not overlap the destination area, then either
;routine can be used.
DMOVE	LDA HLEN
	TAX
	ORA LLEN
	BNE NOTNULL
	RTS
NOTNULL	CLC
	TXA
	ADC FROMH
	STA DMOVLOOP+2
	LDA FROML
	STA DMOVLOOP+1
	CLC
	TXA
	ADC DESTH
	STA DMOVLOOP+5
	LDA DESTL
	STA DMOVLOOP+4
	INX
	LDY LLEN
	BNE DMOVLOOP
	BEQ SKIPDMOV
DMOV1	LDY #255
DMOVLOOP LDA $0000,Y
	STA $0000,Y
	DEY
	CPY #255
	BNE DMOVLOOP
SKIPDMOV DEC DMOVLOOP+2 
	DEC DMOVLOOP+5 
	DEX 
	BNE DMOVl 
	RTS 

;REFRESH copies a screenful of text 
;from the area of memory pointed to by 
;TOPLIN. It works like a printer routine, 
;fitting a line of text between the screen 
;margins, wrapping words, and restarts 
;at the left margin after printing a car- 
;riage return. SpeedScript constantly calls 
;this routine while the cursor is blink- 
;ing, so it has to be very fast. To elimi- 
;nate flicker, it also clears out the end of 
;each line instead of first clearing the 
;screen. It stores the length of the first 
;screen line for the sake of the CHECK 
;routine (which scrolls up by adding 
;that length to TOPLIN), the last text 
;location referenced (so CHECK can see 
;if the cursor has moved off the visible 
;screen). 


REFRESH LDA #COLUMNS

	STA SCR 
	STA COLR 
	LDA #>SCRMEM
	STA SCR+1
	LDA #>COLMEM
	STA COLR+1 
	LDA TOPLIN 
	STA TEX 
	LDA TOPLIN+1
	STA TEX+1
	LDX #1
	LDA INSMODE
	STA WINDCOLR
	LDA SCRCOL
	STA 53280  ; VIC BORDER
PPAGE	LDY #0
PLINE	LDA TEXCOLR
	STA (COLR),Y
	LDA (TEX),Y
	STA LBUFF,Y
	INY
	AND #127
	CMP #RETCHAR
	BEQ BREAK
	CPY #COLUMNS
	BNE PLINE
	DEY
SLOOP	LDA (TEX),Y
	AND #127
NXCUR	CMP #32
	BEQ SBRK
	DEY
	BNE SLOOP
	LDY #39
SBRK	INY

BREAK	STY TEMP
	DEY
COPY	LDA LBUFF,Y
	STA (SCR),Y
	DEY
	BPL COPY
	LDY TEMP
	CLC
	TYA
	ADC TEX
	STA TEX
	LDA TEX+1
	ADC #0
	STA TEX+1
	CPX #1
	BNE CLRLN
	STY LENTABLE
CLRLN	CPY #COLUMNS
	BEQ CLEARED
	LDA #32
	STA (SCR),Y
	INY
	JMP CLRLN
CLEARED	CLC
	LDA
	ADC #COLUMNS
	STA SCR
	STA COLR
	BCC INCNOT
	INC SCR+1
	INC COLR+1
INCNOT	INX
	CPX #ROWS
	BEQ PDONE
	JMP PPAGE
	LDA TEX
	STA BOTSCR
	LDA TEX+1
	STA BOTSCR+1
	RTS

; comment from left column of page 99

ERASE	LDA TEXSTART
	STA TEX
	STA TOPLIN
	STA LASTLINE
	STA CURR
	LDA TEXSTART+1
	STA TEX+1
	STA TOPLIN+1
	STA LASTLINE+1
	STA CURR+1
	SEC
	LDA TEXEND+1
	SBC TEXSTART+1
	TAX
	LDA #32
CLRLOOP	LDY #255
	DEC TEX+1
	STA (TEX),Y
	INY
	INC TEX+1
CLR2	STA (TEX),Y
	INY
	BNE CLR2
	INC TEX+1
	DEX
	BNE CLR2
	STA (TEX),Y
	RTS

; PRMSG is used anytime we need to
; print something at the top of the screen
; (the command line). Pass it the address
; of the message to be printed by storing
; the low byte of the address in the accu
; mulator, and the high byte in the
; register. The message in memory must
; end with a zero byte. The routine does
; not add a carriage return.

PRMSG	STA TEMP
	STY TEMP+1
	LDY #0
PRLOOP	LDA (TEMP),Y
	BEQ PREXIT
	JSR CHROUT
	INY
	BNE PRLOOP
PREXIT	RTS
GETAKEY	JSR GETIN
	BEQ GETAKEY
	RTS
;The initialization routine sets up the
;memory map, clears out certain flags, 
;and enables the raster interrupt

INIT	LDA #147
	JSR CHROUT
	LDA #54
	STA MAP
	LDA #0
	STA INSMODE
	STA TEXSTART
	STA TEXEND
	STA TEXBUF
	STA BUFEND
	STA HUNTLEN
	STA REPLEN
	LDA #>END ;
	CLC
	ADC #1
	STA TEXSTART+1
	LDA #$CF
	STA TEXEND+1
	LDA #$D0
	STA TEXBUF+1
	LDA #$FF
	STA BUFEND+1
	STA FPOS+1
	JMP IOINIT
INIT2	JSR KILLBUFF
	LDA #128
	STA 650 ;TODO
	STA $9D ;TODO
	JSR HIGHLIGHT
	COPY16 MYNMI,$0318
	COPY16 TEXSTART,CURR
	JSR SYSMSG
	DO_PRMSG MSG2
	INC MSGFLG
	RTS

;The NOPS are here because I replaced
;a three-byte JSR CHECK with RTS.
;I did not want the size of the code
;or the location of any routines to change.
;JSR CHECK was originally inserted to fix
;a bug, but caused a bug itself.

	NOP
	NOP

;SYSMSG displays "SpeedScript" and the version.
SYSMSG	JSR TOPCLR
	DO_PRMSG MSG1
	LDA #0
	STA MSGFLG
	RTS

; This routine traps the RESTORE key. It reproduces
; some of the ROM code so that RS-232 is still supported
; (although SpeedScript does not directly support
; RS-232 output).
MYNMI	PHA
	TXA
	PHA
	TYA
	PHA
	LDA #$7F
	STA $DD0D
	LDY $DD0D
	BPL NOTRS
	JMP $FE72
;If RESTORE is pressed, we have to fix the 
;cursor in case it was lit.
NOTRS	LDA BLINKFLAG
	BEQ NOTCURSOR
	LDA UNDERCURS
	LDY #0
	STA (CURR),Y
NOTCURSOR	LDA #2
	STA WINDCOLR
	JSR CLRCHN
	JSR TOPCLR
	DO_PRMSG XITMSG
	JSR YORN
	BNE REBOOT
	JSR DELITE
	SEI
	LDA #$7F
	JMP $FE66
REBOOT	JSR DELITE
	LDX #$FA
	TXS
	JSR INIT2
	JMP MAIN
;
;TOPCLR keeps the command line clean.
;It is called before most messages.
;It's like a one-line clear-screen.
TOPCLR	LDX #COLUMNS-1
	LDA #32 ;SPACE
	STA SCRMEM,X
	DEX
	BPL TOPLOOP
	LDA #19 ;HOME
	JMP CHROUT
;Converts PETSCII to screen codes.
ASTOIN	PHA
	AND #128
	LSR
	STA TEMP
	PLA
	AND #63
	ORA TEMP
	RTS
;The MAIN loop blinks the cursor,
;checks for keystrokes, converts
;them from ASCII to screen codes,
;puts them in text at the CURRent position, and increments
;the CURRent position and LASTLINE. It also checks for special
;cases like the back-arrow and the return key and passes control
;characters to the CONTROL routine. SHIFTed spaces are turned into
;unSHIFTed ones. The INSMODE flag is checked to see if we should
;insert a space before a character.

MAIN LDY #0
	STY BLINKFLAG
	LDA (CURR),Y
	STA UNDERCURS
MAIN2	LDY #0
	LDA (CURR),Y
	EOR #$80
	STA (CURR),Y
	LDA BLINKFLAG
	EOR #1
	STA BLINKFLAG
	JSR REFRESH
WAIT	JSR GETIN
	BNE KEYPRESS
	LDA 162 ;TODO: label
	AND #16 
	BEQ WAIT
	LDA #0
	STA 162
	JMP MAIN2
KEYPRESS	TAX
	LDY #0
	LDA UNDERCURS
	STA (CURR),Y
	STY BLINKFLAG
	CPX #95
	BNE NOTBKS
	JSR LEFT
	LDA #32
	LDY #0
	STA (CURR),Y
	JMP MAIN
NOTBKS	LDA MSGFLG
	BEQ NOMSG
	TXA
	PHA
	JSR SYSMSG
	PLA
	TAX
NOMSG	TXA
	AND #127
	CMP #32
	BCC CONTROL
	CPX #160
	BNE NESHIFT
	LDX #SPACE
NESHIFT	TXA
	PHA
	LDY #0
	LDA (CURR),Y
	CMP #RETCHAR
	BEQ DOINS
	LDA INSMODE
	BEQ NOTINST
DOINS	JSR INSCHAR
NOTINST	PLA
	JSR ASTOIN
PUTCHR	LDY #0
	STA (CURR),Y
	JSR REFRESH
	SEC
	LDA CURR
	SBC LASTLINE
	STA TEMP
	LDA CURR+1
	SBC LASTLINE+1
	ORA TEMP
	BCC INKURR
	LDA CURR
	ADC #0
	STA LASTLINE
	LDA CURR+1
	ADC #0
	STA LASTLINE+1
INKURR	INC CURR
	BNE NOINC2
	INC CURR+1
NOINC2	JSR CHECK
	JMP MAIN
;CONTROl looks up a keyboard command in the list
;of control codes at CTBL. The first byte of
;CTBL is the actual number of commands. Once the
;position is found, this position is doubled as
;an index to the two-byte address table at VECT.
;The address of MAIN-1 is put on the stack, 
;simulating the return address; then the address
;of the command routine taken from VECT is pushed.
;We then perform an RTS. RTS pulls the bytes off
;the stack as if they were put there by a JSR.
;This powerful technique is used to simulate
;ON+GOTO in machine language.	
CONTROL	TXA
	LDX CTBL
SRCH	CMP CTBL,X
	BEQ FOUND
	DEX
	BNE SRCH
	JMP MAIN
FOUND	DEX
	TXA
	ASL
	TAX
	LDA #>MAIN-1
	PHA
	LDA #>MAIN-1
	PHA
	LDA VECT+1,X
	PHA
	LDA VECT,X
	PHA
	RTS
CTBL 	BYTE 39
	BYTE 29,157,137,133,2,12,138,134,20,148
	BYTE 4,19,9,147,135,139,5,136,140
	BYTE 22,145,17,159,18,24,26,16
	BYTE 28,30,6,1,11,8,31,3,131
	BYTE 10,141,7
VECT	WORD RIGHT-1,LEFT-1,WLEFT-1,WRIGHT-1,BORDER-1,LETTERS-1
	WORD SLEFT-1,SRIGHT-1,DELCHAR-1,INSCHAR-1,DELETE-1
	WORD HOME-1,INSTGL-1,CLEAR-1,PARIGHT-1,PARLEFT-1
	WORD ERAS-1,TLOAD-1,TSAVE-1,VERIFY-1
	WORD SLEFT-1,SRIGHT-1,CATALOG-1,INSBUFFER-1,SWITCH-1
	WORD ENDTEX-1,PRINT-1,FORMAT-1,DCMND-1
	WORD DELIN-1,ALPHA-1,KILLBUFF-1,HUNT-1,FREEMEM-1,TAB-1
	WORD LOTTASPACES-1,REPSTART-1,ENDPAR-1,SANDR-1
;The check routine (yadda yadda page 102)
CHECK	JSR CHECK2
	SEC
	LDA CURR
	SBC TOPLIN
	LDA CURR+1
	SBC TOPLIN+1
	BCS OK1
	SEC
	LDA TOPLIN
	SBC TEXSTART
	STA TEMP
	LDA TOPLIN+1
	SBC TEXSTART+1
	ORA TEMP
	BEQ OK1
	LDA CURR
	STA TOPLIN
	LDA CURR+1
	STA TOPLIN+1
	JSR REFRESH
OK1	SEC
	LDA BOTSCR
	SBC CURR
	STA TEX
	LDA BOTSCR+1
	SBC CURR+1
	STA TEX+1
	ORA TEX
	BEQ EQA
	BCS OK2
EQA	CLC
	LDA TOPLIN
	ADC LENTABLE
	STA TOPLIN
	LDA TOPLIN+1
	ADC #0
	STA TOPLIN+1
REF	JSR REFRESH
	JMP OK1
OK2	RTS

CHECK2	SEC
	LDA LASTLINE
	SBC TEXEND
	STA TEMP
	LDA LASTLINE+1
	SBC TEXEND+1
	ORA TEMP
	BCC CK3
	LDA TEXEND
	STA LASTLINE
	LDA TEXEND+1
	STA LASTLINE+1
CK3	SEC
	LDA CURR
	SBC TEXSTART
	STA TEMP
	LDA CURR+1
	SBC TEXSTART+1
	ORA TEMP
	BCS INRANGE
	LDA TEXSTART
	STA CURR
	LDA TEXSTART+1
	STA CURR+1
	RTS
INRANGE	SEC
	LDA CURR
	SBC LASTLINE
	STA TEMP
	LDA CURR+1
	SBC LASTLINE+1
	ORA TEMP
	BCS OUTRANGE
	RTS
OUTRANGE
	LDA LASTLINE
	STA CURR
	LDA LASTLINE+1
	STA CURR+1
	RTS
; move cursor right.
RIGHT	INC CURR
	BNE NOINCR
	INC CURR+1
NOINCR	JMP CHECK

; Cursor left.
LEFT	LDA CURR
	BNE NODEC
	DEC CURR+1
NODEC	DEC CURR
	JMP CHECK
; Word left. We look backward for a space.
WLEFT
	COPY16 CURR,TEX
	DEC TEX+1
	LDY #$FF
STRIP	LDA (TEX),Y
	CMP #SPACE
	BEQ STRLOOP
	CMP #RETCHAR
	BNE WLOOP
STRLOOP	DEY
	BNE STRIP 
WLOOP	LDA (TEX),Y
	CMP #SPACE
	BEQ WROUT
	CMP #RETCHAR
	BEQ WROUT
	DEY
	BNE WLOOP
	RTS
WROUT
	SEC
	TYA
	ADC TEX
	STA CURR
	LDA TEX+1
	ADC #0
	STA CURR+1
	JMP CHECK
;Word right. We scan forward
;for a space. OIDS is not a meaningful label.
WRIGHT	LDY #0
RLOOP	LDA (CURR),Y
	CMP #SPACE
	BEQ ROUT
	CMP #RETCHAR
	BEQ ROUT 
	INY
	BNE RLOOP
	RTS
ROUT	INY
	BNE OIDS
	INC CURR+1
	LDA CURR+1
	CMP LASTLINE+1
	BCC OIDS
	BNE LASTWORD
OIDS	LDA (CURR),Y
	CMP #SPACE
	BEQ ROUT
	CMP #RETCHAR
	BEQ ROUT
; add Y to CURR to move the cursor
; CHECK prevents illegal cursor movement.
; LASTWORD is called if the end
; of the word cannot be found within 255 characters.
ADYCURR	CLC
	TYA
	ADC CURR
	STA CURR
	LDA CURR+1
	ADC #0
	STA CURR+1
WRTN 	JMP CHECK

;ENDTEX is tricky,  (p103)

ENDTEX	LDA #0
	STA TOPLIN
	LDA LASTLINE+1
	SEC
	SBC #4
	CMP TEXSTART+1
	BCS SAFE
	LDA TEXSTART+1
SAFE	STA TOPLIN+1
	JSR REFRESH
	JMP LASTWORD

	MAC INC15
	INC {1}
	LDA {1}
	and #14
	sta {1}
	endm
;The raster interrupt automatically places
;SCRCOL into 53281 when appropriate. The AND
;keeps SCRCOL within a legal range (I know that's
;not really necessary)
BORDER	INC15 SCRCOL
	RTS
SCRCOL	BYTE 12 ; gray
;TEXCOLR (text color) is used in the REFRESH routine
;and stored into color memory. Both SCRCOL and TEXCOLR
;are stored within the SpeedScript code so that after
;they're changed, you can resave SpeedScript and it
;will come up with your color choice in the future.
LETTERS	INC15 TEXCOLR
	RTS
TEXCOLR	BYTE 11 ;dark gray

;Sentence left. We look backward for ending punctuation
;or a return mark, then go forward until we run out of spaces.

	MAC B_IF_PUNCT
	CMP #'.
	BEQ {1}
	CMP #'!
	BEQ {1}
	CMP #'?
	BEQ {1}
	ENDM
	
SLEFT
	COPY16 CURR,TEX
	DEC TEX+1
	LDY #$FF
PMANY	LDA (TEX),Y
	B_IF_PUNCT PSRCH
	CMP #RETCHAR
	BNE PSLOOP
PSRCH	DEY
	BNE PMANY
	RTS
PSLOOP	LDA (TEX),Y
	B_IF_PUNCT PUNCT
	CMP #RETCHAR
	BEQ PUNCT
	DEY
	BNE PSLOOP
	DEC TEX+1
	LDA TEX+1
	CMP TEXSTART
	BCS PSLOOP
	JMP FIRSTWORD
PUNCT	STY TEMP
	DEC TEMP
SKIPSPC	INY
	BEQ REPEAT
	LDA (TEX),y
	CMP #SPACE
	BEQ SKIPSPC
	dey
	jmp wrout
REPEAT	ldy temp
	jmp psloop
firstword	copy16 TEXSTART,CURR
	jmp CHECK
; Sentence right. We look for ending punctuation,
; then skip forward until we run out of spaces.
SRIGHT	LDY #0
	LDA (CURR),Y
	B_IF_PUNCT PUNCT2
	CMP #RETCHAR
	BEQ PUNCT2
	INY
	BNE SRLP
	inc CURR+1
	lda CURR+1
	cmp lastline+1
	beq srlp
srexit	jmp lastword
punct2	iny
	bne nofixcurr
	inc curr+1
	lda curr+1
	cmp lastline+1
	bcc nofixcurr
	beq nofixcurr
	jmp lastword
nofixcurr	lda (curr),y
	cmp #space
	beq punct2
	b_if_punct punct2
	cmp #retchar
	beq punct2
	jmp adycurr
; The text buffer starts at a fixed
; location, but the end of the buffer
; is changed as text is added to it.
; To clear the buffer, we just set
; the end of the buffer to the
; start of the buffer. No text is
; actually erased.
KILLBUFF	COPY16 TEXBUF,TPTR
	JSR TOPCLR
	DO_PRMSG KILLMSG
	LDA #1
	STA MSGFLG
	RTS

; This is the second level of the
; general-purpose delete routines. (p.105)

	; I think this is just an equals check
	; will rename it to CMP16 later
	MAC SBC_ORATEMP
	sec
	lda {1}
	sbc {2}
	sta temp
	lda {1}+1
	sbc {2}+1
	ora temp
	ENDM

DEL1	SBC_ORATEMP CURR,TEXSTART
	BNE DEL1A
DELABORT	PLA
	PLA
	RTS
DEL1A	COPY16 CURR,FROML
	RTS
DEL2	SEC
	LDA CURR
	STA DESTL
	eor #$ff
	adc froml
	sta goblen
	lda curr+1
	sta desth
	eor #$ff
	adc fromh
	sta gorlen+1
DELC	copy16 froml,fromsav
	lda destl
	sta destsav
	sta froml
	lda desth
	sta destsav+1
	sta fromh
	sec
	lda gorlen+1
	adc tptr+1
	cmp bufend+1
	bcc gosav
	top_prmsg buferr
	lda #1
	sta msgflg
	lda #0
	sta 198 ; TODO label
	rts

GOSAV	copy16 tptr,destl
	lda gorlen
	sta llen
	clc 
	adc tptr
	sta tptr
	lda gorlen+1
	sta hlen
	adc tptr+1
	sta tptr+1
	lda #0
	sta $D01A ;TODO: label
	lda #52
	sta MAP
	jsr umove
	lda #54
	sta MAP
	lda #1
	sta $D01A

	copy16 fromsav,froml
	copy16 destsav,destl
	sec
	lda lastline
	sbc destl
	sta llen
	lda lastline+1
	sbc desth
	sta hlen
	jsr umove
	sec
	lda lastline
	sbc goblen
	sta lastline
	lda lastline+1
	sbc goblen+1
	sta lastline+1
	rts
;Most delete commands end up calling
;the above routines. (p106)
DELCHAR	jsr del1
	jsr left
	jsr del2
fixtp 	sec
	lda tptr
	sbc #1
	sta tptr
	lda tptr+1
	sbc #0
	sta tptr+1
	rts
;this is called from CTRL-back arrow.
;We first check to see if SHIFT is also
;held down. If so, we go to another routine
;that "eats" spaces.
DELIN	LDA 653 ;TODO: label
	CMP #5
	BNE DODELIN
	JMP EATSPACE
DODELIN
	JSR RIGHT
	JSR DEL1
	JSR LEFT
	JSR DEL2
	JMP FIXTP

;Called by CTRL-D.  (etc)
DELETE	JSR KILLBUFF
	LDA #2
	STA WINDCOLR
	JSR TOPCLR
	DO_PRMSG DELMSG
	JSR GETAKEY
	PHA
	JSR SYSMSG
	PLA
	AND #191
	CMP #23 ; "W"
	BNE NOTWORD
DELWORD	JSR DEL1
	JSR WLEFT
	JMP DEL2
NOTWORD	CMP #19 ; "S"
	BNE NOTSENT
DELSENT	JSR DEL1
	JSR SLEFT
	JMP DEL2
NOTSENT	CMP #16 ; "P"
	BNE NOTPAR
	JSR DEL1
	JSR PARLEFT
	JMP DEL2
NOTPAR	RTS

	
;Home the cursor. if the cursor
;is already home, move the cursor
;to the top of text.
HOME	SBC_ORATEMP curr,toplin
	beq tophome
	copy16 toplin,curr
	rts
tophome	copy16 texstart,curr
	jmp check

; This deletes all spaces between the
; cursor and following nonspace text.
; Sometimes inventing labels can be fun.
EATSPACE
	COPY163 CURR,TEX,DESTL
	ldy #0
spcsrch	lda (tex),y
	cmp #space
	bne outspace
	iny
	bne spcsrch
	lda tex+1
	cmp lastline+1
	bcc goinc
	copy16 lastline,tex
	ldy #0
	jmp outspace
goinc	inc tex+1
	jmp spcsrch
outspace
	clc
	tya
	adc tex
	sta froml
	lda #0
	adc tex+1
	sta fromh

	; {1} - {2} -> {3}
	mac sub163
	sec
	lda {1}
	sbc {2}
	sta {3}
	lda {1}+1
	sbc {2}+1
	sta {3}+1
	endm

	sub163 lastline,destl,llen
	sub163 froml,destl,goblen
	sub163 lastline,goblen,lastline
	
;Inserts 255 spaces. Notice how it and other
;insert routines use TAB2.
LOTTASPACES
	LDA #255
	STA INSLEN
	JMP TAB2
	
tab	lda #5
	sta inslen
	jsr tab2
	lda (curr),y
	cmp #space
	bne noincy
	iny
noincy	jmp adycurr
tab2	lda #0
	sta inslen+1
	jsr insblock
	lda #space
	ldx inslen
	ldy #0
fillsp	sta (curr),y
	iny
	dex
	bne fillsp
	rts
;SHIFT-RETURN calls this. It inserts
;two spaces, fills them with return marks,
;then calls tAB for a margin indent. Not
;much code for a useful routine.
ENDPAR	JSR INSCHAR
	JSR INSCHAR
	LDA #RETCHAR
	LDY #0
	STA (CURR),Y
	INY
	STA (CURR),Y
	JSR REFRESH
	JSR RIGHT
	JSR RIGHT
	JMP TAB
;insert a single space:
inschar	LDA #1
	sta inslen
	lda #0
	sta inslen+1
	jsr insblock
	lda #space
	ldy #0
	sta (curr),y
	jmp check
;A general routine to insert as many
;spaces as are specified by INSLEN.
INSBLOCK
	clc
	lda lastline
	adc inslen ; discarded?
	lda lastline+1
	adc inslen+1
	cmp texend+1
	bcc okins
	pla
	pla
	jmp inout
okins	clc
	lda curr
	sta froml
	adc inslen
	sta destl
	lda curr+1
	sta froml+1
	adc inslen+1
	sta destl+1
	sub163 lastline,froml,llen
	jsr dmove
	add16 lastline,inslen
	rts
;toggle insert mode. The INSMODE
;flag doubles as the color of the
;command line.
INSTGL	LDA INSMODE
	EOR #14     ; TODO: constant
	STA INSMODE
	RTS
;Another example of modular code.
YORN	DO_PRMSG YMSG
YORNKEY JSR $FF9F  ; TODO: constant
	JSR GETIN
	BEQ YORNKEY
	CMP #147  ;user is spamming CLR/HOME
	BEQ YORNKEY ;ignore it
	AND #127
	CMP #'Y
	RTS
;Erase all text. (p108)
CLEAR	LDA #2  ;red!
	STA WINDCOLR 
	JSR TOPCLR
	DO_PRMSG CLRMSG
	JSR YORN
	BEQ DOIT
	JMP SYSMSG
DOIT LDX #$FA
	TXS
	JSR ERASE
	JSR INIT2
	JMP MAIN
;Paragraph right.
PARIGHT LDY #0
PARLP 	lda (CURR),Y
	cmp #retchar
	beq retfound
	iny
	bne parlp
	inc curr+1
	lda curr+1
	cmp lastline+1
	bcc parlp
	beq parlp
	jmp lastword
retfound iny
	bne goady
	inc curr+1
goady	jmp adycurr
;Paragraph left.
PARLEFT	COPY16 CURR,TEX
	dec tex+1
	ldy #$ff
parloop	lda (tex),y
	cmp #retchar
	beq retf2
parcont	dey
	cpy #255
	bne parloop
	dec tex+1
	lda tex+1
	cmp texstart+1
	bcs parloop
	jmp firstword
retf2	sec
	tya
	adc tex
	sta tex
	lda #0
	adc tex+1
	sta tex+1
	sbc_oratemp tex,curr
	bne textocurr
	sty temp
	clc
	lda tex
	sbc temp
	sta tex
	lda tex+1
	sbc #0
	sta tex+1
	jmp parcont
tectocurr
	copy16 tex,curr
	jmp check
;tis enables the raster interrupt. (p109)
HIGHLIGHT
	SEI
	LDA #0
	STA $DC0E
	LDA #27
	STA $D011
	COPY16 IRQ,$0314
	LDA #1
	STA $D01A
	STA $D012
	CLI
	RTS
IRQ	LDA #58
	LDY WINDCOLR
	CMP $D012
	BNE MID
	LDA #1
	LDY SCRCOL
MID	STY $D021
	STA $D012
SKIP	CMP #1
	BEQ DEFALT
	LDA #1
	STA $D019
	JMP $FEBC
defalt	LDA #1
	STA $D019
	JMP $EA31

;ERAS is called by CTRL-E. It works
;much like CTRL-D. Notice that the
;ORA #64 allows ....
ERAS	LDA 653
	AND #1
	BNE ERAS1
	JSR KILLBUFF
ERAS1	JSR TOPCLR
	DO_PRMSG ERASMSG
ERASAGAIN
	LDY #0
	LDA (CURR),Y
	EOR #$80
	STA (CURR),Y
	JSR REFRESH
	LDY #0
	LDA (CURR),Y
	EOR #$80
	STA (CURR),Y
	lda #2
	sta windcolr
	jsr getakey
	ora #64
	cmp #'W
	bne NOWORD
erasword jsr ERA1
	jsr wright
	jmp era2
noword	cmp #'S
	bne UNSENT
erasent jsr ERA1
	jsr sright
	jmp era2
	cmp #'P
	bne NOPAR
	jsr ERA1
	jsr paright
	jmp era2
nopar	jsr check
	jmp sysmsg
era1	copy163 curr,destl,savcurr
	rts
era2	sec
	lda curr
	sta froml
	sbc savcurr
	sta goblen
	lda curr+1
	sta froml+1
	sbc savcurr+1
	sta goblen+1
	jsr delc
	copy16 savcurr,cur
	jsr refresh
	jmp erasagain
;the INPUT routine is used to get responses
;from the command line.
INPUT	LDA #39
	SBC 211
	STA LIMIT
INP1	LDY #0
	LDA #153
	JSR CHROUT
	LDA #18 ; Ctrl-R
	JSR CHROUT
	LDA #space
	JSR CHROUT
	LDA #157
	JSR CHROUT
	sty inlen
	jsr getakey
	ldy inlen
	sta temp
	lda #146
	jsr CHROUT 
	LDA #space
	JSR CHROUT
	LDA #157
	JSR CHROUT
	LDA #155
	JSR CHROUT
	LDA TEMP
	CMP #13
	BEQ INEXIT
	CMP #20 ;petscii DEL key
	BNE NOBACK
	DEY
	BPL NOTZERO
	iny
	jmp cursin
notzero	lda #157
	jsr chrout
	jmp cursin
noback	lda temp
	and #127
	cmp #space
	bcc cursin
	cpy limit
	beq cursin
	lda temp
	sta inbuff,y
	jsr chrout
	lda #0
	sta 212
	sta 216
	iny
	jmp cursin
inexit	jsr chrout
	lda #0
	sta inbuff,y
	tya
	rts

; i/o

TSAVE
	top_prmsg savmsg
	jsr topen
	bcs error
	copy16 texstart,tex
	ldx lastline
	ldy lastline+1
	lda #tex
	jsr save
	bcs error
; location $90 is the location of the
; kernel's STatus flag. It's shorter
; to use LDA $90 than JSR READST.
	lDA $90
	AND #191
	BNE ERROR
	JMP FINE
;The ERROR message routine. May this
;routine never be called when you use
;SpeedScript, but that's too much to 
;ask for.
ERROR	BEQ STOPPED
	LDA DVN
	CMP #8
	BCC TAPERR
	JSR READERR
	JMP EXIT
TAPERR	LDA DVN
	CMP #1
	BEQ TAPERR
	JSR TOPCLR
	DO_PRMSG FNF
ERXIT	JSR HIGHLIGHT
	LDA #1
	STA MSGFLG
	RTS
STOPPED	JSR TOPCLR
	DO_PRMSG BRMSG
	JMP ERXIT
DVN	BYTE 0

;TOPEN
TOPEN	JSR INPUT
	BEQ OPABORT
OP2	DO_PRMSG TDMSG
	JSR GETAKEY
	LDX #8
	CMP #'D
	BEQ OPCONT
	LDX #1
	CMP #'T
	BEQ OPCONT
OPABORT	JSR SYSMSG
	PLA
	PLA
	RTS
OPCONT	STX DVN
	LDA #1
	LDY #0
	jsr setlfs
	LDY #0
	cpx #1
	beq skipdisk
	lda inbuff,y
	cmp #'@
	nop
	nop
	lda inbuff+1,y
	cmp #':
	beq skipdisk
	lda inbuff+2,y
	cmp #':
	beq skipdisk
; cbm dos magic p.111
addzero lda #'0
	sta filename
	lda #':
	sta filename+1
COPY1	lda inbuff,y
	sta filename+2,y
	iny
	cpy inlen
	bcc copy1
	beq copy1
	iny
	jmp setname
skipdisk	lda inbuff,y
	sta filename,y
	iny
	cpy inlen
	bne skipdisk
setname sty fnlen
	top_prmsg inbuff
	lda fnlen
	ldx #<filename
	ldx #>filename
	jsr SETNAM
	lda #13 ;cr
	jsr chrout
	jmp delite
; called by CTRL-\ to enter a format code.
; It checks insert mode and inserts if necessary.
FORMAT	JSR TOPCLR
	DO_PRMSG FORMSG
	JSR GETAKEY
	JSR ASTOIN
	ORA #$80
	PHA
	LDA INSMODE
	BEQ NOINS
	JSR INSCHAR
NOINS	JSR SYSMSG
	PLA
	JMP PUTCHR
;p112
TLOAD
	SBC_ORATEMP CURR,TEXSTART
	BEQ load2
	lda #5
	sta windcolr
load2	top_prmsg loadmsg
	jsr topen
	lda windcolr
	cmp #5
	beq noer
	jsr erase
noer	lda #0
	ldx curr
	ldy curr+1
ldver	jsr load
	bcc lod
	jmp error
lod	stx lastline
	sty lastline+1
fine	jsr clall
	top_prmsg okmsg
	jmp erxit
;verify
verify	top_prmsg vermsg
	jsr topen
	lda #1
	ldx texstart
	ldy texstart+1
	jsr load
	lda $90
	and #191
	beq fine
	top_prmsg vererr
	jmp erxit
;DELITE turns off the raster interrupt.
DELITE	SEI
	LDA #0
	STA $D01A
	STA 53280
	STA 53281
	LDA #$31
	sta $0314
	lda #$EA
	sta $0315
	lda #1
	sta $dc0e
	cli
	rts
; disk dir
catalog lda #147 ;CLR
	jsr chrout
	lda #13 ;cr
	jsr chrout
	jsr delite
	jsr dir
	lda #13 ;cr
	jsr chrout
	do_prmsg dirmsg
waitkey	jsr getin
	cmp #13
	bne waitkey
	jsr highlight
	jmp sysmsg
endir
	jsr clrchn
	lda #1
	jsr close
	rts
dir	jsr clall
	lda #1
	ldx #0
	ldy #0
	jsr setlfs
	lda #1
	ldx #<dirname
	ldy #>dirname
	jsr setnam
	jsr open
	bcs endir
	ldx #1
	jsr chkin
	jsr dchrin
	jsr dchrin
dirloop	jsr dchrin
	jsr dchrin
	beq endir
pause	jsr clrchn
	jsr getin
	cmp #space
	bne nopause
	jsr getakey
nopause ldx #1
	jsr chkin
	jsr dchrin
	pha
	jsr dchrin
	tay
	pla
	tax
	tya
	ldy #55
	sty map
	jsr $bdcd ; BASIC number output
	ldy #54
	sty map
	lda #space
	jsr chrout
inloop	jsr dchrin
	beq dline
	jsr chrout
	jmp inloop
dline 	cmp #13
	jsr chrout
	jmp dirloop
dchrin  jsr chrin
	pha
	lda $90
	and #191
	beq nosterr
	pla
	pla
	pla
	jmp endir
nosterr	pla
	rts
;
; oh boy
aschex  ldx #0
	stx bcd
	stx bcd+1
	stx hex
	stx hex+1
digit	sec
	lda (tex),y
	sbc #'0
	bcc nonum
	cmp #10 ;radix
	bcs nonum
	asl bcd
	rol bcd+1
	asl bcd
	rol bcd+1
	asl bcd
	rol bcd+1
	asl bcd
	rol bcd+1
	ora bcd
	sta bcd
	iny
	bne digit
	inc tex+1
	jmp digit
nonum	sed
	lda bcd
	ora bcd+1
	beq donenum
	sec
	lda bcd
	sbc #1
	sta bcd
	lda bcd+1
	sbc #0
	sta bcd+1
	inc hex
	bne nohexinc
	inc hex+1
nohexinc jmp dechex
donenum	lda hex
	cld
	rts

;p113

insbuffer
	sec
	sub163 tptr,texbuf,buflen
	ora buflen
	bne okbuff
	jsr topclr
	do_prmsg insmsg
	lda #1
	sta msgflg
	rts

okbuff	clc
	lda curr
	sta froml
	adc buflen
	sta destl
	lda curr+1
	sta froml+1
	adc buflen+1
	sta destl+1
	sub163 lastline,froml, llen
	clc
	adc desth
	cmp texend+1
	bcc okmov
	jsr topclr
	do_prmsg inserr
	lda #1
	sta msgflg
	rts
	
okmov jsr dmove
	clc
	lda buflen
	sta llen
	adc lastline
	sta lastline
	lda buflen+1
	sta llen+1
	adc lastline+1
	sta lastline+1
	copy16 curr,destl
	copy16 texbuf,froml
	lda #0
	sta $d01a
	lda #52
	sta map
	jsr umove
	lda #54
	sta map
	lda #1
	sta $d01a
	jmp check

switch
	ldy #0
	lda (curr),y
	tax
	iny
	lda (curr),y
	dey
	sta (curr),y
	iny
	txa
	sta (curr),y
	rts

alpha
	ldy #0
	lda (curr),y
	and #63
	beq notalpha
	cmp #27
	bcs notalpha
	lda (curr),y
	eor #64
	sta (curr),y
notalpha	jmp right

intoas 	sta temp
	and #$3f
	asl temp
	bit temp
	bpl isk1
	ora #$80
isk1	bvs isk2
	ora #$40
isk2	sta temp
	rts
	
;oh boy printer stuff p114

DEFTAB BYTE 5,75,66,5,58,1,1,1,0,1,0,80
	
PRCODES BYTE 27,14,15,18

pchrout	sta pcr
	txa
	pha
	tya
	pha
	sec
	lda pagenum
	sbc startnum
	lda pagenum+1
	sbc startnum+1
	bcc skipout
	lda pcr
	jsr chrout
shiftfreeze	lda 653
	and #1
	sta 53280
	bne shiftfreeze
	lda $91
	cmp #$7f
	bne skipout
	inc 53280
	jsr cr
	jmp pexit
skipout	pla
	tay
	pla
	tax
	lda pcr
	rts
;display "Printing..."
prin	jsr topclr
	do_prmsg prinmsg
pbort	jmp pexit
;ctrlp
print	lda scrcol
	sta savcol
	lda #0
	sta windcolr
	sta 53280
	sta scrcol
	jsr setnam
	lda #4
	sta devno
	ldy #7
	lda 653
	and #1
	bne askques
	jmp overques
askques	top_prmsg choosemsg
	jsr getakey
	and #127
	ldx #3
	stx devno
	cmp #'S
	beq prcont
notscreen
	ldx #8
	stx devno
	cmp #'D
	beq dofn
	cmp #'P
	bne pbort
	top_prmsg devmsg
	jsr getakey
	sec
	sbc #48
	cmp #4
	bcc pbort
	cmp #80
	bcs pbort
	sta devno
	jmp prcont

dofn	top_prmsg fnmsg
	jsr input
	beq pbort
	ldy inlen
	lda #',
	sta inbuff,y
	iny
	lda #'W
	sta inbuff,y
	iny
	sty inlen
	lda inlen
	ldx #<inbuff
	ldy #>inbuff
	jsr setnam
prcont	lda devno
	tay
	cmp #4
	bcc overques
	cmp
	bcs overques
notd2	top_prmsg sadrmsg
	jsr getakey
	sec
	sbc #'0
	tay
	bpl OVERQUES
	jmp pbort
overques	lda #1
	ldx devno
	jsr setlfs
	jsr prin
	lda #1
	jsr close
	jsr open
	ldx #1
	jsr chrout
	bcc prok	
	jmp pexit
;reset flags

prok	ldx #0
	stx ftlen
	stx hdlen
	stx needasc
	stx underline
	stx linefeed
	
;copy definition
copydef	lda deftab,x
	sta lmargin,x
	inx
	cpx #12
	bne copydef
	lda #$ff
	sta line
	sta nomarg
	ldx #4
copydefs	lda prcodes-1,x
	sta codebuffer+48,x
	dex
	bne copydefs

retex	copy16 texstart,tex
;main printing loop
ploop	ldy #0
	sty pos
	cpy nomarg
	beq ploop1
	lda lmargin
	sta pos
ploop1	lda (tex),y
	bpl notsp
	jmp special
notsp	cmp #retchar
	beq foundspace
notret	sta prbuff,y
	iny
	inc pos
	lda pos
	cmp rmargin
	bcc ploop1
	sty finpos
findspace
	lda (tex),y
	cmp #space
	beq foundspace
	dec pos
	dey
	bne findspace
	ldy finpos
	jmp overstor
fspace	iny
	lda (tex),y
	cmp #space
	beq foundspace
	dey
foundspace
	sty finpos
overstor
	tya
	sec
	adc tex
	sta tex
	lda tex+1
	adc #0
	sta tex+1
	ldy #0
dobuff	lda line
	cmp #$ff
	bne dobuf2
	jsr top
dobuf2	lda nomarg
	beq overmarg
	jsr lmarg
overmarg
	sec
	rol nomarg
	lda finpos
	sta endpos
	lda #<prbuff
	sta indir
	lda #>prbuff
	sta indir+1
	jsr bufprt

zbuff	jsr crlf
	lda line
	cmp botmarg
	bcc notpage
	jsr page
;Have we reached end of text?
	
notpage	sec
	sbc_oratemp tex,lastline
	beq dorpt
	bcc dorpt
;check for footer
	lda ftlen
	beq pxit
	lda #0
	sta hdlen
	sta topmarg
	jsr page

pxit	lda devno
	cmp #3
	bne pexit
	jsr getakey
pexit	jsr stop
	beq pexit
	lda #1
	jsr close
	jsr clall
	lda savcol
	sta scrcol
	ldx #$fa
	txs
	jsr sysmsg
	jmp main
dorpt 	jmp ploop
page	sec
	lda pagelength
	sbc line
	tay
	dey
	dey
	beq nosk
	bmi nosk

nexpage	jsr cr
	dey
	bne nexpage

nosk	lda ftlen
	beq skipft
	lda #<ftbuff
	sta indir
	lda #>ftbuff
	sta indir+1

	jsr lmarg
	jsr bufprt
skipft	jsr cr
	jsr cr
	jsr cr

;increment the page number
	inc pagenum
	bne noipn
	inc pagenum+1
noipn	lda continuous
	bne top
	lda devno
	cmp #3
	beq top
	cmp #0
	beq top
	sec
	lda pagenum
	sbc startnum
	lda pagenum+1
	sbc startnum+1
	bcc top
	jsr clrchn
	top_prmsg waitmsg
	jsr getakey
	jsr prin
	ldx #1
	jsr chkout
;print header
top	lda hdlen
	beq noheader
	sta endpos
	lda #<hdbuff
	sta indir
	lda #>hdbuff
	sta indir+1
	jsr lmarg
	jsr bufprt
noheader
	ldy topmarg
	sty line
	dey
	beq skiptop
	bmi skiptop
toplp	jsr cr
	dey
	bne toplp
skiptop	rts
;left margin routine
lmarg	lda #space
	ldy lmargin
	sty pos
	beq lmexit
lmloop	jsr pchrout
	dey
	bne lmloop
lmexit rts
	
crlf	ldy spacing
	clc
	tya
	adc line
	sta line
crloop	jsr cr
	dey
	bne crloop
	rts

cr	lda #13
	jsr pchrout
	lda linefeed
	beq nolf
	jsr pchrout
nolf 	rts

special	sta savchar	
	and #127
	jsr intoas
	ldx sptab
srchsp	cmp sptab,x
	beq fsp
	dex
	dne srchsp
	dec pos
	jmp define
fsp	dex
	txa
	asl
	tax
	sty ysave
	lda #>spcont-1
	pha
	lda #<spcont-1
	pha
	lda spvect+1,x
	pha
	lda spvect,x
	pha
	rts
spcont	sec
	lda ysave
	adc tex
	sta tex
	lda tex+1
	adc #0
	sta tex+1
	jmp ploop
spcexit	lda (tex),y
	cmp #retchar
	beq noad
	dey
noad	sty ysave
	rts
sptab	byte 18
	text "WALRTBSNHF@P?XMIGJ" 

spvect	word pw-1,as-1,lm-1,rm-1,tp-1
	word bt-1,sp-1,nx-1,hd-1,ft-1
	word pn-1,pl-1,spage-1,across-1
	word mrelease-1,comment-1,link-1
	word lfset-1

;m Margin release.
; INY is used to skip over the format character.
MRELEASE
	INY
	LDA #0
	STA NOMARG
	JMP SPCEXIT
;x Columns across
ACROSS
	INY
	JSR ASCHEX
	STA PAGEWIDTH
	JMP SPCEXIT

	mac prcode
	JSR ASCHEX
	STA {1}
	endm
	mac prcode16
	prcode {1}
	LDA HEX+1
	STA {1}
	endm
;? Print starting at specified page
SPAGE
	INY
	prcode16 startnum
	JMP SPCEXIT
;@ set starting page default number
PN
	INY
	prcode16 pagenum
	JMP SPCEXIT

;p page length
PL	INY
	PRCODE PAGELENGTH
	JMP SPCEXIT 

;w set page wait mode
PW	LDA #0
	STA CONTINUOUS
	INY
	JMP SPCEXIT

LFSET	LDA #10
	STA LINEFEED
	INY
	JMP SPCEXIT
;a set true ASCII mode
AS	INY
	LDA #1
	STA NEEDASC
	JMP SPCEXIT
LM	iny
	prcode lmargin
	jmp spcexit
RM	iny
	prcode rmargin
	jmp spcexit
TP	iny
	prcode topmarg
	jmp spcexit
BT	iny
	prcode botmarg
	jmp spcexit
SP	iny
	prcode spacing
	jmp spcexit
;n Jump to next page
nx	ldy ysave
	iny
	tya
	pha
	jsr page
	pla
	tay
	sty ysave
	rts
;h define header
HD	JSR PASTRET
	DEY
	STY HDLEN
	LDY #1
HDCOPY	LDA (TEX),Y
	STA HDBUFF-1,Y
	INY
	CPY HDLEN
	BCC HDCOPY
	BEQ HDCOPY
	INY
	JMP SPCEXIT
;Skip just past the return mark
PASTRET	INY
	LDA (TEX),Y
	CMP #RETCHAR
	BNE PASTRET
	RTS
;f define header
FT	JSR PASTRET
	DEY
	STY FTLEN
	LDY #1
FTCOPY	LDA (TEX),Y
	STA FTBUFF-1,Y
	INY
	CPY FTLEN
	BCC FTCOPY
	BEQ FTCOPY
	INY
	JMP SPCEXIT

;i ignore a line 
COMMENT	JSR PASTRET
	JMP SPCEXIT

; Define programmable printeys?

DEFINE	INY
	LDA (TEX),Y
	CMP #'=
	BEQ DODEFINE
	DEY
	LDA SAVCHAR
	JMP NOTRET
DODEFINE
	INY
	JSR ASCHEX
	PHA
	LDA SAVCHAR
	AND #127
	TAX
	PLA
	STA CODEBUFFER,X
	JSR SPCEXIT
	JMP SPCONT
;Link to next file
LINK	INY
	LDX #8
	LDA (TEX),Y
	AND #63
	CMP #'D
	BEQ LINK2
	ldx #1
	CMP #'T
	BEQ LINK2
	jmp probt
link2	stx dvn
	iny
	lda (tex),y
	cmp #':
	beq linkloop
	jmp probt
linkloop
	iny
	lda (tex),y
	cmp #retchar
	beq outnam
	jsr intoas
	sta filename-3,y
	jmp linkloop
outnam	tya
	sec
	sbc #3
	ldx #<fiename
	ldy #>filename
	jsr setnam
	jsr clrchn
	lda #2
	jsr close
	lda #2
	ldx dvn
	ldy #0
	jsr setlfs
	jsr erase
	lda #0
	ldx curr
	ldy curr+1
	jsr load
	bcc oklod
	jmp pbort
oklod
	stx lastline
	sty lastline+1
	pla
	pla
	ldx #1
	jsr chkout
	jmp retex
dcmnd jsr clall
	lda #0
	jsr setnam
	lda #15
	ldx #8
	ldy #15
	jsr setlfs
	jsr open
	bcc okd
dcout	lda #15
	jsr close
	jsr clall
	jsr sysmsg
okd	top_prmsg dcmsg
	jsr input
	beq readerr
	ldx #15
	jsr chkout
	bcs dcout
	do_prmsg inbuff
	lda #13 ;cr
	jsr chrout
	jsr clrchn
	
readerr jsr clall	
	lda #0
	jsr setnam
	lda #15
	ldx #8
	ldy #15
	jsr setlfs
	jsr open
	bcs dcout
	jsr topclr
	ldx #15
	jsr chkin
	jsr input
	jsr clrchn
	lda #15
	jsr close
	jsr clall
	lda #1
	sta msgflg
	rts

;Global search and replace. This just
;links together the search-specify routine,
;the replace-specify routine,
;then repeatedly calls Hunt and Replace,
;until Hunt returns "Not Found." (FPOS+1
;is $FF after a search failure.)
SANDR	JSR RESET
	LDA HUNTLEN
	BEQ NOSR
	JSR ASKREP
SNR	JSR CONTSRCH
	LDA FPOS+1
	CMP #$FF
	BEQ NOSR
	JSR REPL
	JSR REFRESH
	JMP SNR
NOSR	JMP SYSMSG
;if SHIFT is held down, we ask for and store
;the hunt phrase. If SHIFT is not down, we
;perform the actual hunt. The line in the INBUFF is compared with
;characters in text. (p121)
HUNT	LDA 653
	CMP #5
	BNE CONTSRCH
	top_prmsg srchmsg
	jsr input
	sta huntlen
	bne oksrch
	jmp sysmsg
oOKSRCH	LDY #0
TOBUFF	LDA INBUFF,Y
	STA HUNTBUFF,Y
	INY
	CPY INLEN
	BNE TOBUFF
	JMP SYSMSG
CONTSRCH	COPY16 CURR,TEX
	LDA #$FF
	STA FPOS+1
	LDY #1
	LDX #0
	LDA HUNTLEN
	BEQ NOTFOUND
SRCH1	LDA HUNTBUFF,X
	JSR ASTOIN
	CMP (TEX),Y
	BEQ CY
	LDX #$FF
CY	INY
	BNE NOVFL
	BCD NOTFOUND
NOVFL	INX
	CPX HUNTLEN
	BNE SRCH1
	CLC
	TYA
	ADC TEX
	STA TEMP
	LDA TEX+1
	ADC #0
	STA TEMP+1
	LDA LASTLINE
	cmp temp
	LDA LASTLINE+1
	sbc temp+1
	bcc notfound
	sec
	lda temp
	sbc huntlen
	sta curr
	sta fpos
	lda temp+1
	sbc #0
	sta curr+1
	sta fpos+1
	jsr check
	rts

	
notfound
	top_prmsg nfmsg
	lda #1
	sta msgflg
	rts
;replace
repstart
	lda 653
	cmp #5
	bne repl
askrep	top_prmsg repmsg
	jsr input
	sta replen
	beq norep
	ldy #0
repmov	lda inbuff,y
	sta repbuff,y
	iny
	cpy inlen
	bne repmov
norep	jmp sysmsg
repl	sec
	lda curr
	sta destl
	sbc fpos
	sta temp
	lda curr+1
	sta destl+1
	sbc fpos+1
	ora temp
	bne norepl
	lda #$ff
	sta fpos+1
	clc
	lda huntlen
	adc curr
	sta froml
	lda #0
	adc curr+1
	sta fromh
	sub lastline,destl, llen
	jsr umove
	lda lastline
	sbc huntlen
	sta lastline
	lda lastline+1
	sbc #0
	sta lastline+1
	lda replen
	beq norepl
	sta inslen
	lda #0
	sta inslen+1
	jsr insblock
	ldy #0
reploop	lda repbuff,y
	jsr astoin
	sta (curr),y
	iny
	cpy replen
	bne reploop
	clc
	lda curr
	adc replen
	sta curr
	lda curr+1
	adc #0
	sta curr+1
norepl	jmp check

bufprt	ldy #0
buflp	cpy endpos
	beq endbuff
	lda (indir),y
	bmi spec2
	jsr intoas
	jsr convasc
	jsr pchrout
;underline mode
	lda underline
	beq nobrk
	lda #8 ;backspace
	jsr pchrout
	lda #95 ;underscore
	jsr pchrout
NOBRK	INY
	jmp buflp
endbuff rts
;stage 2 format commands
spec2	sty ysave
	and #127
	sta savchar
	jsr intoas

OTHER	cmp #'C
	bne notcenter
	sec
	lda pagewidth
	sbc endpos
	lsr
	sec
	sbc lmargin
	tay
	lda #space
	jsr pchrout
	dey
	bne cloop
	ldy ysave
	jmp nobrk
;edge right
notcenter
	cmp #'E
	bne notedge
edge	sec
	lda rmargin
	sbc endpos
	sec
	sbc lmargin
	tay
	lda #space
	jmp cloop
notedge
	cmp #'U
	bne notog
	lda underline
	eor #1
	sta underline
notog
	cmp #'#
	bne docodes
dopgn	sty ysave
	ldx pagenum
	lda pagenum+1
	display_number
	ldy ysave
	jmp nobrk
docodes	LDX SAVCHAR
	lda codebuffer,x
	jsr pchrout
	jmp nobrk
convasc	ldx needasc
	beq skipasc
	sta temp
	and #127
	cmp #'A
	bcc skipasc
	cmp #'[
	bcs skipasc
	tax
	lda temp
	and #128
	eor #128
	lsr
	lsr
	sta temp
	txa
	ora temp
skipasc rts
;display free memory
FREEMEM
	JSR TOPCLR
	SEC
	LDA TEXEND
	SBC LASTLINE
	TAX
	LDA TEXEND+1
	SBC LASTLINE+1
	DISPLAY_NUMBER
	LDA #1
	STA MSGFLG
	RTS

	mac RVS_RETURN
	BYTE 18
	TEXT "RETURN"
	BYTE 146
	endm

MSG1	BYTE 8,14,155,146
	TEXT "SPEEDSCRIPT 3.1"
	BYTE 0
MSG2	TEXT " BY CHARLES BRANNON"
	BYTE 0
KILLMSG	TEXT "BUFFER CLEARED"
	BYTE 0
BUFERR	TEXT "BUFFER FULL"
	BYTE 0
delmsg	TEXT "DELETE (S,W,P)"
	BYTE 0
YNMSG	TEXT ": ARE YOU SURE? (Y/N):"
	BYTE 0
CLRMSG	TEXT "ERASE ALL TEXT"
	BYTE 0
ERASMSG	TEXT "ERASE (S,W,P): "
	RVS_RETURN
	TEXT " TO EXIT"
	BYTE 0
FORMSG	TEXT "PRESS FORMAT KEY:"
	BYTE 0
SAVMSG  TEXT "SAVE:"
	BYTE 0
FNF	TEXT "TAPE ERROR"
	BYTE 0
BRMSG	TEXT "STOPPED"
	BYTE 0
VERERR	TEXT "VERIFY ERROR"
	BYTE 0
OKMSG	TEXT "NO ERRORS"
	BYTE 0
TDMSG	BYTE 147,32,18,212,146
	TEXT "APE OR "
	BYTE 18,196,146
	TEXT "ISK?"
	BYTE 0
loadmsg TEXT "LOAD:"
VERMSG	TEXT "VERIFY:"
DIRMSG	TEXT "PRESS "
	RVS_RETURN
	BYTE 0
DCMSG	TEXT "DISK COMMAND:"
	BYTE 0
DIRNAME	TEXT "$"
INSERR	TEXT "NO ROOM"
	BYTE 0
INSMSG	TEXT "NO TEXT IN BUFFER."
	BYTE 0
CHOOSEMSG
	BYTE 147
	TEXT "PRINT TO "
	BYTE 18,211,146
	text "CREEN,"
	BYTE 18,196,146
	text "ISK,"
	BYTE 18,208,146
	text "RINTER?"
	BYTE 0
devmsg  byte 196
	text "EVICE NUMBER?"
	BYTE 0
SADRMSG BYTE 211
	text "ECONDARY ADDRESS #?"
	BYTE 0
fnmsg	BYTE 208
	text "RINT TO FILENAME:"
	BYTE 0
prinmsg	BYTE 208
	text "RINTING..."
	byte 13,13,0
waitmsg	byte 201
	text "NSERT NEXT SHEET, PRESS "
	RVS_RETURN
	BYTE 0
SRCHMSG	BYTE 200
	TEXT "UNT FOR:"
	BYTE 0
nfmsg	TEXT "NOT FOUND"
	BYTE 0
repmsg	BYTE 210
	TEXT "EPLACE WITH:"
	BYTE 0
xitmsg  BYTE 197,216,201,212,32,211
	TEXT "PEED"
	BYTE 211
	TEXT "CRIPT"
	BYTE 0

TEXSTART
	*= *+2
TEXEND
	*= *+2
TEXBUF
	*= *+2
BUFEND
	*= *+2
LENTABLE
	*= *+1
TOPLIN
	*= *+2
MSGFLG
	*= *+1
INSMODE
	*= *+1
ENDPOS
	*= *+1
FINPOS
	*= *+1
LASTLINE
	*= *+2
LIMIT
	*= *+1
INLEN
	*= *+1
BOTSCR
	*= *+2
LBUFF
	*= *+40
INBUFF
	*= *+40
FILENAME
	*= *+24
FNLEN
	*= *+1
SAVCURR
	*= *+2
bcd
	*= *+2
HEX
	*= *+2
TPTR
	*= *+2
BUFLEN
	*= *+2
GORLEN
	*= *+2
FROMSAV
	*= *+2
DESTSAV
	*= *+2
HDLEN
	ORG *+1
FTLEN
	ORG *+1
LMARGIN
	ORG *+1
RMARGIN
	ORG *+1
PAGELENGTH
	ORG *+1
TOPMARG
	ORG *+1
BOTMARG
	ORG *+1
SPACING
	ORG *+1
CONTINUOUS
	ORG *+1
PAGENUM
	ORG *+2
STARTNUM
	ORG *+2
PAGEWIDTH
	ORG *+1
NOMARG
	ORG *+1
POS
	ORG *+1
LINE
	ORG *+1
YSAVE
	ORG *+1
SAVCHAR
	ORG *+1
INSLEN
	ORG *+1
DEVMO
	ORG *+1
NEEDASC
	ORG *+1
UNDERLINE
	ORG *+1
FPOS
	ORG *+2
pcr
	ORG *+1
huntlen
	ORG *+1
huntbuf
	ORG *+30
replen
	ORG *+1
repbuf
	ORG *+30
codebuffer
	org *+128
prbuff
	org *+256
hdbuff
	org *+256
firstrun
	org *+1
ftbuff
	org *+256
savcol
	org *+1
linefeed
	org *+1
blinkflag
	org *+1
end


