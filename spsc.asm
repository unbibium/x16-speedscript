; SpeedScript 3.1
; copied by hand out of the pdf at archive.org
;
	processor 6502

	org $0801

	; sys2061
	hex 0B080A009E32303631000000

	; some macros
	mac copy16
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
	jsr $BDCD ;BASIC number display
	ldy #54
	sty map
	endm


;Named constants not used in source.
;May be useful for future ports.
SCRMEM equ $0400
COLMEM equ $D800
COLUMNS equ 40
ROWS    equ 25
space   equ 32

;Locations used by high-speed memory 
;move routines: 

froml equ $26 
fromh equ $27 
destl equ $9E 
desth equ $9F 
llen = $B4 
hlen = $B5 

;curr: Position of cursor within text 
;memory. scr: used by the refresh 
;routine. 

curr = $39 
scr = $C3 

;tex: An alternate location used in tan- 
;dem with curr. COLR is used by RE- 
;FRESH. temp is used throughout as a 
;reusable scratchpad pointer. INDIR is 
;also a reusable indirect pointer. 
;UNDERCURS stores the value of the 
;character highlighted by the cursor. 

tex = $FB 
COLR = $14 
temp = $3B 
indir = $FD 
UNDERCURS = $02 

;WINDCOLR: Color of command line 
;window supported by refresh. MAP 
;is the 6510's built-in I/O port, used for 
;mapping in and out ROMs from the 
;address space. RETCHAR is the screen- 
;code value of the return mark (a left- 
;pointing arrow). 

windcolr = $0C 
map equ $01 
retchar equ 31 

;Kernal Routines 
;(refer to the Commodore 64 Programmer's Reference ;Guide): 

chrout = $FFD2 
stop = $FFE1 
setlfs = $FFBA 
setnam = $FFBD 
clall = $FFE7 
open = $FFC0 
chrin = $FFCF 
chkin = $FFC6 
chkout = $FFC9 
getin = $FFE4 
clrchn = $FFCC 
close = $FFC3 
load = $FFD5 
save = $FFD8 
ioinit = $FF84 

;Called only when run from BASIC. It is 
;assumed that the author's initials (that 
;conveniently work out in hex) are not 
;normally present in memory. If they 
;are, we know that SpeedScript has been 
;run before, so we avoid the ERASE 
;routine to preserve the text in memory. 

BEGIN JSR INIT 
	LDA #$CB 
	CMP firstrun 
	STA firstrun 
	BEQ SKIPERAS 
	JSR erase 
SKIPERAS JSR INIT2 
	JMP main 

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
;lete. Set froml/fromh to point to 
;the source area of memory, 
;destl/desth to point to the destina- 
;tion, and llen/hlen to hold the 
;length of the area being moved. 

umove LDA froml 
	STA MOVLOOP+1 
	LDA fromh 
	STA MOVLOOP+2 
	LDA destl 
	STA MOVLOOP+4 
	LDA desth 
	STA MOVLOOP+5 
	LDX hlen 
	BEQ SKIPMOV 
MOV1 	LDA #0 
MOV2 	STA endpos 
	LDY #0 
MOVLOOP LDA $0000,Y 
	STA $0000,Y 
	INY 
	CPY endpos 
	BNE MOVLOOP 
	INC MOVLOOP+2 
	INC MOVLOOP+5 
	CPX #0 
	BEQ OUT
	DEX
	BNE MOV1
SKIPMOV LDA llen
	BNE MOV2
OUT	RTS
;DMOVE uses the same variables as UMOVE, but is used to move an
;overlapping block of memory down ward, so it is used to insert. If the block
;of memory to be moved does not overlap the destination area, then either
;routine can be used.
dmove	LDA hlen
	TAX
	ORA llen
	BNE NOTNULL
	RTS
NOTNULL	CLC
	TXA
	ADC fromh
	STA DMOVLOOP+2
	LDA froml
	STA DMOVLOOP+1
	CLC
	TXA
	ADC desth
	STA DMOVLOOP+5
	LDA destl
	STA DMOVLOOP+4
	INX
	LDY llen
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
	BNE DMOV1 
	RTS 

;refresh copies a screenful of text 
;from the area of memory pointed to by 
;toplin. It works like a printer routine, 
;fitting a line of text between the screen 
;margins, wrapping words, and restarts 
;at the left margin after printing a car- 
;riage return. SpeedScript constantly calls 
;this routine while the cursor is blink- 
;ing, so it has to be very fast. To elimi- 
;nate flicker, it also clears out the end of 
;each line instead of first clearing the 
;screen. It stores the length of the first 
;screen line for the sake of the check 
;routine (which scrolls up by adding 
;that length to toplin), the last text 
;location referenced (so check can see 
;if the cursor has moved off the visible 
;screen). 


refresh LDA #COLUMNS

	STA scr 
	STA COLR 
	LDA #>SCRMEM
	STA scr+1
	LDA #>COLMEM
	STA COLR+1 
	LDA toplin 
	STA tex 
	LDA toplin+1
	STA tex+1
	LDX #1
	LDA INSMODE
	STA windcolr
	LDA scrcol
	STA 53280  ; VIC BORDER
PPAGE	LDY #0
PLINE	LDA TEXCOLR
	STA (COLR),Y
	LDA (tex),Y
	STA lbuff,Y
	INY
	AND #127
	CMP #retchar
	BEQ BREAK
	CPY #COLUMNS
	BNE PLINE
	DEY
SLOOP	LDA (tex),Y
	AND #127
NXCUR	CMP #32
	BEQ SBRK
	DEY
	BNE SLOOP
	LDY #39
SBRK	INY

BREAK	STY temp
	DEY
COPY	LDA lbuff,Y
	STA (scr),Y
	DEY
	BPL COPY
	LDY temp
	CLC
	TYA
	ADC tex
	STA tex
	LDA tex+1
	ADC #0
	STA tex+1
	CPX #1
	BNE CLRLN
	STY LENTABLE
CLRLN	CPY #COLUMNS
	BEQ CLEARED
	LDA #32
	STA (scr),Y
	INY
	JMP CLRLN
CLEARED	CLC
	LDA scr
	ADC #COLUMNS
	STA scr
	STA COLR
	BCC incnot
	INC scr+1
	INC COLR+1
incnot	INX
	CPX #ROWS
	BEQ pdone
	JMP PPAGE
pdone	LDA tex
	STA BOTSCR
	LDA tex+1
	STA BOTSCR+1
	RTS

; comment from left column of page 99

erase	LDA texstart
	STA tex
	STA toplin
	STA lastline
	STA curr
	LDA texstart+1
	STA tex+1
	STA toplin+1
	STA lastline+1
	STA curr+1
	SEC
	LDA texend+1
	SBC texstart+1
	TAX
	LDA #32
CLRLOOP	LDY #255
	DEC tex+1
	STA (tex),Y
	INY
	INC tex+1
CLR2	STA (tex),Y
	INY
	BNE CLR2
	INC tex+1
	DEX
	BNE CLR2
	STA (tex),Y
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

PRMSG	STA temp
	STY temp+1
	LDY #0
PRLOOP	LDA (temp),Y
	BEQ PREXIT
	JSR chrout
	INY
	BNE PRLOOP
PREXIT	RTS
getakey	JSR getin
	BEQ getakey
	RTS
;The initialization routine sets up the
;memory map, clears out certain flags, 
;and enables the raster interrupt

INIT	LDA #147
	JSR chrout
	LDA #54
	STA map
	LDA #0
	STA INSMODE
	STA texstart
	STA texend
	STA texbuf
	STA bufend
	STA huntlen
	STA replen
	LDA #>END ;
	CLC
	ADC #1
	STA texstart+1
	LDA #$CF
	STA texend+1
	LDA #$D0
	STA texbuf+1
	LDA #$FF
	STA bufend+1
	STA fpos+1
	JMP ioinit
INIT2	JSR killbuff
	LDA #128
	STA 650 ;TODO
	STA $9D ;TODO
	JSR highlight
	LDA #<mynmi
	sta $0318
	LDA #>mynmi
	sta $0319
	copy16 texstart,curr
	JSR sysmsg
	DO_PRMSG MSG2
	INC msgflg
	RTS

;The NOPS are here because I replaced
;a three-byte JSR check with RTS.
;I did not want the size of the code
;or the location of any routines to change.
;JSR check was originally inserted to fix
;a bug, but caused a bug itself.

	NOP
	NOP

;sysmsg displays "SpeedScript" and the version.
sysmsg	JSR topclr
	DO_PRMSG MSG1
	LDA #0
	STA msgflg
	RTS

; This routine traps the RESTORE key. It reproduces
; some of the ROM code so that RS-232 is still supported
; (although SpeedScript does not directly support
; RS-232 output).
mynmi	PHA
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
NOTRS	LDA blinkflag
	BEQ NOTCURSOR
	LDA UNDERCURS
	LDY #0
	STA (curr),Y
NOTCURSOR	LDA #2
	STA windcolr
	JSR clrchn
	JSR topclr
	DO_PRMSG xitmsg
	JSR YORN
	BNE REBOOT
	JSR delite
	SEI
	LDA #$7F
	JMP $FE66
REBOOT	JSR delite
	LDX #$FA
	TXS
	JSR INIT2
	JMP main
;
;topclr keeps the command line clean.
;It is called before most messages.
;It's like a one-line clear-screen.
topclr	LDX #COLUMNS-1
	LDA #32 ;space
toploop	STA SCRMEM,X
	DEX
	BPL toploop
	LDA #19 ;HOME
	JMP chrout
;Converts PETSCII to screen codes.
astoin	PHA
	AND #128
	LSR
	STA temp
	PLA
	AND #63
	ORA temp
	RTS
;The MAIN loop blinks the cursor,
;checks for keystrokes, converts
;them from ASCII to screen codes,
;puts them in text at the CURRent position, and increments
;the CURRent position and lastline. It also checks for special
;cases like the back-arrow and the return key and passes control
;characters to the CONTROL routine. SHIFTed spaces are turned into
;unSHIFTed ones. The INSMODE flag is checked to see if we should
;insert a space before a character.

main LDY #0
	STY blinkflag
	LDA (curr),Y
	STA UNDERCURS
main2	LDY #0
	LDA (curr),Y
	EOR #$80
	STA (curr),Y
	LDA blinkflag
	EOR #1
	STA blinkflag
	JSR refresh
WAIT	JSR getin
	BNE KEYPRESS
	LDA 162 ;TODO: label
	AND #16 
	BEQ WAIT
	LDA #0
	STA 162
	JMP main2
KEYPRESS	TAX
	LDY #0
	LDA UNDERCURS
	STA (curr),Y
	STY blinkflag
	CPX #95
	BNE NOTBKS
	JSR left
	LDA #32
	LDY #0
	STA (curr),Y
	JMP main
NOTBKS	LDA msgflg
	BEQ nomsg
	TXA
	PHA
	jsr sysmsg
	PLA
	TAX
nomsg	TXA
	CMP #13
	BNE notcr
	ldx #retchar+64
notcr	txa
	AND #127
	CMP #32
	BCC CONTROL
	CPX #160
	BNE NESHIFT
	LDX #space
NESHIFT	TXA
	PHA
	LDY #0
	LDA (curr),Y
	CMP #retchar
	BEQ DOINS
	LDA INSMODE
	BEQ NOTINST
DOINS	JSR inschar
NOTINST	PLA
	JSR astoin
PUTCHR	LDY #0
	STA (curr),Y
	JSR refresh
	SEC
	LDA curr
	SBC lastline
	STA temp
	LDA curr+1
	SBC lastline+1
	ORA temp
	BCC INKURR
	LDA curr
	ADC #0
	STA lastline
	LDA curr+1
	ADC #0
	STA lastline+1
INKURR	INC curr
	BNE NOINC2
	INC curr+1
NOINC2	JSR check
	JMP main
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
	JMP main
FOUND	DEX
	TXA
	ASL
	TAX
	LDA #>main-1
	PHA
	LDA #>main-1
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
VECT	WORD right-1,left-1,wleft-1,wright-1,BORDER-1,LETTERS-1
	WORD sleft-1,sright-1,DELCHAR-1,inschar-1,DELETE-1
	WORD HOME-1,instgl-1,CLEAR-1,paright-1,parleft-1
	WORD ERAS-1,TLOAD-1,TSAVE-1,verify-1
	WORD sleft-1,sright-1,catalog-1,insbuffer-1,switch-1
	WORD endtex-1,print-1,FORMAT-1,dcmnd-1
	WORD DELIN-1,alpha-1,killbuff-1,HUNT-1,FREEMEM-1,tab-1
	WORD lottaspaces-1,repstart-1,endpar-1,SANDR-1
;The check routine (yadda yadda page 102)
check	JSR check2
	SEC
	LDA curr
	SBC toplin
	LDA curr+1
	SBC toplin+1
	BCS OK1
	SEC
	LDA toplin
	SBC texstart
	STA temp
	LDA toplin+1
	SBC texstart+1
	ORA temp
	BEQ OK1
	LDA curr
	STA toplin
	LDA curr+1
	STA toplin+1
	JSR refresh
OK1	SEC
	LDA BOTSCR
	SBC curr
	STA tex
	LDA BOTSCR+1
	SBC curr+1
	STA tex+1
	ORA tex
	BEQ EQA
	BCS OK2
EQA	CLC
	LDA toplin
	ADC LENTABLE
	STA toplin
	LDA toplin+1
	ADC #0
	STA toplin+1
REF	JSR refresh
	JMP OK1
OK2	RTS

check2	SEC
	LDA lastline
	SBC texend
	STA temp
	LDA lastline+1
	SBC texend+1
	ORA temp
	BCC CK3
	LDA texend
	STA lastline
	LDA texend+1
	STA lastline+1
CK3	SEC
	LDA curr
	SBC texstart
	STA temp
	LDA curr+1
	SBC texstart+1
	ORA temp
	BCS INRANGE
	LDA texstart
	STA curr
	LDA texstart+1
	STA curr+1
	RTS
INRANGE	SEC
	LDA curr
	SBC lastline
	STA temp
	LDA curr+1
	SBC lastline+1
	ORA temp
	BCS OUTRANGE
	RTS
OUTRANGE
	LDA lastline
	STA curr
	LDA lastline+1
	STA curr+1
	RTS
; move cursor right.
right	INC curr
	BNE NOINCR
	INC curr+1
NOINCR	JMP check

; Cursor left.
left	LDA curr
	BNE NODEC
	DEC curr+1
NODEC	DEC curr
	JMP check
; Word left. We look backward for a space.
wleft
	copy16 curr,tex
	DEC tex+1
	LDY #$FF
STRIP	LDA (tex),Y
	CMP #space
	BEQ STRLOOP
	CMP #retchar
	BNE WLOOP
STRLOOP	DEY
	BNE STRIP 
WLOOP	LDA (tex),Y
	CMP #space
	BEQ wrout
	CMP #retchar
	BEQ wrout
	DEY
	BNE WLOOP
	RTS
wrout
	SEC
	TYA
	ADC tex
	STA curr
	LDA tex+1
	ADC #0
	STA curr+1
	JMP check
;Word right. We scan forward
;for a space. OIDS is not a meaningful label.
wright	LDY #0
RLOOP	LDA (curr),Y
	CMP #space
	BEQ ROUT
	CMP #retchar
	BEQ ROUT 
	INY
	BNE RLOOP
	RTS
ROUT	INY
	BNE OIDS
	INC curr+1
	LDA curr+1
	CMP lastline+1
	BCC OIDS
	BNE lastword
OIDS	LDA (curr),Y
	CMP #space
	BEQ ROUT
	CMP #retchar
	BEQ ROUT
; add Y to curr to move the cursor
; check prevents illegal cursor movement.
; LASTWORD is called if the end
; of the word cannot be found within 255 characters.
adycurr	CLC
	TYA
	ADC curr
	STA curr
	LDA curr+1
	ADC #0
	STA curr+1
wrtn 	JMP check

lastword
	copy16 lastline,curr
	jmp check

;endtex is tricky,  (p103)

endtex	LDA #0
	STA toplin
	LDA lastline+1
	SEC
	SBC #4
	CMP texstart+1
	BCS SAFE
	LDA texstart+1
SAFE	STA toplin+1
	JSR refresh
	JMP lastword

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
BORDER	INC15 scrcol
	RTS
scrcol	BYTE 12 ; gray
;TEXCOLR (text color) is used in the refresh routine
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
	
sleft
	copy16 curr,tex
	DEC tex+1
	LDY #$FF
PMANY	LDA (tex),Y
	B_IF_PUNCT PSRCH
	CMP #retchar
	BNE psloop
PSRCH	DEY
	BNE PMANY
	RTS
psloop	LDA (tex),Y
	B_IF_PUNCT PUNCT
	CMP #retchar
	BEQ PUNCT
	DEY
	BNE psloop
	DEC tex+1
	LDA tex+1
	CMP texstart
	BCS psloop
	JMP firstword
PUNCT	STY temp
	DEC temp
SKIPSPC	INY
	BEQ REPEAT
	LDA (tex),y
	CMP #space
	BEQ SKIPSPC
	dey
	jmp wrout
REPEAT	ldy temp
	jmp psloop
firstword	copy16 texstart,curr
	jmp check
; Sentence right. We look for ending punctuation,
; then skip forward until we run out of spaces.
sright	LDY #0
srlp	LDA (curr),Y
	B_IF_PUNCT punct2
	CMP #retchar
	BEQ punct2
	INY
	BNE srlp
	inc curr+1
	lda curr+1
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
killbuff	copy16 texbuf,tptr
	JSR topclr
	DO_PRMSG killmsg
	LDA #1
	STA msgflg
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

del1	SBC_ORATEMP curr,texstart
	BNE DEL1A
DELABORT	PLA
	PLA
	RTS
DEL1A	copy16 curr,froml
	RTS
del2	SEC
	LDA curr
	STA destl
	eor #$ff
	adc froml
	sta goblen
	lda curr+1
	sta desth
	eor #$ff
	adc fromh
	sta goblen+1
delc	copy16 froml,fromsav
	lda destl
	sta destsav
	sta froml
	lda desth
	sta destsav+1
	sta fromh
	sec
	lda goblen+1
	adc tptr+1
	cmp bufend+1
	bcc gosav
	top_prmsg buferr
	lda #1
	sta msgflg
	lda #0
	sta 198 ; TODO label
	rts

gosav	copy16 tptr,destl
	lda goblen
	sta llen
	clc 
	adc tptr
	sta tptr
	lda goblen+1
	sta hlen
	adc tptr+1
	sta tptr+1
	lda #0
	sta $D01A ;TODO: label
	lda #52
	sta map
	jsr umove
	lda #54
	sta map
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
	JSR right
	JSR del1
	JSR left
	JSR del2
	JMP fixtp

;Called by CTRL-D.  (etc)
DELETE	JSR killbuff
	LDA #2
	STA windcolr
	JSR topclr
	DO_PRMSG delmsg
	JSR getakey
	PHA
	JSR sysmsg
	PLA
	AND #191
	CMP #23 ; "W"
	BNE NOTWORD
DELWORD	JSR del1
	JSR wleft
	JMP del2
NOTWORD	CMP #19 ; "S"
	BNE NOTSENT
DELSENT	JSR del1
	JSR sleft
	JMP del2
NOTSENT	CMP #16 ; "P"
	BNE NOTPAR
	JSR del1
	JSR parleft
	JMP del2
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
	COPY163 curr,tex,destl
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
lottaspaces
	LDA #255
	STA inslen
	JMP tab2
	
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
endpar	JSR inschar
	JSR inschar
	LDA #retchar
	LDY #0
	STA (curr),Y
	INY
	STA (curr),Y
	JSR refresh
	JSR right
	JSR right
	JMP tab
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
;spaces as are specified by inslen.
insblock
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
inout	rts
;toggle insert mode. The INSMODE
;flag doubles as the color of the
;command line.
instgl	LDA INSMODE
	EOR #14     ; TODO: constant
	STA INSMODE
	RTS
;Another example of modular code.
YORN	DO_PRMSG ynmsg
YORNKEY JSR $FF9F  ; TODO: constant
	JSR getin
	BEQ YORNKEY
	CMP #147  ;user is spamming CLR/HOME
	BEQ YORNKEY ;ignore it
	AND #127
	CMP #'Y
	RTS
;Erase all text. (p108)
CLEAR	LDA #2  ;red!
	STA windcolr 
	JSR topclr
	DO_PRMSG clrmsg
	JSR YORN
	BEQ DOIT
	JMP sysmsg
DOIT LDX #$FA
	TXS
	JSR erase
	JSR INIT2
	JMP main
;Paragraph right.
paright LDY #0
parlp 	lda (curr),Y
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
parleft	copy16 curr,tex
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
textocurr
	copy16 tex,curr
	jmp check
;tis enables the raster interrupt. (p109)
highlight
	SEI
	LDA #0
	STA $DC0E
	LDA #27
	STA $D011
	copy16 IRQ,$0314
	LDA #1
	STA $D01A
	STA $D012
	CLI
	RTS
IRQ	LDA #58
	LDY windcolr
	CMP $D012
	BNE MID
	LDA #1
	LDY scrcol
MID	STY $D021
	STA $D012
SKIP	CMP #1
	BEQ defalt
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
	JSR killbuff
ERAS1	JSR topclr
	DO_PRMSG erasmsg
erasagain
	LDA #0
	LDA (curr),Y
	EOR #$80
	STA (curr),Y
	JSR refresh
	LDY #0
	LDA (curr),Y
	EOR #$80
	STA (curr),Y
	lda #2
	sta windcolr
	jsr getakey
	ora #64
	cmp #'W
	bne noword
erasword jsr era1
	jsr wright
	jmp era2
noword	cmp #'S
	bne unsent
erasent jsr era1
	jsr sright
	jmp era2
unsent	cmp #'P
	bne nopar
	jsr era1
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
	copy16 savcurr,curr
	jsr refresh
	jmp erasagain
;the INPUT routine is used to get responses
;from the command line.
input	LDA #39
	SBC 211
	STA limit
inp1	LDY #0
cursin	LDA #153
	JSR chrout
	LDA #18 ; Ctrl-R
	JSR chrout
	LDA #space
	JSR chrout
	LDA #157
	JSR chrout
	sty inlen
	jsr getakey
	ldy inlen
	sta temp
	lda #146
	jsr chrout 
	LDA #space
	JSR chrout
	LDA #157
	JSR chrout
	LDA #155
	JSR chrout
	LDA temp
	CMP #13
	BEQ inexit
	CMP #20 ;petscii DEL key
	BNE noback
	DEY
	BPL notzero
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
	BNE error
	JMP fine
;The ERROR message routine. May this
;routine never be called when you use
;SpeedScript, but that's too much to 
;ask for.
error	BEQ STOPPED
	LDA dvn
	CMP #8
	BCC TAPERR
	JSR readerr
	JMP erxit
TAPERR	LDA dvn
	CMP #1
	BEQ TAPERR
	JSR topclr
	DO_PRMSG FNF
erxit	JSR highlight
	LDA #1
	STA msgflg
	RTS
STOPPED	JSR topclr
	DO_PRMSG brmsg
	JMP erxit
dvn	BYTE 0

;TOPEN
topen	JSR input
	BEQ OPABORT
OP2	DO_PRMSG tdmsg
	JSR getakey
	LDX #8
	CMP #'D
	BEQ OPCONT
	LDX #1
	CMP #'T
	BEQ OPCONT
OPABORT	JSR sysmsg
	PLA
	PLA
	RTS
OPCONT	STX dvn
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
copy1	lda inbuff,y
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
	jsr setnam
	lda #13 ;cr
	jsr chrout
	jmp delite
; called by CTRL-\ to enter a format code.
; It checks insert mode and inserts if necessary.
FORMAT	JSR topclr
	DO_PRMSG formsg
	JSR getakey
	JSR astoin
	ORA #$80
	PHA
	LDA INSMODE
	BEQ NOINS
	JSR inschar
NOINS	JSR sysmsg
	PLA
	JMP PUTCHR
;p112
TLOAD
	SBC_ORATEMP curr,texstart
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
;delite turns off the raster interrupt.
delite	SEI
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
dechex	lda bcd
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

deftab BYTE 5,75,66,5,58,1,1,1,0,1,0,80
	
prcodes BYTE 27,14,15,18

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
	cmp #8
	bcs overques
notd2	top_prmsg sadrmsg
	jsr getakey
	sec
	sbc #'0
	tay
	bpl overques
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
	bne srchsp
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
	;text "WALRTBSNHF@P?XMIGJ" 
	hex 57414c525442534e484640503f584d49474a

spvect	word pw-1,as-1,lm-1,rm-1,tp-1
	word bt-1,sp-1,nx-1,hd-1,ft-1
	word pn-1,pl-1,spage-1,across-1
	word mrelease-1,comment-1,link-1
	word lfset-1

;m Margin release.
; INY is used to skip over the format character.
mrelease
	INY
	LDA #0
	STA nomarg
	JMP spcexit
;x Columns across
across
	INY
	JSR aschex
	STA pagewidth
	JMP spcexit

	mac prcode
	JSR aschex
	STA {1}
	endm
	mac prcode16
	prcode {1}
	LDA hex+1
	STA {1}
	endm
;? Print starting at specified page
spage
	INY
	prcode16 startnum
	JMP spcexit
;@ set starting page default number
pn
	INY
	prcode16 pagenum
	JMP spcexit

;p page length
pl	INY
	PRCODE pagelength
	JMP spcexit 

;w set page wait mode
pw	LDA #0
	STA continuous
	INY
	JMP spcexit

lfset	LDA #10
	STA linefeed
	INY
	JMP spcexit
;a set true ASCII mode
as	INY
	LDA #1
	STA needasc
	JMP spcexit
lm	iny
	prcode lmargin
	jmp spcexit
rm	iny
	prcode rmargin
	jmp spcexit
tp	iny
	prcode topmarg
	jmp spcexit
bt	iny
	prcode botmarg
	jmp spcexit
sp	iny
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
hd	JSR PASTRET
	DEY
	STY hdlen
	LDY #1
hdcopy	LDA (tex),Y
	STA hdbuff-1,Y
	INY
	CPY hdlen
	BCC hdcopy
	BEQ hdcopy
	INY
	JMP spcexit
;Skip just past the return mark
PASTRET	INY
	LDA (tex),Y
	CMP #retchar
	BNE PASTRET
	RTS
;f define header
ft	JSR PASTRET
	DEY
	STY ftlen
	LDY #1
FTCOPY	LDA (tex),Y
	STA ftbuff-1,Y
	INY
	CPY ftlen
	BCC FTCOPY
	BEQ FTCOPY
	INY
	JMP spcexit

;i ignore a line 
comment	JSR PASTRET
	JMP spcexit

; Define programmable printeys?

define	INY
	LDA (tex),Y
	CMP #'=
	BEQ DODEFINE
	DEY
	LDA savchar
	JMP notret
DODEFINE
	INY
	JSR aschex
	PHA
	LDA savchar
	AND #127
	TAX
	PLA
	STA codebuffer,X
	JSR spcexit
	JMP spcont
;Link to next file
link	INY
	LDX #8
	LDA (tex),Y
	AND #63
	CMP #'D
	BEQ link2
	ldx #1
	CMP #'T
	BEQ link2
	jmp pbort
link2	stx dvn
	iny
	lda (tex),y
	cmp #':
	beq linkloop
	jmp pbort
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
	ldx #<filename
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
;until Hunt returns "Not Found." (fpos+1
;is $FF after a search failure.)
SANDR	JSR reset
	LDA huntlen
	BEQ NOSR
	JSR askrep
SNR	JSR CONTSRCH
	LDA fpos+1
	CMP #$FF
	BEQ NOSR
	JSR repl
	JSR refresh
	JMP SNR
NOSR	JMP sysmsg
;if SHIFT is held down, we ask for and store
;the hunt phrase. If SHIFT is not down, we
;perform the actual hunt. The line in the inbuff is compared with
;characters in text. (p121)
HUNT	LDA 653
	CMP #5
	BNE CONTSRCH
reset	top_prmsg srchmsg
	jsr input
	sta huntlen
	bne oksrch
	jmp sysmsg
oksrch	LDY #0
tobuff	LDA inbuff,Y
	STA huntbuff,Y
	INY
	CPY inlen
	BNE tobuff
	JMP sysmsg
CONTSRCH	copy16 curr,tex
	LDA #$FF
	STA fpos+1
	LDY #1
	LDX #0
	LDA huntlen
	BEQ notfound
SRCH1	LDA huntbuff,X
	JSR astoin
	CMP (tex),Y
	BEQ CY
	LDX #$FF
CY	INY
	BNE NOVFL
	BCS notfound
NOVFL	INX
	CPX huntlen
	BNE SRCH1
	CLC
	TYA
	ADC tex
	STA temp
	LDA tex+1
	ADC #0
	STA temp+1
	LDA lastline
	cmp temp
	LDA lastline+1
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
	sub163 lastline,destl, llen
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
nobrk	INY
	jmp buflp
endbuff rts
;stage 2 format commands
spec2	sty ysave
	and #127
	sta savchar
	jsr intoas

other	cmp #'C
	bne notcenter
	sec
	lda pagewidth
	sbc endpos
	lsr
	sec
	sbc lmargin
	tay
	lda #space
cloop	jsr pchrout
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
docodes	LDX savchar
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
	JSR topclr
	SEC
	LDA texend
	SBC lastline
	TAX
	LDA texend+1
	SBC lastline+1
	DISPLAY_NUMBER
	LDA #1
	STA msgflg
	RTS

	mac RVS_RETURN
	hex 12 D2 C5 D4 D5 D2 CE 92
	endm
	mac SWP
	hex 28D52CD72CD029 
	endm

MSG1	BYTE 8,14,155,146
	; SpeedScript 3.2
	hex D3 50 45 45 44 D3 43 52 49 50 54 20 33 2e 32 0a
	BYTE 0
MSG2	; by Charles Brannon
	hex 20 42 59 20 C3 48 41 52 4C 45 53 20 C2 52 41 4E
	hex 4E 4F 4E
	BYTE 0
killmsg	;TEXT "BUFFER CLEARED"
	hex C25546464552 20 43484541524544
	BYTE 0
buferr	;TEXT "BUFFER FULL"
	hex C25546464552 20 46554C4C
	BYTE 0
delmsg	;TEXT "DELETE (S,W,P)"
	hex C4454C45544520
	SWP
	BYTE 0
ynmsg	;TEXT ": ARE YOU SURE? (Y/N):"
	hex 3a20c1524520594f5520535552452038d92fce393a00
	BYTE 0
clrmsg	;TEXT "ERASE ALL TEXT"
	hex C5D2C1D3C520C1CCCC20D4C5D8D400
erasmsg	;TEXT "ERASE (S,W,P): "
	hex C55241534520
	SWP
	hex 3A20
	RVS_RETURN
	;TEXT " TO EXIT"
	hex 20544F2045584954
	BYTE 0
formsg	;TEXT "PRESS FORMAT KEY:"
	hex d05245535320464f524d4154204b45593a00
savmsg  ;TEXT "SAVE:"
	hex D34156453A00
FNF	;TEXT "TAPE ERROR"
	hex D441504520C5D2D2CFD200
brmsg	;TEXT "STOPPED"
	hex D3545050454400
vererr	;TEXT "VERIFY ERROR"
	hex D64552494659204552524F5200
okmsg	;TEXT "NO ERRORS"
	hex ce4f204552524f525300
tdmsg	BYTE 147,32,18,212,146
	;TEXT "APE OR "
	hex 415045204f5220
	BYTE 18,196,146
	;TEXT "ISK?"
	hex 49534b3f
	BYTE 0
loadmsg ;TEXT "load:"
	hex cc4f41443a00
vermsg	;TEXT "VERIFY:"
	hex d645524946593a00
dirmsg	;TEXT "PRESS "
	hex d05245535320
	RVS_RETURN
	BYTE 0
dcmsg	;TEXT "DISK COMMAND:"
	BYTE $80+'D,'I,'S,'K,' ,'C,'O,'M,'M,'A,'N,'D
	BYTE 0
dirname	BYTE '#
inserr	;TEXT "NO ROOM"
	hex CE4F20D24F4F4D00
insmsg	;TEXT "NO TEXT IN BUFFER."
	hex ce4f205445585420494e204255464645522d00
choosemsg
	BYTE 147
	;TEXT "PRINT TO "
	hex d052494e5420544f20
	BYTE 18,211,146
	;text "CREEN,"
	hex 435245454e4c
	BYTE 18,196,146
	;text "ISK,"
	hex 49534b2c
	BYTE 18,208,146
	;text "RINTER?"
	hex 52494e5445523f00
devmsg  ;text "DEVICE NUMBER?"
	hex c44556494345204e554d4245523f00
sadrmsg ;text "ECONDARY ADDRESS #?"
	hex d545434f4e44415259204144445245535320233f00
fnmsg	;text "PRINT TO FILENAME:"
	hex d052494e5420544f2046494c454e414d453a00
prinmsg	;text "PRINTING..."
	hex d052494e54494e472d2d2d
	hex 0d0d00
waitmsg	;text "Insert next sheet, press "
	hex c94e53455254204e4558542053484545542c20
	hex 505245535320
	RVS_RETURN
	BYTE 0
srchmsg	;TEXT "Hunt for:"
	hex c8554e5420464f523a00
nfmsg	;TEXT "NOT FOUND"
	hex ce4f5420464f554e4400
repmsg	hex C2 45 50 4C 41 43 45 20 57 49 54 48 3A 00
xitmsg  
	;EXIT SpeedScript
	hex C5 D8 C9 D4 20
	hex D3 50 45 45 44 D3 43 52 49 50 54 00

texstart
	ORG *+2
texend
	ORG *+2
texbuf
	ORG *+2
bufend
	ORG *+2
LENTABLE
	ORG *+1
toplin
	ORG *+2
msgflg
	ORG *+1
INSMODE
	ORG *+1
endpos
	ORG *+1
finpos
	ORG *+1
lastline
	ORG *+2
limit
	ORG *+1
inlen
	ORG *+1
BOTSCR
	ORG *+2
lbuff
	ORG *+40
inbuff
	ORG *+40
filename
	ORG *+24
fnlen
	ORG *+1
savcurr
	ORG *+2
bcd
	ORG *+2
hex
	ORG *+2
tptr
	ORG *+2
buflen
	ORG *+2
goblen
	ORG *+2
fromsav
	ORG *+2
destsav
	ORG *+2
hdlen
	ORG *+1
ftlen
	ORG *+1
lmargin
	ORG *+1
rmargin
	ORG *+1
pagelength
	ORG *+1
topmarg
	ORG *+1
botmarg
	ORG *+1
spacing
	ORG *+1
continuous
	ORG *+1
pagenum
	ORG *+2
startnum
	ORG *+2
pagewidth
	ORG *+1
nomarg
	ORG *+1
pos
	ORG *+1
line
	ORG *+1
ysave
	ORG *+1
savchar
	ORG *+1
inslen
	ORG *+1
devno
	ORG *+1
needasc
	ORG *+1
underline
	ORG *+1
fpos
	ORG *+2
pcr
	ORG *+1
huntlen
	ORG *+1
huntbuff
	ORG *+30
replen
	ORG *+1
repbuff
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
END


