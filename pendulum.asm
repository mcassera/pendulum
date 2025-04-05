; Pendulum
; 2025 Michael Cassera
; Inspired by some Youtube videos of similar style.
; using the SAM dream midi chip for sounds

.cpu "w65c02"				                ; set the cpu to Western Digital 65C02
.include "setup.asm"		                ; all of our initial settings

*=$a0										; Set up buffer for Kernel communication
.dsection zp						        ; Define position for zp (zero page)
.cerror * > $af, "Too many Zero page variables"

*=$1ffd

start:										; ***TEMP CODE FOR PROGRAMMING***
		jmp SC								; Start of the program - We put this jump here so you can load the PGZ into the computer.
											; With the self running code below, you can boot into it in RAM. Without this jump, loading a PGZ will
											; hit the self running code the kernel needs at the start of the slot at $2000 and look like a freeze.
											; hitting the reset on the back of the F256k will start the program.



.include "api.asm"							; This is the Kernel API for communication

;SetupKernel:								; Set up the API to work with

.section zp									; Zero page section $a0 to $a8
event:	.dstruct	kernel.event.event_t
.send


; ************************************************************************************************************************************
*=$2000										; ***TEMP CODE FOR PROGRAMMING***
											; Self running code to send via USB port the F256. Allows me to quickly load into emulator
											; dipswitch 1 should be set to on.
		.byte $f2,$56						; Required bytes for the Kernel to identify
		.byte $04,$01						; how big is the program in 8K sections, What slot to map to
		.byte $0b,$20						; the starting address of your program
		.byte $00,$00,$00,$00				; reserved
		.byte $00							; terminating byte
; ************************************************************************************************************************************
											; *****My program starts here!*****
SC:


		stz MMU_IO_CTRL						; should do this on every program



; ************************************************************************************************************************************

;init_events:
        lda #<event
        sta kernel.args.events
        lda #>event
        sta kernel.args.events+1

; ************************************************************************************************************************************
;Set up TinyVicky to display sprites
		lda #%00100111						; Graphic, Sprites Engine enabled  			|xx|GM|SP|TL|BM|GR|OV|TX|
		sta VKY_MSTR_CTRL_0					; Text overlay enabled						| 0| 0| 1| 0| 0| 1| 1| 1|

		lda #%00000110						; Text mode options for the overlay 		|xx|xx|FS|FO|MS|2Y|2X|70|
		sta VKY_MSTR_CTRL_1					; 320 x 240, 60 Hz, dbl X & Y				| 0| 0| 0| 0| 0| 1| 1| 0|
		stz VKY_BRDR_CTRL					; No Border

		lda #$00							; Set the background color
		sta VKY_BKG_COL_R
		lda #$00
		sta VKY_BKG_COL_G
		lda #$00
		sta VKY_BKG_COL_B

		jsr clrScreen
		;jsr printMenu

; ************************************************************************************************************************************
; enable 24 sprites
		lda #<sprite1						; location of the first sprite
		sta spriteLoc
		lda #>sprite1
		sta spriteLoc+1
		ldx #$00							; set x to zero to start
spriteLoop:
		txa									; transfer to A and multiply by 8
		asl 
		asl 
		asl
		tay									; transfer result to Y, sprite control data is every 8 bytes
		lda #%01100001						; 8x8 sprite, layer 0, lut 0, enable on
		sta VKY_SP0,y 						; from sprite 0 in Vicky indexed to y
		lda ballXLO,x 
		sta VKY_SP0+SP_POS_X_L,Y
		lda #$00
		sta VKY_SP0+SP_POS_X_H,y
		lda ballYLO,X
		sta VKY_SP0+SP_POS_Y_L,Y
		lda ballYHI,X
		sta VKY_SP0+SP_POS_Y_H,Y
		lda spriteLoc
		sta VKY_SP0+SP_AD_L,y 
		lda spriteLoc+1
		sta VKY_SP0+SP_AD_M,y 
		lda #$00
		sta VKY_SP0+SP_AD_H,y
		clc
		lda spriteLoc						; each ball has its own sprite. 8x8 sprites are 64 bytes
		adc #64											
		sta spriteLoc
		lda spriteLoc+1
		adc #$00
		sta spriteLoc+1 
		inx 
		cpx #24
		bne spriteLoop
; ************************************************************************************************************************************
;Load the CLUT into memory
		lda #$01							; Change I/O control to page 1
		sta MMU_IO_CTRL
		lda #<CLUT0							; Set source pointer to CLUT for color information
		sta ptr_src
		lda #>CLUT0
		sta ptr_src+1

		lda #<VKY_GR_CLUT_0					; Set destination pointer to Graphics CLUT 0
		sta ptr_dst
		lda #>VKY_GR_CLUT_0
		sta ptr_dst+1

		ldx #$00							; Y is the number of colors to copy, check for 32
		ldy #32
; ************************************************************************************************************************************
makeClut:
		sty totalColors
color_loop:
		ldy #$00							; Y points to the color component (Blue Red Green Alpha)
comp_loop:
		lda (ptr_src),y						; Read byte from our color table 
		sta (ptr_dst),y						; write byte to the Graphic CLUT
		iny
		cpy #$04							; Do 4 bytes for one color + Alpha
		bne comp_loop

		inx
		cpx totalColors						; Loop for all colors of the CLUT
		beq done_lut

		clc									; Move the source pointer to the next Color
		lda ptr_src
		adc #$04
		sta ptr_src
		lda ptr_src+1
		adc #$00
		sta ptr_src+1

		clc									; Move the destination pointer to the next Color
		lda ptr_dst
		adc #$04
		sta ptr_dst
		lda ptr_dst+1
		adc #$00
		sta ptr_dst+1

		jmp color_loop						; and start copying the next color
done_lut:
		stz MMU_IO_CTRL
; ************************************************************************************************************************************
setFont:									; let's change the font
		lda #<font
		sta $80
		lda #>font
		sta $81
		lda #$c1
		stz $82
		sta $83
		ldy #$00
		ldx #$03
		lda #$01
		sta MMU_IO_CTRL
_sfLoop:
		lda ($80),y
		sta ($82),y 
		iny
		bne _sfLoop
		inc $81
		inc $83
		dex
		bne _sfLoop
		stz MMU_IO_CTRL


; ************************************************************************************************************************************
;set initial ball speed, ratio, and midi instrument
		jsr setBalls
		jsr setMidiInstrument
		
; ************************************************************************************************************************************
; set timer for SOF

		lda #kernel.args.timer.FRAMES		; set the Timer to Frames
		ora #kernel.args.timer.QUERY		; and query what frame we're on
		sta kernel.args.timer.units			; store in units parameter
		jsr kernel.Clock.SetTimer			; jsr to Kernel Routine
		bcs skipSet							; If Carry set, ignore
		adc #$01							; if not add 1 to Accumulator for next frame
		sta $d0
skipSet:
		jsr SetTimer						; Let's get the kernel set up for the timer

; ************************************************************************************************************************************		
; demo starts doing stuff here

loop:
		
		jsr handle_events					; This is my game loop
		jmp loop

handle_events:
		lda kernel.args.events.pending		; Peek at the queue to see if anything is pending
		bpl done_handle_events				; Nothing to do
		jsr kernel.NextEvent				; Get the next event.
		bcs done_handle_events				; If Carry is set, skip the handler
		jsr dispatch						; Handle the event
		jmp handle_events					; go and check for another event
done_handle_events:
		rts 								

dispatch:
		lda event.type						; get the event type from Kernel
		cmp #kernel.event.timer.EXPIRED		; is the event timer.EXPIRED?
		beq UpdateScreenJmp					; run the screen update
		cmp #kernel.event.key.PRESSED		                     
        beq keypress	
		rts

UpdateScreenJmp
		jmp UpdateScreen					; jmp because conditional is tto far

keypress:
		lda menuFlag
		beq checkTab
        lda event.key.flags               	; Once a key is pressed, we can get the ascii value by loading the byte from the
        lda event.key.ascii                 ; event.key.ascii location assigned by the kernel. We then check to see if it's a
		cmp #16								; These are the ascii characters for the menu input!
		beq moreGap
		cmp #14
		beq lessGapJmp						; TAB does not work here. These do not work when the menu is off.
		cmp #6
		beq moreSpeed
		cmp #2
		beq lessSpeed
		cmp #105
		beq nextInst
		cmp #73
		beq lastInst
		cmp #13
		beq restart
keyDone:
		rts

lessGapJmp
		jmp lessGap							; jmp because conditional is too far away

nextInst:									; This routine selects the next midi insturment
		clc
		lda midiInst
		adc #$01
		cmp #52
		bcs nInstDone
		sta midiInst
nInstDone:	
		jsr printMidiInst
		jsr setMidiInstrument
		rts

lastInst:									; This routine select the previous instrument
		sec
		lda midiInst
		sbc #$01
		bmi lInstDone
		sta midiInst
lInstDone:	
		jsr printMidiInst
		jsr setMidiInstrument
		rts

restart:									; this is when you hit return. It clears the menu
		jsr clrScreen						; and restarts the balls from 0
		jsr resetBalls
		jsr setBalls
		stz menuFlag
		rts

checkTab:									; This looks for the TAB when the program is running to bring up the menu
		lda event.key.flags               	; Once a key is pressed, we can get the ascii value by loading the byte from the
		lda event.key.ascii                 ; event.key.ascii location assigned by the kernel. We then check to see if it's a
		cmp #9 
		beq runMenu
		rts

moreGap:									; This increases the gap space between balls, it is a multiple of 4
		clc									; This does not take effect until RETURN is hit
		lda speedRatio
		adc #$04
		cmp #$44
		bcs topGap
		sta speedRatio
topGap:
		jsr printGap
		rts

moreSpeed:									; This increases the speed of the slowest ball. The other balls	
		clc									; will adjust with the gap setting once RETURN is hit
		lda speedBase
		adc #$08
		sta speedBase
		lda speedBase+1
		adc #$00
		sta speedbase+1
		cmp #$03
		bne mSDone
		stz speedBase
mSDone:
		jsr printSpeed
		rts

lessSpeed:									; This decreases the speed of the slowest ball. The other balls
		sec									; will adjust once the RETURN is hit.
		lda speedBase
		sbc #$08
		sta speedbase
		lda speedbase+1
		sbc #$00
		sta speedbase+1
		;lda speedbase+1
		bne lSpeedDone
checkSpeedLO:
		lda speedBase
		bne lSpeedDone
		lda #$08
		sta speedBase
lSpeedDone:
		jsr printSpeed
		rts	

lessGap:									; This decreases the gap space between the balls. The change does not
		sec									; take affect until RETURN is hit.
		lda speedRatio
		sbc #$04
		cmp #$04
		bcc bottomGap
		sta speedRatio
bottomGap:
		jsr printGap
		rts


runMenu:
		jsr printMenu
		inc menuFlag
		rts


; ************************************************************************************************************************************		

UpdateScreen:
		jsr SetTimer						; reset timer for next SOF
		jsr moveBalls						; move the balls
noMove:
		rts

moveBalls:
		ldx #$00							; set X to zero to loop 24 balls
mbLoop:
		txa									; transfer x to A
		asl									; and multiply by 8
		asl 								;		sprite control is 8 bytes large
		asl									;       this sets y at the start of each sprite
		tay									; 		and store in y
		lda ballDir,x 						; Get the ball direction 1 for right FF for left
		cmp #$ff							; check if we're going left
		beq goBack							; if yest, go to left routine
goForward:
		clc									; go right routine
		lda ballXFR,x						; we're going to add the speed to the X paramter
		adc ballSdF,x 						; using a single byte fixed point fraction
		sta ballXFR,x 
		lda ballXLO,x 
		adc ballSdL,x
		sta ballXLO,x 
		lda ballXHI,x 
		adc #$00
		sta ballxHI,x 
		lda ballXLO,x 						; see if we hit the right side of the screen
		cmp #$59		
		bcc placeBall						; no, let's set the sprite on the screen
		lda ballxHI,x 						; need to check HI byte as well
		beq placeBall						; no, place sprite on screen
		lda #$ff							; yes, change the direction flag to $ff
		sta ballDir,x 
		jsr reverseBack			
		jsr makeMusic						; hit the note on the SAM chip
		jsr makeColor						; fill the ball with the correct color
		bra placeBall						; and drop down to the opposite direction
goBack:
		sec 								; go left direction
		lda ballXFR,x						; subtract the ball speed from the x potition
		sbc ballSdF,x 
		sta ballXFR,x 
		lda ballXLO,x 
		sbc ballSdL,x
		sta ballXLO,x 
		lda ballXHI,x 
		sbc #$00
		sta ballxHI,x 
		lda ballXLO,x 						; check if we hit the left side of the screen
		cmp #$1f					
		bcs placeBall						; no, place the sprite
		lda ballxHI,x 						; we need to check the hi byte too
		bne placeBall						; no, place the sprite
		lda #$01							; change the ball direction to right
		sta ballDir,x 
		jsr reverseForward
		jsr makeMusic						; hit the note on the SAM chip
		jsr makeColor						; fill the ball with the correct color
placeBall:
		lda ballXLO,X						; set the sprite position parameter
		sta VKY_SP0+SP_POS_X_L,Y			
		lda ballxHI,X
		sta VKY_SP0+SP_POS_X_H,y 
		jsr dropColor						; reduce the color intesity as it travels away from the edge
		jsr assignColor						; and assign the color to the CLUT
		inx									; go to next ball in sequence	
		cpx #24								; have we gone through all 24 balls
		beq doneBallLoop					; yes skip the jmp command to top
		jmp mbLoop
doneBallLoop:
		rts			

reverseBack:
		sec
		lda ballXFR,x 
		sbc #$00
		sta resultFR
		lda ballXLO,x 
		sbc #$59
		sta resultLO
		sec
		lda #$00
		sbc resultFR
		sta ballXFR,x 
		lda #$59
		sbc resultLO
		sta ballXLO,x 
		rts

reverseForward:
		sec
		lda #$00
		sbc ballXFR,x 
		sta resultFR
		lda #$1f
		sbc ballXLO,x 
		sta resultLO
		clc
		lda #$00
		adc resultFR
		sta ballXFR,x 
		lda #$1f
		adc resultLO
		sta ballXLO,x
		rts

; ************************************************************************************************************************************
; set midi device
setMidiInstrument:
		lda #$c0							; set the instrument for channel 0
		sta MIDI_COM
		ldx midiInst								; this is the instrument number
		lda midiTable,x
		sta MIDI_COM
		rts

makeMusic:									; make midi note
			
		lda #$90							; strike a note on channel 0
		sta MIDI_COM
		lda ballNote,X						; send note value based on what ball hit the edge
		sta MIDI_COM
		lda #$40							; set strike velocity ($40 is the default value)
		sta MIDI_COM
		rts

makeColor:
		lda colorG,X						; get the reference color
		sta ballG,X							; and store it in the ball
		lda colorR,X						; for all three colors
		sta ballR,X
		lda colorB,X
		sta ballB,X
		rts
dropColor:
		dec ballG,X							; each cycle reduce the color value by one
		dec ballR,x 
		dec ballB,x 
		lda ballG,X							; check if we've gone too far
		cmp #$ff							; and reset to zero
		bne checkRed 
		lda #$00
		sta ballG,x 
checkRed:
		lda ballR,x 
		cmp #$ff
		bne checkBlue 
		lda #$00
		sta ballR,x 
checkBlue:
		lda ballB,x 
		cmp #$ff
		bne colorDone
		lda #$00
		sta ballB,x 
colorDone:
		rts

assignColor:
		lda #$01							; Change I/O control to page 1
		sta MMU_IO_CTRL

		txa									; grab which ball we have
		clc				
		adc #$02							; and add two for the first color to first ball
		asl									; then multiply by 4 to get to the right
		asl									; location for the RGBA bytes for each color
		tay									; and transfer to Y for indirect indexing
		lda #<VKY_GR_CLUT_0					; Set destination pointer to Graphics CLUT 0
		sta ptr_dst
		lda #>VKY_GR_CLUT_0
		sta ptr_dst+1

		lda ballB,x 						; get the ball color
		sta (ptr_dst),y						; and store it in the CLUT
		iny									; incrment y for next color component
		lda ballR,x 						; and repeat
		sta (ptr_dst),y 
		iny
		lda ballG,x 
		sta (ptr_dst),y 
		stz MMU_IO_CTRL						; reset mmu IO to zero
		rts
; ************************************************************************************************************************************
SetTimer:	
		inc $d0
		lda $d0
		sta kernel.args.timer.absolute		; store in timer.absolute paramter
		sta kernel.args.timer.cookie		; saved as a cookie to the kernel (same as frame number)
		lda #kernel.args.timer.FRAMES		; set the Timer to Frames
		sta kernel.args.timer.units			; store in units parameter
		jsr kernel.Clock.SetTimer			; jsr to Kernel routine to set timer
		rts
; ************************************************************************************************************************************
setBalls:									; We'll set the base speed for the slowest ball here
		lda speedBase
		ldx #23 
		sta ballSdF,X
		lda speedBase+1
		sta ballSdL,X
		dex
		
sbLoop:
		clc									; Then we'll add the speed ratio to each ball up the chain
		lda ballSdF+1,X						; so each ball is faster than the previous.
		adc speedRatio						; save these values in our speed table
		sta ballSdF,X
		lda ballSdL+1,X
		adc #$00
		sta ballSdL,X
		dex
		bmi sbDone
		bra sbLoop
sbDone:
		rts
; ************************************************************************************************************************************
clrScreen:
		ldx #$00							; set x for indexing
csLoop:
		lda #$02							; set the output to character matrix
		sta MMU_IO_CTRL
		lda #$20							; set a to a blank character
		sta $c000+$000,x 					; and save every 240 memory locations
		sta $c000+$0f0,x 					;
		sta $c000+$1e0,x 					; We're only going to loop once instead of
		sta $c000+$2d0,x 					; nesting loops
		sta $c000+$3c0,x 
		lda #$03							; set the output to the color matrix
		sta MMU_IO_CTRL
		lda #$f0							; pick white
		sta $c000+$000,x 					; do the same save groups
		sta $c000+$0f0,x 
		sta $c000+$1e0,x 
		sta $c000+$2d0,x 
		sta $c000+$3c0,x 
		inx									; inc x
		cpx #$f1 							; and check if we've hit 241
		bcc csLoop							; if less, continue looping

		stz MMU_IO_CTRL						; reset IO to 0
		rts

; ************************************************************************************************************************************
printMenu:
		ldx #$00
pMenuLoop:
		lda menu,x
		cmp #$ff
		beq pMenuDone
		sta ptr_dst
		inx
		lda menu,x
		sta ptr_dst+1
		inx
		lda menu,x
		sta ptr_src
		inx
		lda menu,x 
		sta ptr_src+1
		jsr outputText
		inx
		bra pMenuLoop
pMenuDone:
		jsr printSpeed 
		jsr printGap
		jsr printMidiInst
		
		rts

outputText:
		lda #$02
		sta MMU_IO_CTRL
		ldy #$00
oTextLoop:
		lda (ptr_src),y
		cmp #94
		beq oTextDone
		sta (ptr_dst),y
		iny
		bra oTextLoop
oTextDone:
		stz MMU_IO_CTRL
		rts

printSpeed:
		lda #$02
		sta MMU_IO_CTRL
		lda speedbase+1
		and #$0f
		tax
		lda hex,x 
		sta $c161
		lda speedbase
		lsr
		lsr
		lsr
		lsr
		tax
		lda hex,X
		sta $c161+1
		lda speedbase
		and #$0f
		tax
		lda hex,x 
		sta $c161+2
		stz MMU_IO_CTRL
		rts

printgap:
		lda #$02
		sta MMU_IO_CTRL
		lda speedRatio
		lsr
		lsr
		lsr
		lsr
		tax
		lda hex,x 
		sta $c1b1
		lda speedRatio
		and #$0f
		tax
		lda hex,x
		sta $c1b1+1
		stz MMU_IO_CTRL
		rts

printMidiInst:
		ldx midiInst
		lda midiTable,x
		sta MULU_A_L
		stz MULU_A_H
		lda #23
		sta MULU_B_L
		stz MULU_B_H
		clc
		lda MULU_LL
		adc #<instrumentList
		sta ptr_src
		lda MULU_LH
		adc #>instrumentList
		sta ptr_src+1
		lda #$33
		sta ptr_dst
		lda #$c2 
		sta ptr_dst+1
		lda #$02
		sta MMU_IO_CTRL
		ldy #$00
pMidiLoop:
		lda #$02
		sta MMU_IO_CTRL
		lda (ptr_src),y
		sta (ptr_dst),y
		inc MMU_IO_CTRL
		lda #$b0
		sta (ptr_dst),y
		iny
		cpy #23
		bcc pMidiLoop
		stz MMU_IO_CTRL
		rts
; ************************************************************************************************************************************

resetBalls:
		ldx #00
rBallLoop:
		lda #$20
		sta ballXLO,X
		lda #$00
		sta ballxHI,x 
		sta ballXFR,x 
		lda #$01
		sta ballDir,x 
		inx
		cpx #24
		bcc rBallLoop
		rts
; ************************************************************************************************************************************
; working memory
; ball direction
ballDir:	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
; ball speed
ballSdF:	.byte $00,$f0,$e0,$d0,$c0,$b0,$a0,$90,$80,$70,$60,$50,$40,$30,$20,$10,$00,$f0,$e0,$d0,$c0,$b0,$a0,$90
ballSdL:	.byte $02,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00
; ball X position
ballXFR:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ballXLO:	.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
ballXHI:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; ball Y position
ballYLO:	.byte $21,$2b,$35,$3f,$49,$53,$5d,$67,$71,$7b,$85,$8f,$99,$a3,$ad,$b7,$c1,$cb,$d5,$df,$e9,$f3,$fd,$07
ballYHI:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
; midi notes, middle C0 down to C2
ballNote:	.byte 60, 59, 58, 57, 56, 55, 54, 53, 52 ,51 ,50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37
; Ball color
ballG:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ballR:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ballB:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

midiTable:	.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,24,25,26,27,28,32,33,34,35,36,37,38,39,45,46,47     ;31
			.byte 88,96,98,99,100,103,104,105,106,107,108,112,113,114,115,116,117,118,123,127				;51

; 				 Cyan,DeepSkyBlue,DodgerBlue,ElecBlue,Blue,Indigo,Violet,Purple,Magenta,Rose,HotPink,DeepPink,Red,Vermilion,Orange,Amber,Yellow,Chartreuse,Lime,BGreen,Green,SpGreen,MedSpGrn,Aqua
colorG:		.byte $00,$00,        $00,       $00,     $00, $00,   $3F,   $7F,   $BF,    $FF, $FF,    $FF,     $FF,$FF,      $FF,   $FF,  $FF,   $BF,       $7F, $3F,   $00,  $00,    $00,     $00
colorR:		.byte $FF,$BF,        $7F,       $3F,     $00, $00,   $00,   $00,   $00,    $00, $00,    $00,     $00,$3F,      $7F,   $BF,  $FF,   $FF,       $FF, $FF,   $FF,  $FF,    $FF,     $FF
colorB:		.byte $FF,$FF,        $FF,       $FF,     $FF, $BF,   $BF,   $BF,   $BF,    $BF, $7F,    $7F,     $7F,$7F,      $3F,   $3F,  $3F,   $3F,       $00, $00,   $00,  $00,    $00,     $00

speedBase:		.word $0090 					; The slowest ball speed
speedRatio:		.byte $10						; the amount between balls

resultFR:		.byte $00
resultLO:		.byte $00

midiInst:		.byte 11
totalColors:	.byte 32
spriteLoc:		.word $0000

menuFlag:		.byte $00
hex:			.text "0123456789abcdef"


font:
.binary "atari.bin"
.include "menu.s"
.include "ball.s"
.include "midi_instruments.s"