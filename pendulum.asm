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

.section zp									; Zero page section $20 to $28
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
		lda #%00100100						; Graphic, Sprites Engine enabled  			|xx|GM|SP|TL|BM|GR|OV|TX|
		sta VKY_MSTR_CTRL_0					;                  		                    | 0| 0| 1| 0| 0| 1| 0| 0|

		lda #%00000000						; Text mode options for the overlay 		|xx|xx|FS|FO|MS|2Y|2X|70|
		sta VKY_MSTR_CTRL_1					; 320 x 240, 60 Hz, dbl X & Y				| 0| 0| 0| 0| 0| 0| 0| 0|
		stz VKY_BRDR_CTRL					; No Border

		lda #$00							; Set the background color
		sta VKY_BKG_COL_R
		lda #$00
		sta VKY_BKG_COL_G
		lda #$00
		sta VKY_BKG_COL_B

; ************************************************************************************************************************************
; enable 24 sprites
		lda #<sprite1
		sta spriteLoc
		lda #>sprite1
		sta spriteLoc+1
		ldx #$00
spriteLoop:
		txa
		asl 
		asl 
		asl
		tay
		lda #%01100001			; 8x8 sprite, layer 0, lut 0, enable on
		sta VKY_SP0,y 
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
		lda spriteLoc
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

		ldx #$00							; X is the number of colors to copy, check for 154
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
; set midi device

		lda #$c0							; set the instrument for channel 0
		sta MIDI_COM
		lda #11								; this is the instrument number
		sta MIDI_COM
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
		
		jsr handle_events
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
		beq UpdateScreen					; run the screen update
		rts

UpdateScreen:
		jsr SetTimer						; reset timer for next SOF
		jsr moveBalls						; move the balls
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

makeMusic:									; make midi note
			
		lda #$90							; set channel 0
		sta MIDI_COM
		lda ballNote,X						; send note value based on what ball hit the edge
		sta MIDI_COM
		lda #$40							; volume is default velocity
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

		lda ballG,x 						; get the ball color
		sta (ptr_dst),y						; and store it in the CLUT
		iny									; incrment y for next color component
		lda ballR,x 						; and repeat
		sta (ptr_dst),y 
		iny
		lda ballB,x 
		sta (ptr_dst),y 
		stz MMU_IO_CTRL						; reset mmu IO to zero
		rts
; ************************************************************************************************************************************
; jsr routines
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
; working memory

ballDir:	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
ballSdF:	.byte $00,$f0,$e0,$d0,$c0,$b0,$a0,$90,$80,$70,$60,$50,$40,$30,$20,$10,$00,$f0,$e0,$d0,$c0,$b0,$a0,$90
ballSdL:	.byte $02,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00
ballXFR:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ballXLO:	.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
ballXHI:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ballYLO:	.byte $21,$2b,$35,$3f,$49,$53,$5d,$67,$71,$7b,$85,$8f,$99,$a3,$ad,$b7,$c1,$cb,$d5,$df,$e9,$f3,$fd,$07
ballYHI:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
;midi notes, middle C0 down to C2
ballNote:	.byte 60, 59, 58, 57, 56, 55, 54, 53, 52 ,51 ,50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37
ballG:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ballR:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
ballB:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


; 				 Cyan,DeepSkyBlue,DodgerBlue,ElecBlue,Blue,Indigo,Violet,Purple,Magenta,Rose,HotPink,DeepPink,Red,Vermilion,Orange,Amber,Yellow,Chartreuse,Lime,BGreen,Green,SpGreen,MedSpGrn,Aqua
colorG:		.byte $00,$00,        $00,       $00,     $00, $00,   $3F,   $7F,   $BF,    $FF, $FF,    $FF,     $FF,$FF,      $FF,   $FF,  $FF,   $BF,       $7F, $3F,   $00,  $00,    $00,     $00
colorR:		.byte $FF,$BF,        $7F,       $3F,     $00, $00,   $00,   $00,   $00,    $00, $00,    $00,     $00,$3F,      $7F,   $BF,  $FF,   $FF,       $FF, $FF,   $FF,  $FF,    $FF,     $FF
colorB:		.byte $FF,$FF,        $FF,       $FF,     $FF, $BF,   $BF,   $BF,   $BF,    $BF, $7F,    $7F,     $7F,$7F,      $3F,   $3F,  $3F,   $3F,       $00, $00,   $00,  $00,    $00,     $00


resultFR:	.byte $00
resultLO:	.byte $00

totalColors:	.byte 32
spriteLoc:		.word $0000



.include "ball.s"