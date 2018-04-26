;         V I C   A L I E N - I N V A D E R S
;
;             by Davide Bucci, April 2018
;
; This program is a Space-Invaders clone that runs on an unexpanded VIC-20
; A certain amount of work has been done to ensure that the graphics is smooth
; enough so that the effect hopefully appears to be quite polished.
; It is my first attempt to write a complete game in 6502 assembly language,
; even if I learnt that language many years ago and I used it mainly as a
; complement for BASIC programs.
;
; The assembler used is ca65

; General-use addresses

        GRCHARS1 = $1C00     ; Address of user-defined characters

; KERNAL routines used
        GETIN = $FFE4

; VIC-chip addresses

        VICSCRHO = $9000    ; Horisontal position of the screen
        VICSCRVE = $9001    ; Vertical position of the screen
        VICCOLNC = $9002    ; Screen width in columns and video memory addr.
        VICROWNC = $9003    ; Screen height, 8x8 or 8x16 chars, scan line addr.
        VICCHGEN = $9005    ; Character gen. and video matrix addresses.
        VICCOLOR = $900F    ; Screen and border colours

        MEMSCR   = $1E00    ; Start address of the screen memory (unexp. VIC)
        MEMCLR   = $9600    ; Start address of the colour memory (unexp. VIC)

        REPEATKE = $028A    ; Repeat all keys

.export main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

; Main program here.

main:
            jsr Init
            lda #34
            sta AlienPosY   ; Initial position of aliens
            jsr DrawAliens

@mainloop:  jsr GETIN
            beq @mainloop
            cmp #$58        ; X: increase position of the cannon
            bne @continue1
            ;inc CannonPos
            lda #$08
            clc
            adc CannonPos
            sta CannonPos
            cmp #$7F
            bcc @continue1
            lda #$7F
            sta CannonPos
@continue1: cmp #$5A        ; Z: decrease position of the cannon
            bne @continue2
            ;dec CannonPos
            sec
            lda CannonPos
            sbc #$08
            bcc @continue2
            sta CannonPos
@continue2: jmp @mainloop

; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT
;
; Initialization code: prepare the screen to the correct size, center it and
; load the graphic chars and configure the IRQ handler.
;
; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT

Init:
            lda #$80        ; Autorepeat on on the keyboard
            sta REPEATKE
            lda #$08        ; Define screen colour and background (black)
            sta VICCOLOR
            lda #$90        ; Set a 16 column-wide screen
            sta VICCOLNC
            lda #$BE        ; Set a 31 row-high column
            sta VICROWNC
            lda #$16        ; Center the screen vertically...
            sta VICSCRVE
            lda #$12        ; ... and horisontally
            sta VICSCRHO
            lda #$FF        ; Move the character generator address to $1C00
            sta VICCHGEN    ; while leaving ch. 128-255 to their original pos.
            jsr MovCh       ; Load the graphic chars
            jsr CLS

            sei             ; Configure the interrupt handler
            lda #<IrqHandler
            sta $0314
            lda #>IrqHandler
            sta $0315
            cli
            jsr test1l
            rts

test1l:
            ldx #0
            ldy #0
            lda #$1
            sta Colour
            lda #(49+$80)
            jsr DrawChar
            ldx #1
            ldy #0
            lda #$1
            sta Colour
            lda #(50+$80)
            jsr DrawChar
            ldx #2
            ldy #0
            lda #$1
            sta Colour
            lda #(51+$80)
            jsr DrawChar
            ldx #3
            ldy #0
            lda #$1
            sta Colour
            lda #(52+$80)
            jsr DrawChar
            rts

; Copy the graphic chars. They are subjected to be changed during the pixel-by
; pixel movement, so that routine gives only the initial situation.

MovCh:
            ldx #0
@loop:
            lda DefChars,x
            sta GRCHARS1,x
            inx
            cpx #5*8
            bne @loop
            rts

; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ
;
; This is the interrupt handler, called 60 times each second when the VIC-20
; is working normally. It does the following things:
;
; 1 - Calculate positions of the aliens and, if necessary, redraw them
; 2 - Calculate positions of the falling bombs and fire shoots and draw them
; 3 - Update the position of the cannon and draw it
; 5 - Check for collisions and handle explosions
; 6 - Call the sound driver
; 7 - Jump to the original IRQ handler (for scanning the keyboard, etc).
;
; The user interface is handled outside of the interrupt, in the main program
; loop and the communication with the IRQ handler is made by a set of
; appropriate flags. This approach has the following advantages:
;
; - The speed of the aliens and of the cannon is controlled very precisely as
;   the IRQ handler is called at a predictable and stable rate.
; - The code for the visualization and for the user interface (i.e. recognizing
;   the joystick movements) is kept separate.
;
; The main drawback is that the IRQ is supposed to do many things and it should
; be doing that VERY RAPIDLY in order not to mess with the calling order. As a
; rule of thumb, I would say that the IRQ should be completed in less than 5 ms
; at least most of times.
;
; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ

IrqHandler:
            pha
            txa             ; Save registers
            pha
            tya
            pha

            lda IrqCn
            cmp #30         ; Increment the Y position of the aliens 
            bne @cont
            lda #0
            sta IrqCn
            lda #$05        ; Erase aliens in the current position
            sta AlienCode1
            lda #$05
            sta AlienCode2
            jsr DrawAliens
            inc AlienPosY
            lda AlienPosY
            cmp #25*8
            bne @draw
            lda #0
            sta AlienPosY
@draw:      lda #$FF        ; Make sure position of the cannon is updated.
            sta OldCannonP
            lda #$00
            sta AlienCode1
            lda #$01
            sta AlienCode2
            jsr DrawAliens
@cont:
            lda CannonPos   ; Check if the cannon position has changed
            cmp OldCannonP
            beq @nochange
            jsr ClearCannon
            lda CannonPos   ; Update the OldCannonP value to the current pos.
            sta OldCannonP
            jsr DrawCannon
@nochange:
            inc IrqCn
            pla             ; Retrieve registers
            tay
            pla
            tax
            pla

            jmp $EABF       ; Jump to the standard IRQ handling routine

; Draw the cannon on the screen, at the current position, contained in
; CannonPos (in pixels).

DrawCannon:
            lda CannonPos
            lsr                 ; The position is in pixel, divide by 8
            lsr                 ; to obtain position in characters
            lsr
            tax
            ldy #30             ; Vertical position of the cannon
            lda #$1             ; Cannon in white
            sta Colour
            lda #$4             ; Cannon char
            jsr DrawChar
            rts

; Clear the cannon on the screen, at the current position, contained in
; OldCannonP (in pixels).

ClearCannon:
            lda OldCannonP
            lsr                 ; The position is in pixel, divide by 8
            lsr                 ; to obtain position in characters
            lsr
            tax
            ldy #30             ; Vertical position of the cannon
            lda #$1             ; Cannon in white
            sta Colour
            lda #$5             ; Space
            jsr DrawChar

; Draw the aliens on the screen. They are several lines with at most 8 aliens
; each. The presence of an alien in the first row is given by bits in the
; AliensR1 byte. An alien is present at the beginning of the game (or level)
; and can be destroyed when hit. In this case, the corresponding bit in the
; AliensR1 byte is set to 0. Same for AliensR2 and AliensR3.

DrawAliens:
            lda #$FF
            sta AliensR1
            sta AliensR2
            ldx #$10
            lda AlienPosY      ; The position is in pixel, divide by 8
            lsr                ; to obtain position in characters
            lsr
            lsr
            sta AlienCurrY
@loop1:     dex
            ldy AlienCurrY
            lda AliensR1
            clc
            rol
            sta AliensR1
            bcs @drawAlien0
@ret1:      dex
            bne @loop1
            inc AlienCurrY
            inc AlienCurrY
            ldx #$10
@loop2:     dex
            ldy AlienCurrY
            lda AliensR2
            clc
            rol
            sta AliensR1
            bcs @drawAlien1
@ret2:      dex
            bne @loop2
            inc AlienCurrY
            rts

@drawAlien0:
            lda #2
            sta Colour
            lda AlienCode1
            jsr DrawChar
            jmp @ret1
@drawAlien1:
            lda #3
            sta Colour
            lda AlienCode2
            jsr DrawChar
            jmp @ret2

; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. It uses Dummy1 and Dummy2.
; Colour indicates the colour code of the character. It uses 3 bytes in the
; stack and does not change registers.

DrawChar:
            sta Dummy1
            stx Dummy2
            pha
            txa             ; Save registers
            pha
            tya
            pha

            tya
            asl             ; 16 columns per line
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            bcc @tophalf    ; we need to write in the bottom-half of the screen
            adc Dummy2
            tay
            lda Dummy1
            sta MEMSCR+255,Y
            lda Colour
            sta MEMCLR+255,Y
            jmp @exit
@tophalf:   adc Dummy2
            tay
            lda Dummy1
            sta MEMSCR,Y
            lda Colour
            sta MEMCLR,Y
@exit:      pla             ; Retreive registers
            tay
            pla
            tax
            pla
            rts

; Clear the screen. This maybe is too slow to be used in an interrupt handler.
CLS:
            size=16*31/4
            ldx #size
@loop:      lda #5
            sta MEMSCR,X            ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size,X       ; to mess with a 16-bit loop.
            sta MEMSCR+size*2,X
            sta MEMSCR+size*3,X
            lda #0
            sta MEMCLR,X
            sta MEMCLR+size,X
            sta MEMCLR+size*2,X
            sta MEMCLR+size*3,X
            dex
            bne @loop
            lda #5
            sta MEMSCR,X            ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size,X       ; to mess with a 16-bit loop.
            sta MEMSCR+size*2,X
            sta MEMSCR+size*3,X
            lda #0
            sta MEMCLR,X
            sta MEMCLR+size,X
            sta MEMCLR+size*2,X
            sta MEMCLR+size*3,X
            rts

; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA
;
; Data and configuration settings.
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

Dummy1:     .byte $00
Dummy2:     .byte $00
IrqCn:      .byte $00

Colour:     .byte $00           ; Colour to be used by the printing routines
AliensR1:   .byte $F7           ; Presence of aliens in row 1
AliensR2:   .byte $F7           ; Same for row 2
AliensR3:   .byte $F7           ; Same for row 3
AlienCode1: .byte $00           ; Character for alien row 1
AlienCode2: .byte $00           ; Character for alien row 2
AlienCode3: .byte $00           ; Character for alien row 3
AlienPosX:  .byte $00           ; Horisontal position of aliens (in pixels)
AlienPosY:  .byte $00           ; Vertical position of aliens (in pixels)
AlienCurrY: .byte $00           ; Vertical position of alien being drawn
CannonPos:  .byte $8*8          ; Horisontal position of the cannon (in pixels)
OldCannonP: .byte $00           ; Old position of the cannon

DefChars:
            .byte %00111100     ; Alien #1, associated to ch. 0 (normally @)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01000010
            .byte %10000001

            .byte %00111100     ; Alien #2, associated to ch. 1 (normally A)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01100110
            .byte %00000000
            
            .byte %10000001     ; Alien #3, associated to ch. 2 (normally B)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %11000011
            .byte %00100100

            .byte %10000001     ; Alien #4, associated to ch. 3 (normally C)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01000010
            .byte %10000001

            .byte %00010000     ; Cannon, associated to ch. 4 (normally D)
            .byte %00111000
            .byte %00111000
            .byte %01111100
            .byte %01111100
            .byte %01111100
            .byte %11101110
            .byte %11000110

            .byte $0            ; Blank char, ch. 5 (E)
            .byte $0
            .byte $0
            .byte $0
            .byte $0
            .byte $0
            .byte $0
            .byte $0
            
            