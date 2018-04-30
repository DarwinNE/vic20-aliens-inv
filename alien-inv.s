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

; Difficulty-related constants
        BOMBPROB = $70      ; Higher = more bombs falling
        PERIOD = 20         ; Higher = slower alien movement
        FIRESPEED = 2       ; Higher = faster speed of cannon fire

; General constants
        NMBOMBS = 8         ; Maximum number of bombs falling at the same time
        NMSHOTS = 8         ; Maximum number of cannon shots at the same time

; General-use addresses
        GRCHARS1 = $1C00    ; Address of user-defined characters. Since in the
                            ; unexpanded VIC the screen matrix starts at
                            ; $1E00, there are 512 bytes free, i.e. 64 chars
                            ; that can be defined. That leaves 3059 bytes free
                            ; for the machine language code (counting the
                            ; 752 SYS4109 stub in BASIC that launches the
                            ; program.

; Colour constants for the VIC 20
        BLACK    = $00
        WHITE    = $01
        RED      = $02
        CYAN     = $03
        MAGENTA  = $04
        GREEN    = $05
        BLUE     = $06
        YELLOW   = $07
        

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
            sec
            lda CannonPos
            sbc #$08
            bcc @continue2
            sta CannonPos
@continue2: cmp #$20        ; Space: fire!
            bne @continue3
            ldx #0          ; Search for the first free shot
@search:    lda FireSpeed,X ; (i.e. whose speed = 0)
            cmp #0
            beq @found
            inx
            cmp NMSHOTS
            bne @search
            jmp @continue3  ; No enough shots allowed in parallel. Abort fire.
@found:     lda CannonPos
            lsr
            lsr
            lsr
            sta FirePosX,X
            lda #30
            sta FirePosY,X
            lda #1
            sta FireSpeed,X
@continue3: jmp @mainloop

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
            jsr DrawShield
            rts

; Draw the shields in the following positions:
; ------------1---
; 0---4---8---2---
;  **  **  **  **

DrawShield: ldx #1
            ldy #29
            lda #WHITE
            sta Colour
            lda #BLOCK
            jsr DrawChar
            ldx #2
            jsr DrawChar
            ldx #5
            jsr DrawChar
            ldx #6
            jsr DrawChar
            ldx #9
            jsr DrawChar
            ldx #10
            jsr DrawChar
            ldx #13
            jsr DrawChar
            ldx #14
            jsr DrawChar

            ldx #1
            ldy #28
            lda #BLOCKL
            jsr DrawChar
            ldx #2
            lda #BLOCKR
            jsr DrawChar
            ldx #5
            lda #BLOCKL
            jsr DrawChar
            ldx #6
            lda #BLOCKR
            jsr DrawChar
            ldx #9
            lda #BLOCKL
            jsr DrawChar
            ldx #10
            lda #BLOCKR
            jsr DrawChar
            ldx #13
            lda #BLOCKL
            jsr DrawChar
            ldx #14
            lda #BLOCKR
            jsr DrawChar
            rts

test1l:
            ldx #0
            ldy #0
            lda #WHITE
            sta Colour
            lda #(49+$80)
            jsr DrawChar
            ldx #1
            lda #(52+$80)
            jsr DrawChar
            ldx #2
            lda #(51+$80)
            jsr DrawChar
            ldx #3
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
            cpx #(LASTCH+1)*8
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
            cmp #PERIOD     ; Exercute every PERIOD/60 of second
            bne @cont3
            lda #0
            sta IrqCn
            lda #EMPTY      ; Erase aliens in the current position
            sta AlienCode1
            lda #EMPTY
            sta AlienCode2
            jsr DrawAliens  ; Redraw aliens
            jsr FallBombs   ; Make bombs fall. Aliens will be on top of bombs
            inc AlienPosY   ; Increment the Y position of the aliens
            lda Direction   ; Increment or decrement the X position,
            and #$01        ; depending on the Direction value
            beq @negative
@positive:  inc AlienPosX   ; The postion should be increased
            jmp @cont
@negative:  dec AlienPosX   ; The position should be decreased
@cont:      lda AlienPosX
            cmp #3
            bcc @cont1
            lda Direction   ; Invert the direction
            eor #$FF
            sta Direction
@cont1:     cmp #$FF        ; Check if the position is negative
            bcs @cont2
            lda Direction   ; Invert the direction
            eor #$FF
            sta Direction
@cont2:     lda AlienPosY   ; Check if the aliens came to bottom of screen
            cmp #28*8
            bne @draw
            lda #1          ; Reset the position of aliens (placeholder)
            sta AlienPosY
@draw:      lda #$FF        ; Make sure position of the cannon is updated.
            sta OldCannonP
            lda #ALIEN1
            sta AlienCode1
            lda #ALIEN2
            sta AlienCode2
            jsr DrawAliens
@cont3:     lda CannonPos   ; Check if the cannon position has changed
            cmp OldCannonP
            beq @nochange
            jsr ClearCannon
            lda CannonPos   ; Update the OldCannonP value to the current pos.
            sta OldCannonP
            jsr DrawCannon
@nochange:  jsr CannonFire  ; Update the position of cannon shots
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
            lda #WHITE          ; Cannon in white
            sta Colour
            lda #CANNON         ; Cannon char
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
            lda #WHITE          ; Cannon in white
            sta Colour
            lda #EMPTY          ; Space
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
            ldx #8*2
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
            ldx #8*2
@loop2:     dex
            ldy AlienCurrY
            lda AliensR2
            clc
            rol
            sta AliensR2
            bcs @drawAlien1
@ret2:      dex
            bne @loop2
            inc AlienCurrY
            rts

@drawAlien0:
            lda #RED
            sta Colour
            txa
            pha
            clc
            adc AlienPosX
            tax
            lda AlienCode1
            jsr DrawChar
            pla
            tax
            jmp @ret1
@drawAlien1:
            lda #CYAN
            sta Colour
            txa
            pha
            clc
            adc AlienPosX
            tax
            lda AlienCode2
            jsr DrawChar
            pla
            tax
            jmp @ret2

; Control the movement of the bullet/laser shot fired by the cannon.

CannonFire: ldx #0              ; Update the position of the shot
@loop:      lda FireSpeed,X     ; Check if the shot is active (speed>0)
            cmp #0
            beq @cont
            lda FirePosY,X
            sec                 ; If speed >0, update current Y position
            sbc FireSpeed,X     ; The movement is vertical, so subtract
            cmp #1              ; Check if we reached the top of the screen
            bcs @stillf
            lda #$FF            ; In this case, destroy the bomb
            sta FireSpeed,X
@stillf:    sta FirePosY,X
            ; At this point we should check for collisions!
@cont:      inx
            cpx #NMSHOTS
            bne @loop
DrawShots:  ldx #0              ; Draw bombs
            lda #YELLOW         ; Colour of the shots
            sta Colour
@loop4:     stx tmp1
            lda FireSpeed,X     ; Do not draw inactive shots
            cmp #0
            beq @notmove
            lda FirePosX,X
            sta tmp2            ; Store the X position of the shot
            lda FirePosOY,X
            cmp FirePosY,X
            beq @notmove
            tay
            lda #EMPTY          ; Erase the previous shot
            ldx tmp2            ; Load the X position
            jsr DrawChar        ; Erase the shot in the old position
            ldx tmp1
            lda FireSpeed,X     ; Check if the shot should be destroyed
            cmp #$FF
            bne @normal
            lda #0
            sta FireSpeed,X
@normal:    lda FirePosY,X
            sta FirePosOY,X     ; Save the current position
            tay
            lda #SHOT
            ldx tmp2
            jsr DrawChar        ; Draw the shot in the new position
@notmove:   ldx tmp1
            inx
            cpx #NMSHOTS
            bne @loop4
            rts


; Control bombs dropping. A maximum of 8 bombs can be falling at the same
; time. A bomb is active and falling if its speed is greater than 0.
; BombSpeed, BombPosX and BombPosY are the arrays containing the speed and the
; positions. Exploit tmp1 and tmp2, change registers A, X, Y.
; This routine does three things:
;
; 1 - For each alien alive, and for each of the 8 bombs available, decide if
;     a bomb is dropped by drawing a random number and check if it is inside
;     a given interval
; 2 - Update the positions of the bombs active in the screen and check for
;     collisions.
; 3 - Draw the bombs in the new positions on the screen.
;

FallBombs:  lda #$FF
            sta AliensR1
            sta AliensR2
            ldx #NMBOMBS
            lda AlienPosY       ; The position is in pixel, divide by 8
            lsr                 ; to obtain position in characters
            lsr
            lsr
            sta AlienCurrY      ; This byte contains the current position of
@loop1:     ldy AlienCurrY      ; aliens.
            lda AliensR1        ; Shift AliensR1 and check the carry to see
            clc                 ; which aliens are alive.
            rol
            sta AliensR1
            bcc @ret1
            jsr DropBomb
@ret1:      dex
            bne @loop1          ; End of loop for processing the first line
            inc AlienCurrY
            inc AlienCurrY
            ldx #NMBOMBS
@loop2:     ldy AlienCurrY
            lda AliensR2
            clc
            rol
            sta AliensR2
            bcc @ret2
            jsr DropBomb
@ret2:      dex
            bne @loop2

            ldx #0              ; Update the position of the bombs
@loop3:     lda BombSpeed,X     ; Check that the bomb is active (speed>0)
            cmp #0
            beq @cont
            clc                 ; If speed >0, update current Y position
            adc BombPosY,X
            cmp #31             ; Check if we reached the last line
            bcc @stillf
            lda #0              ; In this case, destroy the bomb
            sta BombSpeed,X
@stillf:    sta BombPosY,X
            ; At this point we should check for collisions!
@cont:      inx
            cpx #NMBOMBS
            bne @loop3

DrawBombs:  ldx #0              ; Draw bombs
            lda #MAGENTA        ; Colour of the bombs
            sta Colour
@loop4:     stx tmp1
            lda BombSpeed,X     ; Do not draw inactive bombs
            cmp #0
            beq @notmove
            lda BombPosX,X
            sta tmp2            ; Store the X position of the bomb
            lda BombPosOY,X
            cmp BombPosY,X
            beq @notmove
            tay
            lda #EMPTY          ; Erase the previous bomb
            ldx tmp2            ; Load the X position
            jsr DrawChar        ; Erase the bomb in the old position
            ldx tmp1
            lda BombPosY,X
            sta BombPosOY,X     ; Save the current position
            tay
            lda #BOMB
            ldx tmp2
            jsr DrawChar        ; Draw the bomb in the new position
@notmove:   ldx tmp1
            inx
            cpx #NMBOMBS
            bne @loop4
            rts

; Decide if a bomb should be dropped or not.
; X should contain the current alien being processed in the line and is not
; changed by this routine.

DropBomb:   jsr GetRand
            lda Random          ; Get a random number and check if it is less
            cmp #BOMBPROB       ; than a given threshold
            bcc @nobomb
            ldy #$FF            ; That will overflow to 0 at the first iny
@searchlp:  iny
            cpy #NMBOMBS        ; Check if there is still place for bombs
            beq @nobomb
            lda #0
            cmp BombSpeed,Y     ; Check if the current bomb is not active
            bne @searchlp
            lda #1
            sta BombSpeed,Y
            lda AlienCurrY
            sta BombPosY,Y
            asl
            clc
            adc #$01
            sta BombPosX,Y
@nobomb:    rts

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
            txa
            cmp #16         ; Check if the X value is out of range
            bcs @exit       ; Exit if X greater than 16 (no of columns)
            tya
            cmp #31         ; Check if the Y value is out of range
            bcs @exit       ; Exit if Y greater than 31 (no of rows)
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            bcc @tophalf    ; we need to write in the bottom-half of the screen
            clc
            adc Dummy2
            tay
            lda Dummy1
            sta MEMSCR+256,Y
            lda Colour
            sta MEMCLR+256,Y
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
@loop:      lda #EMPTY
            sta MEMSCR,X            ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size,X       ; to mess with a 16-bit loop.
            sta MEMSCR+size*2,X
            sta MEMSCR+size*3,X
            lda #BLACK
            sta MEMCLR,X
            sta MEMCLR+size,X
            sta MEMCLR+size*2,X
            sta MEMCLR+size*3,X
            dex
            bne @loop
            lda #EMPTY
            sta MEMSCR,X            ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size,X       ; to mess with a 16-bit loop.
            sta MEMSCR+size*2,X
            sta MEMSCR+size*3,X
            lda #BLACK
            sta MEMCLR,X
            sta MEMCLR+size,X
            sta MEMCLR+size*2,X
            sta MEMCLR+size*3,X
            rts


; Random number generation routine. Adapted from here: 
; http://sleepingelephant.com/ipw-web/bulletin/bb/viewtopic.php?t=2304
; Creates a pseudo-random number in Random and Random+1
; Change register A and employs tmp1

GetRand:
            LDA Random+1
            STA tmp1
            LDA Random
            ASL
            ROL tmp1
            ASL
            ROL tmp1
            CLC
            ADC Random
            PHA
            LDA tmp1
            ADC Random+1
            STA Random+1
            PLA
            CLC             ; added this instruction - kweepa
            ADC #$11
            STA Random
            LDA Random+1
            ADC #$36
            STA Random+1
            RTS

; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA
;
; Data and configuration settings.
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

Random:     .word $0000
Dummy1:     .byte $00
Dummy2:     .byte $00
IrqCn:      .byte $00
tmp1:       .byte $00
tmp2:       .byte $00

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
Direction:  .byte $00           ; The first bit indicates aliens' X direction
CannonPos:  .byte $8*8          ; Horisontal position of the cannon (in pixels)
OldCannonP: .byte $00           ; Old position of the cannon

BombSpeed:  .res NMBOMBS, $00   ; Array containing the speed of the bombs sent
BombPosX:   .res NMBOMBS, $00   ; Array with X positions of bombs
BombPosY:   .res NMBOMBS, $00   ; Array with Y positions of bombs
BombPosOY:  .res NMBOMBS, $00   ; Array with old Y positions of bombs

FireSpeed:  .res NMSHOTS, $00   ; Array containing the speed of the bullet sent
FirePosX:   .res NMSHOTS, $00   ; Array with X positions of bullet
FirePosY:   .res NMSHOTS, $00   ; Array with Y positions of bullet
FirePosOY:  .res NMSHOTS, $00   ; Array with old Y positions of bullet


DefChars:
            ALIEN1 = 0
            .byte %00111100     ; Alien #1, associated to ch. 0 (normally @)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01000010
            .byte %10000001

            ALIEN2 = 1
            .byte %00111100     ; Alien #2, associated to ch. 1 (normally A)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01100110
            .byte %00000000

            ALIEN3 = 2
            .byte %10000001     ; Alien #3, associated to ch. 2 (normally B)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %11000011
            .byte %00100100

            ALIEN4 = 3
            .byte %10000001     ; Alien #4, associated to ch. 3 (normally C)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01000010
            .byte %10000001

            CANNON = 4
            .byte %00010000     ; Cannon, associated to ch. 4 (normally D)
            .byte %00111000
            .byte %00111000
            .byte %00111000
            .byte %00111000
            .byte %01111100
            .byte %11101110
            .byte %11000110

            EMPTY = 5
            .byte %00000000     ; Blank char, ch. 5 (E)
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000

            BOMB = 6
            .byte %00000000     ; Bomb, associated to ch. 6 (normally F)
            .byte %00000000
            .byte %00000000
            .byte %00011000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000

            BLOCK = 7
            .byte %11111111     ; Block, ch. 7 (normally G)
            .byte %11111111
            .byte %11111111
            .byte %11111111
            .byte %11111111
            .byte %11111111
            .byte %11111111
            .byte %11111111

            BLOCKL = 8
            .byte %00000000     ; Block, ch. 8 (normally H)
            .byte %00000000
            .byte %00000000
            .byte %00000111
            .byte %00011111
            .byte %00111111
            .byte %01111111
            .byte %01111111

            BLOCKR = 9
            .byte %00000000     ; Block, ch. 9 (normally I)
            .byte %00000000
            .byte %00000000
            .byte %11100000     
            .byte %11111000
            .byte %11111100
            .byte %11111110
            .byte %11111110

            SHOT = 10
            .byte %00010000     ; Block, ch. 10 (normally L)
            .byte %00010000
            .byte %00010000
            .byte %00010000
            .byte %00000000
            .byte %00010000
            .byte %00000000
            .byte %00010000
            LASTCH = SHOT

            