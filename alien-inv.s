;         V I C   A L I E N - I N V A D E R S
;
;             by Davide Bucci, April-May 2018
;
; This program is a Space-Invaders clone that runs on an unexpanded VIC-20
;
; The objective is not to reproduce perfectly the original game, but more to
; propose something very playable and quite fast on the VIC-20. Arcadia has been
; inspiring, even if the gameplay is very different from Space Invaders.
;
; FUTURE MODE ON (for the moment everything is still character-based)
; A certain amount of work has been done to ensure that the graphics is smooth
; enough so that the effect hopefully appears to be quite polished.
; It is my first attempt to write a complete game in 6502 assembly language,
; even if I learnt that language many years ago and I used it mainly as a
; complement for BASIC programs.
; FUTURE MODE OFF
;
; The assembler used is ca65
;
; The bombs drop by aliens are described by four arrays:
; BombSpeed, BombPosX, BombPosY and BombPosOY. Their name should be quite
; self-descriptive, except for BombPosOY, that contains the old position of the
; bombs (Y coordinate, as bombs fall vertically) and is used for erasing the
; bombs when they are to be drawn at the new coordinate. Speed is positive for
; bombs falling.
;
; The cannon shots operates with a very similar principle with respect to bombs
; and are described by FireSpeed, FirePosX, FirePosY and FirePosOY. The only
; difference is that a positive speed means that shots move upwards.
;
; A speed of $FF (or 255 in decimal) means that the bomb is exploding and
; should be destroyed; i.e. erased from the screen and then deactivated, by
; putting a final speed of 0.


; Difficulty-related constants
        BOMBPROB = $F0      ; Higher = less bombs falling
        PERIODS = 20        ; Initial difficulty (less=faster)

; General constants
        NMBOMBS = 8         ; Maximum number of bombs falling at the same time
        NMSHOTS = 2         ; Maximum number of cannon shots at the same time

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
            jsr Init        ; Init the game (load graphic chars, etc...)
@restart:   jsr StartGame   ; Set the starting values of game variables
@mainloop:  jsr GETIN       ; Main loop waiting for keyboard events
            beq @mainloop
            sta keyin
            cmp #$0D        ; Wait for return if the game stopped
            bne @norestart
            lda Win
            cmp #$00        ; If the game has stopped, restart
            bne @restart
@norestart: lda keyin
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
@continue1: lda keyin
            cmp #$5A        ; Z: decrease position of the cannon
            bne @continue2
            lda CannonPos
            sec
            sbc #$08
            bcc @continue2
            sta CannonPos
@continue2: lda keyin
            cmp #$20        ; Space: fire!
            bne @continue3
            jsr CannonShoot
@continue3: jmp @mainloop


CannonShoot:
            ldx #0          ; Search for the first free shot
@search:    lda FireSpeed,X ; (i.e. whose speed = 0)
            beq @found
            inx
            cpx #NMSHOTS
            bne @search
            rts             ; No enough shots allowed in parallel. Abort fire.
@found:     lda CannonPos
            lsr
            lsr
            lsr
            sta FirePosX,X  ; Put the actual cannon position in the X coord.
            lda #30         ; Shoot from the last line
            sta FirePosY,X
            lda #1
            sta FireSpeed,X
            rts

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
            sei             ; Configure the interrupt handler
            lda #<IrqHandler
            sta $0314
            lda #>IrqHandler
            sta $0315
            cli
            lda #PERIODS
            sta Period
            rts

StartGame:
            sei
            lda #34
            sta AlienPosY   ; Initial position of aliens
            sta AlienCurrY
            jsr DrawAliens
            lda #$FF
            sta AliensR1s
            sta AliensR2s
            sta AliensR3s
            lda #64
            sta CannonPos   ; Initial position of the cannon
            lda #$FF
            sta OldCannonP
            lda #$00
            sta Win
            sta IrqCn
            sta AlienPosX
            ldx #NMBOMBS    ; Clear all bombs
@loopg:     sta BombSpeed-1,X
            lda #$FF
            sta BombPosOY-1,X
            lda #$00
            dex
            bne @loopg
            ldx #NMSHOTS    ; Clear all shoots
@loopp:     sta FireSpeed-1,X
            dex
            bne @loopp
            lda #EMPTY      ; Clear the screen
            jsr CLS
            lda #BLACK
            jsr PaintColour
            jsr DrawShield
            cli
            rts

; Put zero in the current score

ZeroScore:  lda #$00
            sta Score
            sta Score+1
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
            inx
            jsr DrawChar
            ldx #5
            jsr DrawChar
            inx
            jsr DrawChar
            ldx #9
            jsr DrawChar
            inx
            jsr DrawChar
            ldx #13
            jsr DrawChar
            inx
            jsr DrawChar
            ldx #1
            ldy #28
            lda #BLOCKL
            jsr DrawChar
            inx
            lda #BLOCKR
            jsr DrawChar
            ldx #5
            lda #BLOCKL
            jsr DrawChar
            inx
            lda #BLOCKR
            jsr DrawChar
            ldx #9
            lda #BLOCKL
            jsr DrawChar
            inx
            lda #BLOCKR
            jsr DrawChar
            ldx #13
            lda #BLOCKL
            jsr DrawChar
            inx
            lda #BLOCKR
            jsr DrawChar
            rts

draw1l:
            lda Score       ; Load the current score and convert it to BCD
            sta Val
            lda Score+1
            sta Val+1
            jsr Bin2BCD
            ldx #0
            ldy #0
            lda #WHITE
            sta Colour
            lda Res+2       ; Print all the BCD chars
            jsr PrintBCD
            lda Res+1
            jsr PrintBCD
            lda Res
            jsr PrintBCD
            lda #(48+$80)   ; Write a zero, to multiply x10 the score
            jsr DrawChar
            inx
            inx
            lda Score       ; Update the high score if needed
            cmp HiScore
            bcc @noupdate
            lda Score+1
            cmp HiScore+1
            bcc @noupdate
            jsr UpdateHiSc  
@noupdate:  lda HiScore     ; Load the current hi score and convert it to BCD
            sta Val
            lda HiScore+1
            sta Val+1
            jsr Bin2BCD
            ldx #08
            ldy #00
            lda #CYAN
            sta Colour
            lda Res+2       ; Print all the BCD chars
            jsr PrintBCD
            lda Res+1
            jsr PrintBCD
            lda Res
            jsr PrintBCD
            lda #(48+$80)   ; Write an additional zero
            jsr DrawChar
            rts

UpdateHiSc: lda Score       ; Update the high score
            sta HiScore
            lda Score+1
            sta HiScore+1
            rts

; Copy the graphic chars. They are subjected to be changed during the pixel-by
; pixel movement, so that routine gives only the initial situation.

MovCh:
            ldx #(LASTCH+1)*8+1
@loop:      lda DefChars-1,x
            sta GRCHARS1-1,x
            dex
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

IrqHandler: pha
            txa             ; Save registers
            pha
            tya
            pha
            lda Win         ; If Win !=0 stop the game
            cmp #$00
            beq @contirq
            jmp @exitirq
@contirq:   lda IrqCn
            cmp Period      ; Exercute every PERIOD/60 of second
            bne @cont3
            lda #0
            sta IrqCn
            jsr draw1l
            lda #EMPTY      ; Erase aliens in the current position
            sta AlienCode1
            sta AlienCode2
            sta AlienCode3
            jsr DrawAliens  ; Redraw aliens
            jsr FallBombs   ; Make bombs fall. Aliens will be on top of bombs
            lda Win         ; If Win !=0 stop the game
            cmp #$00
            bne @exitirq
            inc AlienPosY   ; Increment the Y position of the aliens
            lda Direction   ; Increment or decrement the X position,
            and #$01        ; depending on the Direction value
            beq @negative
@positive:  inc AlienPosX   ; The postion should be increased
            lda AlienMaxX   ; Check if the direction should be reversed
            cmp #14
            bcc @cont
            lda Direction   ; Invert the direction
            eor #$FF
            sta Direction
            jmp @cont
@negative:  dec AlienPosX   ; The position should be decreased
            lda AlienMinX   ; Check if the direction should be reversed
            cmp #2
            bcs @cont
            lda Direction   ; Invert the direction
            eor #$FF
            sta Direction
@cont:      lda AlienPosY   ; Check if the aliens came to bottom of screen
            cmp #28*8
            bne @draw
            jsr GameOver    ; In this case, the game is finished
@draw:      lda #ALIEN1
            sta AlienCode1
            lda #ALIEN2
            sta AlienCode2
            lda #ALIEN3
            sta AlienCode3
            jsr DrawAliens
@cont3:     lda CannonPos   ; Check if the cannon position has changed
            cmp OldCannonP
            beq @nochange
            jsr ClearCannon
            lda CannonPos   ; Update the OldCannonP value to the current pos.
            sta OldCannonP
@nochange:  jsr DrawCannon
            jsr MoveShoots  ; Update the position of cannon shots
            inc IrqCn
@exitirq:   pla             ; Restore registers
            tay
            pla
            tax
            pla
            jmp $EABF       ; Jump to the standard IRQ handling routine

; Draw the cannon on the screen, at the current position, contained in
; CannonPos (in pixels).

DrawCannon: lda CannonPos
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
            rts

; Draw aliens on the screen. They are several lines with at most 8 aliens
; each. The presence of an alien in the first row is given by bits in the
; AliensR1 byte. An alien is present at the beginning of the game (or level)
; and can be destroyed when hit. In this case, the corresponding bit in the
; AliensR1 byte is set to 0. Same for AliensR2 and AliensR3.

DrawAliens: lda #$ff
            sta AlienMinX
            lda #$00
            sta AlienMaxX
            lda AliensR1s
            sta AliensR1
            lda AliensR2s
            sta AliensR2
            lda AliensR3s
            sta AliensR3
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
            inc AlienCurrY
            ldx #8*2
@loop3:     dex
            ldy AlienCurrY
            lda AliensR3
            clc
            rol
            sta AliensR3
            bcs @drawAlien2
@ret3:      dex
            bne @loop3
            rts

@drawAlien0:
            lda #RED
            sta Colour
            txa
            pha
            clc
            adc AlienPosX
            tax
            jsr UpdMinMax
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
            jsr UpdMinMax
            lda AlienCode2
            jsr DrawChar
            pla
            tax
            jmp @ret2

@drawAlien2:
            lda #GREEN
            sta Colour
            txa
            pha
            clc
            adc AlienPosX
            tax
            jsr UpdMinMax
            lda AlienCode3
            jsr DrawChar
            pla
            tax
            jmp @ret3

; Update the minimum and maximum value of aliens' positions
; X contains the current horisontal position

UpdMinMax:  cpx AlienMinX
            bcs @nomin
            stx AlienMinX
@nomin:     cpx AlienMaxX
            bcc @nomax
            stx AlienMaxX
@nomax:     rts

; Control the movement of the bullet/laser shot fired by the cannon.

MoveShoots: ldx #0              ; Update the position of the shot
@loop:      lda FireSpeed,X     ; Check if the shot is active (speed!=0)
            beq @cont
            lda FirePosY,X
            sec                 ; If speed >0, update current Y position
            sbc FireSpeed,X     ; The movement is vertical, subtract
            cmp #1              ; Check if we reached the top of the screen
            bcs @stillf
            lda #$FF            ; In this case, destroy the bomb
            sta FireSpeed,X
@stillf:    sta FirePosY,X
@cont:      inx
            cpx #NMSHOTS
            bne @loop
DrawShots:  ldx #0              ; Draw shots
            lda #YELLOW         ; Colour of the shots
            sta Colour
loop4:      stx tmpindex
            lda FirePosX,X
            sta tmpx            ; Store the X position of the shot
            lda FirePosOY,X
            cmp FirePosY,X      ; Check if the shot should be redrawn
            beq notmove
            tay
            lda #EMPTY          ; Erase the previous shot
            ldx tmpx            ; Load the X position
            jsr DrawChar        ; Erase the shot in the old position
            ldx tmpindex
            lda FireSpeed,X     ; Do not draw inactive shots
            cmp #0
            beq notmove
            cmp #$FF
            bne @normal
            lda #0
            sta FireSpeed,X
@normal:    lda FirePosY,X
            sta tmpy
            sta FirePosOY,X     ; Save the current position
            tay
            ldx tmpx
            jsr GetChar         ; Check for a collision
            cmp #EMPTY
            bne collision
            lda #SHOT
            jsr DrawChar        ; Draw the shot in the new position
notmove:    ldx tmpindex
            inx
            cpx #NMSHOTS
            bne loop4
            rts

; Here we know that a collision took place, so we should see what element has
; been encountered by the bullet. A contains the character that has been found

collision:  cmp #ALIEN1
            beq alienshot
            cmp #ALIEN2
            beq alienshot
            cmp #ALIEN3
            beq alienshot
            cmp #ALIEN4
            beq alienshot
            cmp #BLOCK
            beq bunkershot
            cmp #BLOCKR
            beq bunkershot
            cmp #BLOCKL
            beq bunkershot
            jsr CheckWin
            jmp notmove

; Handle the different collisions.
; X and Y contain the position of the collision, also available in tmpx and
; tmpy respectively
bunkershot:
            lda #EXPLOSION1
            jsr DrawChar
            lda #$FF
            ldx tmpindex
            sta FireSpeed,X
            jmp notmove

alienshot:  lda Score
            clc
            adc #$01
            sta Score
            bcc @contadd
            lda Score+1
            clc
            adc #$01
            sta Score+1
@contadd:   txa
            sec
            sbc AlienPosX
            lsr
            tax
            lda #$01
            cpx #$00
            beq @r1
@contsh:    asl
            dex
            bne @contsh
@r1:        pha                 ; A contains the code to XOR to the aliens line
            lda AlienPosY
            lsr
            lsr
            lsr
            sta tmp4
            pla
            cpy tmp4
            bne @l2
            eor AliensR1s       ; kill aliens with XOR!    ;-)
            sta AliensR1s
            jmp @follow
@l2:        inc tmp4
            inc tmp4
            cpy tmp4
            bne @l3
            eor AliensR2s       ; Add here for more than three lines of aliens
            sta AliensR2s
            jmp @follow
@l3:        eor AliensR3s       ; Add here for more than three lines of aliens
            sta AliensR3s
@follow:    ldx tmpx
            ldy tmpy
bombshot:   lda #EXPLOSION1
            jsr DrawChar
            lda #$00
            ldx tmpindex
            sta FireSpeed,X
            jmp notmove

; Check if the player won the game.

CheckWin:   lda AliensR1s       ; Check if all aliens have been destroyed
            cmp #$00
            bne @exit
            lda AliensR2s
            cmp #$00
            bne @exit
            lda AliensR3s
            cmp #$00
            bne @exit
            lda #$FF            ; If we come here, all aliens have been shot
            sta Win             ; That will stop the game
            ldx #4              ; write "YOU WON"
            ldy #15
            lda #YELLOW
            sta Colour
            lda #(25+$80)       ; Y
            jsr DrawChar
            inx
            lda #(15+$80)       ; O
            jsr DrawChar
            inx
            lda #(21+$80)       ; U
            jsr DrawChar
            inx
            lda #(32+$80)       ;
            jsr DrawChar
            inx
            lda #(23+$80)       ; W
            jsr DrawChar
            inx
            lda #(15+$80)       ; O
            jsr DrawChar
            inx
            lda #(14+$80)       ; N
            jsr DrawChar
            lda Period          ; Decrease Period (increase alien speed)
            sec
            sbc #$02
            bcc @exit           ; Avoid having "negative" speeds
            sta Period
@exit:      rts

; Game over! Zero the score, turn the screen to red and write "GAME OVER"

GameOver:   lda #$FF
            sta Win             ; Stop the game
            jsr ZeroScore       ; Put the score to zero
            lda #RED            ; Put all the screen in red (sooo bloody!)
            sta Colour
            jsr PaintColour
            ldx #2              ; write "GAME OVER"
            ldy #15
            lda #(7+$80)        ; G
            jsr DrawChar
            inx
            lda #(1+$80)        ; A
            jsr DrawChar
            inx
            lda #(13+$80)       ; M
            jsr DrawChar
            inx
            lda #(5+$80)        ; E
            jsr DrawChar
            inx
            lda #(32+$80)       ;
            jsr DrawChar
            inx
            lda #(15+$80)       ; O
            jsr DrawChar
            inx
            lda #(22+$80)       ; V
            jsr DrawChar
            inx
            lda #(5+$80)        ; E
            jsr DrawChar
            inx
            lda #(18+$80)       ; R
            jsr DrawChar
            lda #PERIODS
            sta Period
@exit:      rts

; Control bombs dropping. A maximum of 8 bombs can be falling at the same
; time. A bomb is active and falling if its speed is greater than 0.
; BombSpeed, BombPosX and BombPosY are the arrays containing the speed and the
; positions. Exploit tmpindex and tmpx, change registers A, X, Y.
; This routine does three things:
;
; 1 - For each alien alive, and for each of the 8 bombs available, decide if
;     a bomb is dropped by drawing a random number and check if it is inside
;     a given interval
; 2 - Update the positions of the bombs active in the screen and check for
;     collisions.
; 3 - Draw the bombs in the new positions on the screen.
;

FallBombs:  lda AliensR1s
            sta AliensR1
            lda AliensR2s
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
            clc                 ; If speed !=0, update current Y position
            adc BombPosY,X
            cmp #31             ; Check if we reached the last line
            bcc @stillf
            lda #$FF            ; In this case, destroy the bomb
            sta BombSpeed,X
@stillf:    sta BombPosY,X
@cont:      inx
            cpx #NMBOMBS
            bne @loop3

DrawBombs:  ldx #0              ; Draw bombs
            lda #MAGENTA        ; Colour of the bombs
            sta Colour
@loop4:     stx tmpindex
            lda BombSpeed,X     ; Do not draw inactive bombs
            cmp #0
            beq @notmove
            lda BombPosX,X
            sta tmpx            ; Store the X position of the bomb
            lda BombPosOY,X
            cmp BombPosY,X
            beq @notmove
            tay
            lda #EMPTY          ; Erase the previous bomb
            ldx tmpx            ; Load the X position
            jsr DrawChar        ; Erase the bomb in the old position
            ldx tmpindex
            lda BombSpeed,X
            cmp #$FF            ; If the speed is #$FF, do not draw the bomb
            bne @normal
            lda #$00
            sta BombSpeed,X
            jmp @notmove
@normal:    lda BombPosY,X
            sta BombPosOY,X     ; Save the current position
            sta tmpy
            tay
            ldx tmpx
            jsr GetChar         ; Check for a collision
            cmp #CANNON
            beq @BombExpl        ; Explode the bomb
            cmp #BLOCK
            beq @BombExpl        ; Explode the bomb
            cmp #BLOCKR
            beq @BombExpl        ; Explode the bomb
            cmp #BLOCKL
            beq @BombExpl        ; Explode the bomb
            lda #BOMB
            jsr DrawChar        ; Draw the bomb in the new position
@notmove:   ldx tmpindex
            inx
            cpx #NMBOMBS
            bne @loop4
            rts

@BombExpl:  cmp #CANNON         ; Check if the cannon has been hit
            bne @explode
            jsr GameOver        ; If yes... player has lost!
            ldx tmpx
            ldy tmpy
@explode:   lda #EXPLOSION1     ; Draw an explosion
            jsr DrawChar
            lda #$FF            ; Delete the bomb
            ldx tmpindex
            sta BombSpeed,X
            jmp @notmove

; Decide if a bomb should be dropped or not.

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
            txa
            asl
            clc
            adc AlienPosX
            adc #$FF
            sta BombPosX,Y
@nobomb:    rts

; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. It uses Dummy1 and Dummy2.
; Colour indicates the colour code of the character. It uses 3 bytes in the
; stack and does not change registers.

DrawChar:   sta Dummy1
            stx Dummy2
            sty Dummy3
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
@exit:      lda Dummy1
            ldx Dummy2
            ldy Dummy3
            rts

; Get the screen code of the character in the X and Y locations.
; The character is returned in A.

GetChar:    stx Dummy2
            sty Dummy3
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
            lda MEMSCR+256,Y
            jmp @exit
@tophalf:   adc Dummy2
            tay
            lda MEMSCR,Y
@exit:      ldx Dummy2
            ldy Dummy3
            rts

; Clear the screen. This maybe is too slow to be used in an interrupt handler.
; Draw everywhere the character contained in the A register. Employs X.

CLS:
            size=16*31/4+1
            ldx #size
@loop:      sta MEMSCR-1,X          ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size-1,X     ; to mess with a 16-bit loop.
            sta MEMSCR+size*2-1,X
            sta MEMSCR+size*3-1,X
            dex
            bne @loop
            rts

; Put the colour code contained in A everywhere in the screen

PaintColour:
            ldx #size
@loop:      sta MEMCLR-1,X
            sta MEMCLR+size-1,X
            sta MEMCLR+size*2-1,X
            sta MEMCLR+size*3-1,X
            dex
            bne @loop
            rts

; Random number generation routine. Adapted from here:
; http://sleepingelephant.com/ipw-web/bulletin/bb/viewtopic.php?t=2304
; Creates a pseudo-random number in Random and Random+1
; Change register A and employs tmpindex

GetRand:    lda Random+1
            sta tmpindex
            lda Random
            asl
            rol tmpindex
            asl
            rol tmpindex
            clc
            adc Random
            pha
            lda tmpindex
            adc Random+1
            sta Random+1
            pla
            clc             ; added this instruction - kweepa
            adc #$11
            sta Random
            lda Random+1
            adc #$36
            sta Random+1
            rts

; Convert a 16-bit word to a 24-bit BCD. Adapted from here:
; http://www.obelisk.me.uk/6502/algorithms.html
; I like how it is compact and the clever use of the BCD mode of the 6502

; Convert an 16 bit binary value into a 24bit BCD value
Bin2BCD:    lda #0          ; Clear the result area
            sta Res+0
            sta Res+1
            sta Res+2
            ldx #16         ; Setup the bit counter
            sed             ; Enter decimal mode
@loop:      asl Val+0       ; Shift a bit out of the binary
            rol Val+1       ; ... value
            lda Res+0       ; And add it into the result, doubling
            adc Res+0       ; ... it at the same time
            sta Res+0
            lda Res+1
            adc Res+1
            sta Res+1
            lda Res+2
            adc Res+2
            sta Res+2
            dex             ; More bits to process?
            bne @loop
            cld             ; Leave decimal mode
            rts

; Print the BCD value in A as two ASCII digits
PrintBCD:   pha             ; Save the BCD value
            lsr A           ; Shift the four most significant bits
            lsr A           ; ... into the four least significant
            lsr A
            lsr A
            clc
            adc #(48+$80)   ; Make an screen code char
            jsr DrawChar
            inx
            pla             ; Recover the BCD value
            and #$0F        ; Mask out all but the bottom 4 bits
            clc
            adc #(48+$80)   ; Make an screen code char
            jsr DrawChar
            inx
            rts

; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA
;
; Data and configuration settings.
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

Random:     .word $0000
Dummy1:     .byte $00           ; Employed in DrawChar
Dummy2:     .byte $00
Dummy3:     .byte $00
IrqCn:      .byte $00
tmpindex:       .byte $00
tmpx:       .byte $00
tmpy:       .byte $00
tmp4:       .byte $00
keyin:      .byte $00           ; Last key typed.
Val:        .word $0000         ; Used for the BCD conversion
Res:        .res 3, $00         ; the result of the BCD conversion

Period:     .byte 20            ; Higher = slower alien movement

Colour:     .byte $00           ; Colour to be used by the printing routines
AliensR1s:  .byte $FF           ; Presence of aliens in row 1
AliensR2s:  .byte $FF           ; Same for row 2
AliensR3s:  .byte $FF           ; Same for row 3
AliensR1:   .byte $FF           ; Presence of aliens in row 1 (temporary)
AliensR2:   .byte $FF           ; Same for row 2 (temporary)
AliensR3:   .byte $FF           ; Same for row 3 (temporary)
AlienCode1: .byte $00           ; Character for alien row 1
AlienCode2: .byte $00           ; Character for alien row 2
AlienCode3: .byte $00           ; Character for alien row 3
AlienPosX:  .byte $00           ; Horisontal position of aliens (in pixels)
AlienMaxX:  .byte $00
AlienMinX:  .byte $00
AlienPosY:  .byte $00           ; Vertical position of aliens (in pixels)
AlienCurrY: .byte $00           ; Vertical position of alien being drawn
Direction:  .byte $00           ; The first bit indicates aliens' X direction
CannonPos:  .byte $8*8          ; Horisontal position of the cannon (in pixels)
OldCannonP: .byte $00           ; Old position of the cannon
Win:        .byte $00           ; If 1, the level is won. If $FF, game over
Score:      .word $0000         ; Current score (divided by 10)
HiScore:    .word $0000         ; High Score (divided by 10)

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
            .byte %11111110
            .byte %11111110

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

            EXPLOSION1=11
            .byte %01000000     ; Block, ch. 11 (normally M)
            .byte %10010010
            .byte %01000100
            .byte %0011100
            .byte %10011010
            .byte %10110001
            .byte %01100011
            .byte %10000001
            
            LASTCH = EXPLOSION1

            