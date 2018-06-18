;         V I C   A L I E N - I N V A D E R S
;
;             by Davide Bucci, April-June 2018
;
; This program is a Space-Invaders clone that runs on an unexpanded VIC-20
;
; The objective is not to reproduce perfectly the original game, but more to
; propose something very playable and quite fast on the VIC-20. Arcadia has been
; inspiring, even if its gameplay is very different from Space Invaders.
;
; A certain amount of work has been done to ensure that the graphics is smooth
; enough so that the effect hopefully appears to be quite polished.
; It is my first attempt to write a complete game in 6502 assembly language,
; even if I learnt that language many years ago and I used it mainly as a
; complement for BASIC programs.
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
;
; Plenty of things are done during the IRQ handling routine, synchronized with
; the PAL refresh rate of the monitor. For this reason, expect a lot of flicker 
; in the aliens when this game is played on a NTSC machine.


; Difficulty-related constants
        BOMBPROB = $F0      ; Higher = less bombs falling
        SHIPPROB = $F5      ; Higher = less mother ships
        PERIODS = 5         ; Initial difficulty (less=faster)

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
        MULTICOLOUR = $08

; KERNAL routines used
        GETIN = $FFE4

; Page-0 addresses used (for indirect indexed addressing and other things)

        LAB_01 = $01
        LAB_02 = $02
        LAB_03 = $03
        LAB_04 = $04
        LAB_05 = $05
        LAB_06 = $06
        LAB_07 = $07
        LAB_08 = $08
        LAB_09 = $09
        LAB_0A = $0A
        LAB_0B = $0B

        CharCode = LAB_03   ; Employed in DrawChar
        PosX = LAB_04
        PosY = LAB_05
        Colour = LAB_06     ; Colour to be used by the printing routines
        tmpindex = LAB_07   ; Temporary variables
        tmpx = LAB_08
        tmpy = LAB_09
        tmp4 = LAB_0A
        SPRITECH  = $0C     ; Pointer to the group of 4 ch. for a sprite (word)
        CHRPTR    = $0E     ; Pointer to the original ch. in a sprite (word)
        SpriteX   = $10     ; X position (offset in a char) of a sprite (byte)
        SpriteY   = $11     ; Y position (offset in a char) of a sprite (byte)
        CharShr   = $12     ; Employed in LoadSprite
        AlienPosYc= $13     ; Vertical alien position (in characters)
        PixPosX   = $14     ; Position in characters
        ColourRead= $15     ; Colour read by GetChar
        POSCHARPT = $1A     ; Pointer for a character in memory (word)
        POSCOLPT  = $1C     ; Pointer for a colour in memory (word)
        Bombcntr  = $1E     ; Counter for bomb interrupts
        PixPosXO  = $1F     ; Old Position in characters
        TmpScan   = $20     ; Used in raster line sync code
        Random    = $21     ; Position where to store a random word by GetRand
        IrqCn     = $23     ; Counter for interrupt
        keyin     = $24     ; Last key typed.
        Val       = $25     ; Used for the BCD conversion (word)
        Res       = $27     ; The result of the BCD conversion (3 bytes)
        Joystick  = $2A     ; Different from zero if the joystick was used
        MotherPos = $2B     ; Position of the mother ship ($FD: no ship)
        Period    = $2C     ; Higher = slower alien movement
        MotherCntr= $2D     ; Counter for slowing down mother ship
        AliensR1s = $2E     ; Presence of aliens in row 1
        AliensR2s = $2F     ; Same for row 2
        AliensR3s = $30     ; Same for row 3
        AliensR   = $31     ; Presence of aliens in row (temporary)
        AliensR1  = $32     ; Presence of aliens in row 1 (temporary)
        AliensR2  = $33     ; Same for row 2 (temporary)
        AliensR3  = $34     ; Same for row 3 (temporary)
        AlienCode1= $35     ; Character for alien row 1
        AlienCode2= $36     ; Character for alien row 2
        AlienCode3= $37     ; Character for alien row 3
        AlienPosX = $38     ; Horizontal position of aliens (in pixels)
        AlienMaxX = $39     ; Maximum position in X of aliens (in characters)
        AlienMinX = $3A     ; Minimum position in X of aliens (in characters)
        AlienPosY = $3B     ; Vertical position of aliens (in pixels)
        AlienPosYO= $3C     ; Previous (old) vertical position of aliens (px)
        AlienCurrY= $3D     ; Vertical (temp) position of alien being drawn (ch)
        Direction = $3E     ; The first bit indicates aliens' X direction
        CannonPos = $3F     ; Horizontal position of the cannon (in characters)
        OldCannonP= $40     ; Old position of the cannon
        Win       = $41     ; If 1, the level is won. If $FF, game over
        Score     = $42     ; Current score (divided by 10) (word)
        HiScore   = $44     ; High Score (divided by 10) (word)
        BombPeriod= $46     ; Period for updating bomb positions
        Level     = $47     ; Current level
        tmpp      = $48     ; Temp for bomb dropping position
        ExplMXpos = $49     ; Hor. position (ch) of the explosion on mother sh.
        ExplMCnt  = $4A     ; Mother ship explosion counter
    
        INITVALC=$ede4

; VIC-chip addresses
        VICSCRHO = $9000    ; Horizontal position of the screen
        VICSCRVE = $9001    ; Vertical position of the screen
        VICCOLNC = $9002    ; Screen width in columns and video memory addr.
        VICROWNC = $9003    ; Screen height, 8x8 or 8x16 chars, scan line addr.
        VICRAST  = $9004    ; Bits 8-1 of the current raster line
        VICCHGEN = $9005    ; Character gen. and video matrix addresses.
        GEN1     = $900A    ; First sound generator
        GEN2     = $900B    ; Second sound generator
        GEN3     = $900C    ; Third sound generator
        NOISE    = $900D    ; Noise sound generator
        VOLUME   = $900E    ; Volume and additional colour info
        VICCOLOR = $900F    ; Screen and border colours

        PORTAVIA1 = $9111   ; Port A 6522 (joystick)
        PORTAVIA1d = $9113  ; Port A 6522 (joystick)
        PORTBVIA2 = $9120   ; Port B 6522 2 value (joystick)
        PORTBVIA2d = $9122  ; Port B 6522 2 direction (joystick

        MEMSCR   = $1E00    ; Start address of the screen memory (unexp. VIC)
        MEMCLR   = $9600    ; Start address of the colour memory (unexp. VIC)

        REPEATKE = $028A    ; Repeat all keys

        VOICE1  = GEN1      ; Voice 1 for music
        VOICE2  = GEN2      ; Voice 2 for music
        EFFECTS = GEN3      ; Sound effects (not noise)

.export main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

; Main program here.

main:       jsr Init        ; Init the game (load graphic chars, etc...)
restart:    jsr StartGame   ; Set the starting values of game variables
mainloop:   lda Joystick
            beq @ccc
            jsr ShortDelay
            lda #$00
            sta Joystick
@ccc:       jsr GETIN       ; Main loop waiting for keyboard events
            sta keyin
            lda #$ff
            sta Joystick
            lda PORTAVIA1
            and #%00010000  ; Left
            beq left
            lda PORTBVIA2
            and #%10000000  ; Right
            beq right
            lda PORTAVIA1
            and #%00100000  ; Fire
            beq fire
            lda #$00
            sta Joystick
            lda keyin
            beq mainloop
            cmp #$0D        ; Wait for return if the game stopped
            bne @norestart
            lda Win         ; If the game has stopped, restart
            bne restart
@norestart: lda keyin
            cmp #$58        ; X: increase position of the cannon (right)
            beq right
            cmp #$5A        ; Z: decrease position of the cannon (left)
            beq left
            cmp #$20        ; Space: fire!
            beq fire
            cmp #$4D        ; M toggle music on/off
            bne mainloop
            lda VoiceBase
            eor #$80
            sta VoiceBase
@continue4: jmp mainloop

right:      inc CannonPos
            lda CannonPos
            cmp #15
            bcc @continue
            lda #15
            sta CannonPos
@continue:  jmp mainloop

left:       dec CannonPos
            bmi @zeroc
            jmp mainloop
@zeroc:     lda #0
            sta CannonPos
            jmp mainloop

fire:       lda Win         ; If the game has stopped, restart
            bne restart
            jsr CannonShoot
            jmp mainloop

CannonShoot:
            ldx #0          ; Search for the first free shot
@search:    lda FireSpeed,X ; (i.e. whose speed = 0)
            beq @found
            inx
            cpx #NMSHOTS
            bne @search
            rts             ; No enough shots allowed in parallel. Abort fire.
@found:     lda CannonPos
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
            lda INITVALC
            cmp #$05
            beq CenterScreenNTSC    ; Load the screen settings
            bne CenterScreenPAL
ContInit:   sty VICSCRVE    ; Centre the screen vertically...
            stx VICSCRHO    ; ... and horizontally
            lda #$FF        ; Move the character generator address to $1C00
            sta VICCHGEN    ; while leaving ch. 128-255 to their original pos.
            jsr MovCh       ; Load the graphic chars
            sei             ; Configure the interrupt handler
            
            ; Data for PAL machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES = 312
            CYCLES_PER_LINE = 71
            TIMER_VALUE = LINES * CYCLES_PER_LINE - 2
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #112
            bne @loopsync
            lda #<TIMER_VALUE
            ldx #>TIMER_VALUE
            stx $9125       ; Set up the timer
            sta $9126
            lda #<IrqHandler; And the IRQ handler
            sta $0314
            lda #>IrqHandler
            sta $0315
            cli
            lda #$0         ; Prepare joystick
            sta Level
            sta Score
            sta Score+1
            sta HiScore
            sta HiScore+1
            sta PORTAVIA1d
            lda #$7F
            sta PORTBVIA2d
            rts

; Screen init value for PAL and NTSC

CenterScreenPAL:
            ldx #$12        
            ldy #$16        
            jmp ContInit

CenterScreenNTSC:
            ldx #$0A        
            ldy #$6        
            jmp ContInit

StartGame:
            sei
            ;lda #<EndMemMrk ; DEBUG
            ;sta HiScore     ; DEBUG
            ;lda #>EndMemMrk ; DEBUG
            ;sta HiScore+1   ; DEBUG
            
            lda #$2F        ; Turn on the volume, set multicolour add. colour 2
            sta VOLUME
            lda #17
            sta AlienPosY   ; Initial position of aliens
            jsr DrawAliens
            lda #$FD        ; No mother ship visible
            sta MotherPos
            lda #$FF
            sta AlienPosYO
            sta AliensR1s
            sta AliensR2s
            sta AliensR3s
            sta OldCannonP
            lda #8
            sta CannonPos   ; Initial position of the cannon
            lda #$00
            sta Direction
            sta Win
            sta IrqCn
            sta NOISE
            sta AlienPosX
            jsr ConfLevel
            ldx #NMBOMBS    ; Clear all bombs
@loopg:     LDA #$0
            sta BombSpeed-1,X
            lda #$FF
            sta BombPosOY-1,X
            lda #EMPTY
            dex
            bne @loopg
            ldx #NMSHOTS    ; Clear all shoots
@loopp:     lda #$00
            sta FireSpeed-1,x
            sta FirePosX-1,x
            sta FirePosY-1,x
            sta FirePosOY-1,x
            sta FireColOver-1,X
            lda #EMPTY
            sta FireChOver-1,X
            dex
            bne @loopp
            lda #EMPTY      ; Clear the screen
            jsr CLS
            lda #BLACK
            jsr PaintColour
            jsr DrawShield
            lda #32
            bit VoiceBase
            bpl @musicok
            lda #$A0
@musicok:   sta VoiceBase
            jsr draw1l
            cli
            rts

; Put zero in the current score

ZeroScore:  lda #$00
            sta Score
            sta Score+1
            rts

; Configure the level (in Level)

ConfLevel:  ldx Level
            cpx #NUMLEVEL-1
            bmi @validlevel
            ldx #NUMLEVEL-1
            stx Level
@validlevel:lda LevelsBomb,x
            sta BombPeriod
            lda LevelsPer,x
            sta Period
            rts

; Draw the shields in the following positions:
; ------------1---
; 0---4---8---2---
;  **  **  **  **

DrawShield: ldx #1
            ldy #29
            lda Period
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
            jmp DrawChar

draw1l:
            lda Score       ; Load the current score and convert it to BCD
            sta Val
            lda Score+1
            sta Val+1
            jsr Bin2BCD
            lda #WHITE
            ldx #0
            jsr PrintRes
            lda #(48+$80)   ; Write a zero, to multiply x10 the score
            jsr DrawChar
            lda Score+1
            cmp HiScore+1
            bcc @nou
            beq @nou
            jsr UpdateHiSc
@nou:       lda Score       ; Update the high score if needed
            cmp HiScore
            bcc @noupdate
            jsr UpdateHiSc
@noupdate:  lda HiScore     ; Load the current hi score and convert it to BCD
            sta Val
            lda HiScore+1
            sta Val+1
            jsr Bin2BCD
            ldx #09
            lda #CYAN
            jsr PrintRes
            lda #(48+$80)   ; Write an additional zero
            jmp DrawChar

UpdateHiSc: lda Score       ; Update the high score
            sta HiScore
            lda Score+1
            sta HiScore+1
            rts

PrintRes:   sta Colour
            ldy #0
            lda Res+2       ; Print all the BCD chars
            jsr PrintBCD
            lda Res+1
            jsr PrintBCD
            lda Res
            jsr PrintBCD
            rts

; Copy the graphic chars. They are subjected to be changed during the pixel-by
; pixel movement, so that routine moves only the characters not used as sprites

MovCh:      ldx #(LASTCH+1)*8+1
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
; 6 - Call the sound drivers
; 7 - Jump to the original IRQ handler (for scanning the keyboard, etc).
;
; The user interface is handled outside of the interrupt, in the main program
; loop and the communication with the IRQ handler is made by a set of
; appropriate flags. This approach has the following advantages:
;
; - The speed of the aliens and of the cannon is controlled very precisely as
;   the IRQ handler is called at a predictable and stable rate.
; - The code for the visualization and for the user interface (i.e. recognizing
;   keyboard and joystick movements) is kept separate.
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
@still:     ; lda VICRAST
;             bne @still
;             lda #09         ; DEBUG
;             sta VICCOLOR    ; DEBUG
            ldx #$00
            lda MotherPos   ; If the mother ship is present, do not mute
            cmp #$FD        ; sound effects
            bne @nomute     ; If the mother ship is present, do not mute SFX
            stx EFFECTS
@nomute:    lda ExplMCnt
            beq @noerMexp
            dec ExplMCnt
            bne @noerMexp
            ldx ExplMXpos
            ldy #1
            lda #EMPTY
            jsr DrawChar
@noerMexp:  bit Win         ; If Win <0 stop the game
            bpl @contirq
            jmp @exitirq
@contirq:   lda IrqCn
            cmp Period      ; Execute every PERIOD/60 of second
            beq @contint
            jmp @cont3
@contint:   ldx #$00
            stx IrqCn
            stx NOISE
            lda AlienPosY
            pha
            lsr
            lsr
            bit VoiceBase   ; If music is active, VoiceBase is linked to the
            bmi @nomusic    ; vertical alien position (music becomes higher
            sta VoiceBase   ; pitched when aliens scroll down).
@nomusic:   lsr
            sta AlienPosYc  ; Calculate Y pos of aliens in characters
            pla
            and #7
            sta SpriteY     ; Calculate Y shift in pixel
            jsr EraseAliens
            jsr FallBombs   ; Make bombs fall. Aliens will be on top of bombs
            bit Win         ; If Win <0 stop the game
            bmi @exitirq
            bit Direction   ; Increment or decrement the X position,
            bmi @negative   ; depending on the Direction value
@positive:  inc AlienPosX   ; The position should be increased
            jmp @cont
@negative:  dec AlienPosX   ; The position should be decreased
@cont:      ldx AlienPosY   ; Check if the aliens came to bottom of screen
            lda AliensR3s
            bne @comp
            inx
            inx
            lda AliensR2s
            bne @comp
            inx
            inx
@comp:      cpx #25*8
            bne @draw
            jsr GameOver    ; In this case, the game is finished
@draw:      lda PixPosX
            ror
            bcs @altaliens
            ldx #ALIEN1
            ldy #ALIEN3
            bcc @normal
@altaliens: ldx #ALIEN2
            ldy #ALIEN4
@normal:    stx AlienCode1
            sty AlienCode2
            jsr DrawAliens
            lda AlienMaxX   ; Check if the direction should be reversed
            cmp #15
            bmi @cont2
            lda #128
            sta Direction   ; Invert the direction
            inc AlienPosY   ; Increment the Y position of the aliens
@cont2:     lda AlienMinX   ; Check if the direction should be reversed
            cmp #1
            bcs @cont3      ; Check for the pixel position too
            lda SpriteX
            bne @cont3
            lda #0
            sta Direction   ; Invert the direction
            inc AlienPosY   ; Increment the Y position of the aliens
@cont3:     lda CannonPos   ; Check if the cannon position has changed
            cmp OldCannonP
            beq @nochange
            jsr ClearCannon ; If yes, redraw it
@nochange:  jsr DrawCannon
            jsr MoveShoots  ; Update the position of cannon shots
            inc IrqCn
            jsr MotherSh    ; Check if we should enter the mother ship
@exitirq:   bit Win
            bmi @nomusic1
            jsr Music1
            jsr Music2
@nomusic1:  pla             ; Restore registers
            tay
            pla
            tax
            pla
            jmp $EABF       ; Jump to the standard IRQ handling routine



; Decide wether the mother ship enters the screen and handle its movement.

MotherSh:   lda MotherPos
            cmp #$FD
            bne @moveship
            lda Random+1        ; Get a random number and check if it is less
            cmp #SHIPPROB       ; than a given threshold
            bcc @exitsh
            lda #$0F
            sta MotherPos
            lda #0
            sta MotherCntr
            rts

@moveship:  ldx MotherCntr
            cpx #3
            beq @move
            inc MotherCntr
            rts

@move:      ldx #0
            stx MotherCntr
            tax
            ldy #$1
            lda #$A0
            clc
            adc MotherPos
            sta EFFECTS
            lda #YELLOW
            sta Colour
            lda #EMPTY      ; Erase the ship in the previous position
            inx
            inx
            jsr DrawChar    ; Erase
            dec MotherPos
            beq @exitsh
            ldx MotherPos   ; Draw the ship
            lda #MOTHER1
            jsr DrawChar
            inx
            lda #MOTHER2
            jsr DrawChar
            inx
            lda #MOTHER3
            jsr DrawChar
@exitsh:    rts

; Draw the cannon on the screen, at the current position, contained in
; CannonPos (in characters).

DrawCannon: lda CannonPos
            sta OldCannonP
            tax
            ldy #30             ; Vertical position of the cannon
            lda #WHITE          ; Cannon in white
            sta Colour
            lda #CANNON         ; Cannon char
            jmp DrawChar

; Clear the cannon on the screen, at the current position, contained in
; OldCannonP (in characters).

ClearCannon:
            ldx OldCannonP
            ldy #30             ; Vertical position of the cannon
            lda #WHITE          ; Cannon in white
            sta Colour
            lda #EMPTY          ; Space
            jmp DrawChar

; Erase aliens. Calculate AlienCurrY (in chars)

EraseAliens:
            ldy AlienPosYc
            dey
            
            lda #<MEMSCR    ; Inlined version of CharPos without colour
            sta POSCHARPT
            lda #>MEMSCR
            sta POSCHARPT+1
            tya
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            bcc @nocorr     ; we need to write in the bottom-half of the screen
            inc POSCHARPT+1
            clc
@nocorr:    adc POSCHARPT
            sta POSCHARPT
            jsr Waitrast
            ldy #16*7
            lda #EMPTY
@loop:      sta (POSCHARPT),y   ; A little bit of loop unrolling
            dey
            sta (POSCHARPT),y
            dey
            sta (POSCHARPT),y
            dey
            sta (POSCHARPT),y
            dey
            sta (POSCHARPT),y
            dey
            sta (POSCHARPT),y
            dey
            sta (POSCHARPT),y
            dey
            sta (POSCHARPT),y
            dey
            bne @loop
            sta (POSCHARPT),y
            rts

; Calculate SpriteX and PixPos from AlienPosX. It is a little tricky, as
; AlienPosX can be negative and some corrections have to be done.

CalcXpos:   lda AlienPosX      ; Sprite generating code starts here
            and #7
            sta SpriteX
            
            lda AlienPosX       ; Calculate the position in characters
            bpl @positive
            eor #$FF
            clc
            adc #1
            pha
            and #7
            sta SpriteX
            pla
@positive:  lsr                 ; Divide by 8
            lsr
            lsr
            sta PixPosX         ; Store it in PixPosX
            lda AlienPosX       ; Calculate the position in characters
            bpl @positive1
            lda #$FF
            sec
            sbc PixPosX
            sta PixPosX
            lda #7
            sec
            sbc SpriteX
            sta SpriteX
@positive1: rts

; Draw aliens on the screen. They are several lines with at most 8 aliens
; each. The presence of an alien in the first row is given by bits in the
; AliensR1 byte. An alien is present at the beginning of the game (or level)
; and can be destroyed when hit. In this case, the corresponding bit in the
; AliensR1 byte is set to 0. Same for AliensR2 and AliensR3.
; The vertical position of the aliens should be in AliensPosY (in pixels) and
; AlienCurrY (in chars) should have been already calculated.

DrawAliens: 
            lda AlienPosYc
            sta AlienCurrY
            jsr CalcXpos
            lda #<(GRCHARS1+SPRITE1A*8)
            sta SPRITECH
            lda #>(GRCHARS1+SPRITE1A*8)
            sta SPRITECH+1

            ;jsr Waitrast

            jsr ClearSprite
            lda AlienCode1
            sta CharCode
            jsr LoadSprite      ; End of sprite gen code
            
            lda #<(GRCHARS1+SPRITE2A*8)
            sta SPRITECH
            lda #>(GRCHARS1+SPRITE2A*8)
            sta SPRITECH+1
            jsr ClearSprite
            lda AlienCode2
            sta CharCode
            jsr LoadSprite

            lda #$ff            ; Reset the min/max positions of aliens in ch.
            sta AlienMinX
            lda #$00
            sta AlienMaxX

            lda AliensR1s       ; Top line of aliens
            sta AliensR
            lda #SPRITE1A      ; AlienCode1
            sta CharCode
            lda #RED
            sta Colour
            jsr AlienLoop

            inc AlienCurrY      ; Second line of aliens
            inc AlienCurrY
            lda #SPRITE2A      ; AlienCode1
            sta CharCode
            lda #CYAN
            sta Colour
            lda AliensR2s
            sta AliensR
            jsr AlienLoop
            
            inc AlienCurrY      ; Third line of aliens
            inc AlienCurrY

            lda #GREEN
            sta Colour
            lda #SPRITE1A      ; AlienCode1
            sta CharCode
            lda AliensR3s
            sta AliensR
            jmp AlienLoop

; PAL-only code here!

Waitrast:   ;lda #9
            ;sta VICCOLOR
Waitrast1:  lda VICRAST   ; Wait if there is a risk of flicker
            lsr
            lsr
            sec
            sta TmpScan
            sbc #20
            bpl @greater
            lda #0
@greater:   cmp AlienPosYc
            bcs @noflicker
            lda TmpScan
            cmp AlienPosYc
            bcc @noflicker
            bcs Waitrast1
@noflicker: ;lda #8
            ;sta VICCOLOR
            rts

; End of PAL-only code

; Draw a line of aliens (the sprite should have been already created)

AlienLoop:  ldy AlienCurrY
            ldx #0
            jsr PosChar
            ldx #8*2
@loop:      dex             ; X contains the alien pos. in the line (in ch.)
            asl AliensR
            bcc @ret
            txa
            clc
            adc PixPosX     ; To be added to the pos. in characters
            tay             ; to obtain the actual position where to draw al.
            dey
            cpy AlienMinX   ; Update the minimum and maximum value of positions
            bcs @nomin
            sty AlienMinX
@nomin:     cpy AlienMaxX
            bcc @nomax
            sty AlienMaxX
@nomax:     lda CharCode
            sta (POSCHARPT),Y       ; Sprite char A
            lda Colour
            sta (POSCOLPT),Y
            iny
            sta (POSCOLPT),Y
            clc
            lda CharCode
            adc #2
            sta (POSCHARPT),Y       ; Sprite char C
            tya
            adc #15
            tay
            lda CharCode
            adc #1
            sta (POSCHARPT),Y       ; Sprite char B
            lda Colour
            sta (POSCOLPT),Y
            iny
            sta (POSCOLPT),Y
            lda CharCode
            adc #3
            sta (POSCHARPT),Y       ; Sprite char D
@ret:       dex
            bne @loop
            rts

; Add the value contained in A to the current score

AddScore:   clc
            adc Score
            sta Score
            bcc @contadd
            inc Score+1
@contadd:   jsr draw1l
            rts

; Control the movement of the bullet/laser shot fired by the cannon.

MoveShoots: ldx #0              ; Update the position of the shot
@loop:      lda FireSpeed,X     ; Check if the shot is active (speed!=0)
            beq @cont
            dec FirePosY,X
            lda FirePosY,X
            cmp #1              ; Check if we reached the top of the screen
            bcs @stillf
            lda #$0             ; Here we reached the top of the screen
            sta EFFECTS
            lda #$FF            ; In this case, destroy the bomb
            sta FireSpeed,X
            sta FirePosY,X
            bmi @cont
@stillf:    ora #$80
            sta EFFECTS         ; Sound effect: peeewwww!!!
@cont:      inx
            cpx #NMSHOTS
            bne @loop
DrawShots:  ldx #0              ; Draw shots
loop4:      stx tmpindex
            lda FirePosX,X
            sta tmpx            ; Store the X position of the shot
            lda FirePosOY,X
            cmp FirePosY,X      ; Check if the shot should be redrawn
            beq notmove
            tay
            lda FireColOver,X
            sta Colour
            lda FireChOver,X    ; Erase the previous shot
            cmp #$FF            ; In some situations, all the aliens have to
            bne @normalc        ; be redrawn to avoid a glith...
            jsr DrawAliens      ; this is not very elegant!
            jmp @noerase
@normalc:   ldx tmpx            ; Load the X position
            jsr DrawChar        ; Erase the shot in the old position
@noerase:   ldx tmpindex
            lda FireSpeed,X     ; Do not draw inactive shots
            beq notmove
            cmp #$FF
            bne @normal
            lda #0
            sta FireSpeed,X
            sta EFFECTS
@normal:    lda FirePosY,X
            sta tmpy
            sta FirePosOY,X     ; Save the current position
            tay
            ldx tmpx
            jsr GetChar         ; Check for a collision
            ldx tmpindex
            sta FireChOver,X
            pha
            lda ColourRead
            sta FireColOver,X
            pla
            ldx tmpx
            cmp #EMPTY
            bne collision
            lda #YELLOW         ; Colour of the shots
            sta Colour
            lda #SHOT
            jsr DrawChar        ; Draw the shot in the new position
notmove:    ldx tmpindex
            inx
            cpx #NMSHOTS
            bne loop4
            rts

; Here we know that a collision took place, so we should see what element has
; been encountered by the bullet. A contains the character that has been found

collision:  cmp #SPRITE1A
            bmi @noalien
            cmp #SPRITE2D
            bpl @noalien
            jmp checkalienshot
            
@noalien:   pha
            ldy tmpindex
            lda #EMPTY
            sta FireChOver,y
            ldy tmpy
            pla

            cmp #BLOCK
            beq bunkershot
            cmp #BLOCKR
            beq bunkershot
            cmp #BLOCKL
            beq bunkershot
            cmp #MOTHER1
            beq mothershot
            cmp #MOTHER2
            beq mothershot
            cmp #MOTHER3
            beq mothershot
            jmp notmove
backcoll:   jsr CheckWin
            jmp notmove

; Handle the different collisions.
; X and Y contain the position of the collision, also available in tmpx and
; tmpy respectively

; Collision with the mother ship

mothershot: txa
            pha
            ldx MotherPos   ; Draw the ship
            ldy #$1
            lda #EMPTY
            jsr DrawChar
            inx
            jsr DrawChar
            inx
            jsr DrawChar
            jsr Explosion
            pla
            tax
            jsr DrawChar
            lda #$FD
            sta MotherPos
            lda #$FF
            stx ExplMXpos
            ldx tmpindex
            sta FireSpeed,X
            sta FirePosY,X      ; This would cause a redraw erasing the shot
            lda #$05            ; Update the score: +50 pts
            sta ExplMCnt
            jsr AddScore
            ldx tmpindex
            jmp backcoll

bunkershot: lda #RED+MULTICOLOUR
            sta Colour
            lda #EXPLOSION1
            jsr DrawChar
            lda #$FF
            ldx tmpindex
            sta FireSpeed,X
            sta FirePosY,X      ; This would cause a redraw erasing the shot
            lda #EMPTY
            sta FireChOver,X
            jmp backcoll

; Handle a possible collision with an alien.

checkalienshot:
            sta CharCode
            jsr CalcChGenOfs
            ldy #7
@loop:      lda #SHOTMSK        ; Check collision between active pixels in the
            and (CHRPTR),Y      ; sprite and the SHOTMSK
            bne @realshot
            dey
            cpy #$FF
            bne @loop

; No collision: blend the sprite with the shot and print it on screen

            ldy #7
@loop1:     lda GRCHARS1+SHOT*8,Y
            ora (CHRPTR),Y
            sta GRCHARS1+BLENDCH*8,Y
            dey
            cmp #$ff
            bne @loop1
            ldy tmpy
            lda ColourRead
            sta Colour
            lda #BLENDCH
            jsr DrawChar
            ldy tmpindex
            lda #$FF            ; Redraw all the aliens in this situation.
            sta FireChOver,y    ; If this is not done, there is a possible
            jmp backcoll        ; glitch that appears when the old char is not
                                ; correct in the new sprite position.

; Here, the collision with an alien is sure

@realshot:  lda #$D0            ; We are going to make some noise!
            sta NOISE
            txa                 ; X contains the current hor. position
            sec
            sbc PixPosX         ; Correct for the pixel shift in the character
            lsr                 ; Divide it by two.
            tax
            lda #$01            ; Create a code by shifting left the number of
            cpx #$00            ; times that corresponds to the X position/2
            beq @r1
@contsh:    asl
            dex
            bne @contsh
@r1:        pha                 ; A contains the code to XOR to the aliens line
            lda AlienPosYc
            sta tmp4
            pla
            ldy tmpy            ; Get the current vert. position
            cpy tmp4
            beq @p1
            dey
            cpy tmp4
            bne @l2
@p1:        eor AliensR1s       ; kill aliens with XOR!    ;-)
            sta AliensR1s
            jmp @follow
@l2:        inc tmp4
            cpy tmp4
            beq @p2
            inc tmp4
            cpy tmp4
            bne @l3
@p2:        eor AliensR2s
            sta AliensR2s
            jmp @follow

@l3:        eor AliensR3s       ; Add here for more than three lines of aliens
            sta AliensR3s
@follow:    
            jsr Explosion       ; BOOOOM!
            ldy tmpy
            ldx tmpx
            jsr DrawChar
            lda #$FF
            ldx tmpindex
            sta FireSpeed,X
            sta FirePosY,X      ; This would cause a redraw erasing the shot
            lda #1
            jsr AddScore
            jmp backcoll

; Load an explosion in FireChOver. Employs X register, A will contain the
; EXPLOSION1 char, load a multicolour value in Color.

Explosion:  ldx tmpindex
            lda #YELLOW+MULTICOLOUR
            sta FireColOver,X
            sta Colour
            lda #EXPLOSION1
            sta FireChOver,X
            rts


; Check if the player won the game.

CheckWin:   lda AliensR1s       ; Check if all aliens have been destroyed
            bne @exit
            lda AliensR2s
            bne @exit
            lda AliensR3s
            bne @exit
            lda #$FF            ; If we come here, all aliens have been shot
            sta Win             ; That will stop the game
            lda #$00            ; Mute all effects
            sta NOISE
            sta EFFECTS
            ldx #4              ; write "YOU WON"
            ldy #15
            lda #YELLOW
            sta Colour
            lda #<YouWonSt
            sta LAB_01
            lda #>YouWonSt
            sta LAB_02
            jsr PrintStr
            lda #$00
            sta VOICE2
            lda #$B0            ; Win! Play a chime!
            sta VOICE1
            jsr Delay
            lda #$C0
            sta VOICE1
            jsr Delay
            lda #$D0
            sta VOICE1
            jsr Delay
            lda #$00
            sta VOICE1
            inc Level
@exit:      rts

; Game over! Zero the score, turn the screen to red and write "GAME OVER"

GameOver:   lda #$00            ; Mute all effects
            sta EFFECTS
            sta Level
            lda #$FF
            sta Win             ; Stop the game
            jsr ZeroScore       ; Put the score to zero
            lda #RED            ; Put all the screen in red (sooo bloody!)
            sta Colour
            jsr PaintColour
            lda #$B0            ; Explosion sound
            sta NOISE
            lda #$FF
@loop:      jsr ShortDelay
            sec
            sbc #$11
            sta VOLUME
            bne @loop
            lda #$00
            sta NOISE
            ldx #4              ; write "GAME OVER"
            ldy #15
            lda #<GameOverSt
            sta LAB_01
            lda #>GameOverSt
            sta LAB_02
            jsr PrintStr
@exit:      rts

; Control bombs dropping. A maximum of 8 bombs can be falling at the same
; time. A bomb is active and falling if its speed is greater than 0.
; BombSpeed, BombPosX and BombPosY are the arrays containing the speed and the
; positions. Exploit tmpindex and tmpx, change registers A, X, Y.
; This routine does three things:
;
; 1 - For each alien alive, and for each of the NMBOMBS bombs available, decide
;     if a bomb is dropped by drawing a random number and check if it is inside
;     a given interval
; 2 - Update the positions of the bombs active in the screen and check for
;     collisions.
; 3 - Draw the bombs in the new positions on the screen.
;

FallBombs:  inc Bombcntr
            lda Bombcntr
            cmp BombPeriod
            bcc DrawBombs
            lda #0
            sta Bombcntr
            lda AliensR1s
            sta AliensR1
            lda AliensR2s
            sta AliensR2
            ldx #NMBOMBS
            ldy AlienPosYc
            sty AlienCurrY      ; Load the current vertical position of aliens
@loop1:     asl AliensR1
            bcc @ret1
            jsr DropBomb
@ret1:      dex
            bne @loop1          ; End of loop for processing the first line
            inc AlienCurrY
            inc AlienCurrY
            ldy AlienCurrY
            ldx #NMBOMBS
@loop2:     lda AliensR2
            asl AliensR2
            bcc @ret2
            jsr DropBomb
@ret2:      dex
            bne @loop2

            ldx #NMBOMBS        ; Update the position of the bombs
@loop3:     lda BombSpeed-1,X   ; Check that the bomb is active (speed>0)
            beq @cont
            clc                 ; If speed !=0, update current Y position
            adc BombPosY-1,X
            cmp #31             ; Check if we reached the last line
            bcc @stillf
            lda #$FF            ; In this case, destroy the bomb
            sta BombSpeed-1,X
@stillf:    sta BombPosY-1,X
@cont:      dex
            bne @loop3

DrawBombs:  ldx #0              ; Draw bombs
            lda BombPeriod      ; Colour of the bombs
            sta Colour
@loop4:     stx tmpindex
            lda BombSpeed,X     ; Do not draw inactive bombs
            beq @notmove
            stx tmpindex
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
            beq @notmove
@normal:    lda BombPosY,X
            sta BombPosOY,X     ; Save the current position
            sta tmpy
            tay
            ldx tmpx
            jsr GetChar         ; Check for a collision
            cmp #EMPTY
            beq @drawbomb
            cmp #CANNON
            beq @BombExpl        ; Explode the bomb
            cmp #BLOCK
            beq @explode         ; Explode the bomb
            cmp #BLOCKR
            beq @explode         ; Explode the bomb
            cmp #BLOCKL
            beq @explode         ; Explode the bomb
@drawbomb:  lda #BOMB
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
            lda BombSpeed,Y     ; Check if the current bomb is not active
            bne @searchlp
            lda #1
            sta BombSpeed,Y
            lda SpriteX
            sta tmpp
            lda AlienCurrY
            sta BombPosY,Y
            txa
            asl
            adc PixPosX
            ror Random+1
            sbc #1
            ror tmpp
            ror tmpp
            adc #$0
            sta BombPosX,Y
@nobomb:    rts

; Music driver for voice 1. It should be called every IRQ to handle music

Music1:     ldy Voice1ctr
            beq @playnext
            cpy Voice1nod
            bne @dec
            lda #$00
            sta VOICE1
@dec:       dey 
            sty Voice1ctr
            rts

@playnext:  ldx Voice1ptr
            lda Voice1data,x
            cmp #repeatm
            beq @repeat
            cmp #endloop
            beq @endmloop
            and #maskcode
            cmp #loopcode
            beq @loopmusic
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice1ptr
            rts

@loopmusic: lda Voice1data,x
            and #unmask
            sta Loop1ctr
            inx
            stx Voice1ptr
            stx Loop1str
            jmp @playnext

@endmloop:  ldy Loop1ctr
            dey
            sty Loop1ctr
            beq @exitloop
            ldx Loop1str
            stx Voice1ptr
            jmp @playnext
@exitloop:  inx
            stx Voice1ptr
            jmp @playnext

@note:      lda Voice1data,x
            and #unmask
            clc
            adc #128
            adc VoiceBase
            sta VOICE1
            lda Voice1drt
            sta Voice1ctr
            jmp @exitmusic

@duration:  lda Voice1data,x
            and #unmask
            sta Voice1drt
            inx
            lda Voice1data,x
            sta Voice1nod
            inx
            stx Voice1ptr
            jmp @playnext

@repeat:    ldx #$FF            ; That will overflow to 0 at the next inx
            jmp @exitmusic

; Music driver for the second voice. Very similar to voice 1.

Music2:     ldy Voice2ctr
            beq @playnext
            cpy Voice2nod
            bne @dec
            lda #$00
            sta VOICE2
@dec:       dey 
            sty Voice2ctr
            rts

@playnext:  ldx Voice2ptr
            lda Voice2data,x
            cmp #repeatm
            beq @repeat
            cmp #endloop
            beq @endmloop
            and #maskcode
            cmp #loopcode
            beq @loopmusic
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice2ptr
            rts

@loopmusic: lda Voice2data,x
            and #unmask
            sta Loop2ctr
            inx
            stx Voice2ptr
            stx Loop2str
            jmp @playnext

@endmloop:  ldy Loop2ctr
            dey
            sty Loop2ctr
            beq @exitloop
            ldx Loop2str
            stx Voice2ptr
            jmp @playnext
@exitloop:  inx
            stx Voice2ptr
            jmp @playnext

@note:      lda Voice2data,x
            and #unmask
            clc
            adc #128
            adc VoiceBase
            sta VOICE2
            lda Voice2drt
            sta Voice2ctr
            jmp @exitmusic

@duration:  lda Voice2data,x
            and #unmask
            sta Voice2drt
            inx
            lda Voice2data,x
            sta Voice2nod
            inx
            stx Voice2ptr
            jmp @playnext

@repeat:    ldx #$FF            ; That will overflow to 0 at the next inx
            jmp @exitmusic


; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. It uses PosX and PosY.
; Colour contains the colour code of the character. It uses 1 byte in the
; stack and does not change A, X, Y.

DrawChar:   cpx #16         ; Check if the X value is out of range
            bcs @exit       ; Exit if X greater than 16 (no of columns)
            cpy #31         ; Check if the Y value is out of range
            bcs @exit       ; Exit if Y greater than 31 (no of rows)
            sty PosY
            pha
            jsr PosChar
            ldy #0
            lda Colour
            sta (POSCOLPT),Y
            pla
            sta (POSCHARPT),Y
            ldy PosY
@exit:      rts

; Get the screen code of the character in the X and Y locations.
; The character is returned in A. The character colour is returned in Colour

GetChar:    cpx #16         ; Check if the X value is out of range
            bcs @exit       ; Exit if X greater than 16 (no of columns)
            cpy #31         ; Check if the Y value is out of range
            bcs @exit       ; Exit if Y greater than 31 (no of rows)
            sty PosY
            jsr PosChar
            ldy #0
            lda (POSCOLPT),Y
            sta ColourRead
            lda (POSCHARPT),Y
            ldy PosY
            rts
@exit:      lda #EMPTY
            rts

; Calculate the address of a screen position and put it in POSCHARPT. Do the
; same for the color address and put it in POSCOLPT.
; X and Y contain screen coordinates.

PosChar:    stx PosX
            lda #<MEMSCR
            sta POSCHARPT
            lda #>MEMSCR
            sta POSCHARPT+1
            lda #<MEMCLR
            sta POSCOLPT
            lda #>MEMCLR
            sta POSCOLPT+1
            tya
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            bcc @nocorr     ; we need to write in the bottom-half of the screen
            inc POSCHARPT+1
            inc POSCOLPT+1
            clc
@nocorr:    adc PosX
            pha
            adc POSCHARPT
            sta POSCHARPT
            pla
            adc POSCOLPT
            sta POSCOLPT
            rts


; Put a "sprite", that is a 8x8 cell in a 2x2 character position.
; A contains the character code. It should be less than 32.
; The 4 characters are disposed as follows:
;
;    AC
;    BD
;
; Characters are redefined starting from address contained in SPRITECH
; SpriteX: the horizontal offset in pixels [0,8]
; SpriteY: the vertical offset in pixels [0,8]
; CharCode: the character to be used for the sprite
; Employs SPRITECH pointer to the group of 4 ch. for a sprite (word)
; and CHRPTR pointer to the original ch. in a sprite (word)

LoadSprite: clc
            lda SPRITECH        ; Calculate the vert. offset in ch. table
            adc SpriteY
            sta SPRITECH
            bcc @normal         ; Correct if page change
            inc SPRITECH+1
@normal:    jsr CalcChGenOfs
            ldy #0              ; Copy 8 bytes
@loop1:     lda #0
            sta CharShr
            lda (CHRPTR),y      ; Charge source
            ldx SpriteX
            beq @noshift
@loop2:     lsr                 ; At the beginning of the cycle, x contains
            ror CharShr         ; the shift in pixels to the right
            dex
            bne @loop2
@noshift:   sta (SPRITECH),y    ; Save
            tya
            pha
            adc #16
            tay
            lda CharShr
            sta (SPRITECH),y    ; Save
            pla
            tay
            iny
            cpy #08
            bne @loop1
            rts

; Load CHRPTR and CHRPTR+1 with the address of the character generator area
; corresponding to the character contained in CharCode

CalcChGenOfs:
            lda #<GRCHARS1      ; Charge the address of the ch. gen in CHARPTR
            sta CHRPTR
            lda #>GRCHARS1
            sta CHRPTR+1
            lda CharCode        ; Charge the ch. code to be used
            asl                 ; Multiply it times 8
            asl
            asl
            adc CHRPTR          ; Add to the CHRPTR (to get address of the ch.)
            sta CHRPTR
            bcc @normal         ; Correct if page change
            inc CHRPTR+1
@normal:    rts

; Clear the contents of a "sprite".
ClearSprite:
            lda #0
            ldy #32
@loop:      dey
            sta (SPRITECH),y    ; sta does not affect processor flags
            bne @loop
            rts

; A basic test showing a "sprite"
; testsprite: lda #<(GRCHARS1+(LASTCH+1)*8)
;             sta SPRITECH
;             lda #>(GRCHARS1+(LASTCH+1)*8)
;             sta SPRITECH+1
;             jsr ClearSprite
; 
;             lda SpriteX
;             tax
;             lda AlienPosY
;             and #7
;             tay
;             ;ldx #7
;             lda #ALIEN3
;             jsr LoadSprite
;             
;             lda #MAGENTA
;             sta Colour
; 
;             lda #(LASTCH+1)
;             ldx #8
;             ldy #15
;             jsr DrawChar
;             
;             lda #(LASTCH+2)
;             ldx #8
;             ldy #16
;             jsr DrawChar
;             
;             lda #(LASTCH+3)
;             ldx #9
;             ldy #15
;             jsr DrawChar
;             
;             lda #(LASTCH+4)
;             ldx #9
;             ldy #16
;             jsr DrawChar
;             rts

; Print a string (null terminated) whose address is contained in LAB_01 and
; LAB_02 at the position given by X and Y pointers

PrintStr:   sty PosY
            ldy #$00
@loop:      lda (LAB_01),Y
            beq @exit
            sty tmp4
            ldy PosY
            jsr DrawChar
            ldy tmp4
            iny
            inx
            jmp @loop
@exit:      ldy PosY
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

; A simple delay

Delay:      ldx #$FF
            ldy #$FF
Delayloop:  dex
            bne Delayloop
            dey
            bne Delayloop
            rts

ShortDelay: ldy #$40
            ldx #$FF
            jmp Delayloop

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
            lsr             ; Shift the four most significant bits
            lsr             ; ... into the four least significant
            lsr
            lsr
            clc
            adc #(48+$80)   ; Make a screen code char
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
; Data and configuration settings. Tables and some music-related stuff.
; Characters!
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

BombSpeed:  .res NMBOMBS, $00   ; Array containing the speed of the bombs sent
BombPosX:   .res NMBOMBS, $00   ; Array with X positions of bombs
BombPosY:   .res NMBOMBS, $00   ; Array with Y positions of bombs
BombPosOY:  .res NMBOMBS, $00   ; Array with old Y positions of bombs

FireSpeed:  .res NMSHOTS, $00   ; Array containing the speed of the bullet sent
FirePosX:   .res NMSHOTS, $00   ; Array with X positions of bullet
FirePosY:   .res NMSHOTS, $00   ; Array with Y positions of bullet
FirePosOY:  .res NMSHOTS, $00   ; Array with old Y positions of bullet
FireChOver: .res NMSHOTS, $00   ; Array containing the character overwritten
FireColOver:.res NMSHOTS, $00   ; Array containing the ch. colour overwritten


; Music data. Much is loop-based, to reduce mem occupation. 
; The code for a loop is as follows:
; 1 byte: 10xx xxxx where the xxx xxx represent the number of times the loop
;                   should be repeated
; Special code: 1111 1111 repeat from start
; The code for a note is as follows:
;         01 zz zzzz where zz zzzz represents the note (standard VIC 20)
;         01 11 1111 is a silence
; Special codes for note durations:
;         00 ss ssss specify that the following notes should have the given
;            duration in 1/50's of seconds
;         it should be followed by a byte giving the duration of the silence in
;         the note
loopcode = %10000000
notecode = %01000000
silence  = %01111111
duracode = %00000000
endloop  = %11000000
repeatm  = %11111111
maskcode = %11000000
unmask   = %00111111

VoiceBase:  .byte $00

Voice1ptr:  .byte $00
Voice1ctr:  .byte $00
Loop1ctr:   .byte $00
Loop1str:   .byte $00
Voice1drt:  .byte $00
Voice1nod:  .byte $00

Voice2ptr:  .byte $00
Voice2ctr:  .byte $00
Loop2ctr:   .byte $00
Loop2str:   .byte $00
Voice2drt:  .byte $00
Voice2nod:  .byte $00

Voice1data: .byte duracode + 30, 25
            .byte loopcode + 2
            ; a simple scale
            .byte notecode + 0, notecode + 2, notecode + 4, notecode + 5
            .byte notecode + 7, notecode + 9, notecode + 11, notecode + 12
            .byte endloop
            
            .byte loopcode + 2
            ; a simple scale
            .byte notecode + 24, notecode + 26, notecode + 28, notecode + 29
            .byte notecode + 31, notecode + 33, notecode + 35, notecode + 36
            .byte endloop
            
            .byte repeatm

Voice2data: .byte duracode + 15, 12
            .byte loopcode + 8
            ; a simple scale
            .byte notecode + 40, silence
            .byte notecode + 48, silence
            .byte endloop

            .byte loopcode + 16
            ; a simple scale
            .byte notecode + 32, silence
            .byte endloop
            
            .byte repeatm

YouWonSt:   .byte (25+$80), (15+$80), (21+$80), (32+$80), (23+$80), (15+$80)
            .byte (14+$80), 0

GameOverSt: .byte (7+$80), (1+$80), (13+$80), (5+$80), (32+$80), (15+$80)
            .byte (22+$80), (5+$80), (18+$80), 0

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
            .byte %00100100
            .byte %00011000
            .byte %00100100
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
            
            SHOTMSK = %00010000 ; Mask for detecting a collision

            EXPLOSION1=11
            .byte %10000000     ; Block, ch. 11 (normally M)
            .byte %00100010
            .byte %10010000
            .byte %00110100
            .byte %11101110
            .byte %00110000
            .byte %00110011
            .byte %10000010

            MOTHER1=12
            .byte %00000000     ; Mother ship 1
            .byte %00011111
            .byte %01111111
            .byte %11011011
            .byte %11111111
            .byte %01111111
            .byte %00011111
            .byte %00000000

            MOTHER2=13
            .byte %00000000     ; Mother ship 2
            .byte %11111111
            .byte %11111111
            .byte %01101101
            .byte %11111111
            .byte %11111111
            .byte %11111111
            .byte %00000000

            MOTHER3=14
            .byte %00000000     ; Mother ship 3
            .byte %11110010
            .byte %11111100
            .byte %10110110
            .byte %11111110
            .byte %11111100
            .byte %11110011
            .byte %00000000

            LASTCH = MOTHER3
            SPRITE1A = LASTCH+1
            SPRITE1B = LASTCH+2
            SPRITE1C = LASTCH+3
            SPRITE1D = LASTCH+4
            
            ; The sprites characters should be continuous.
            ; This simplifies collision detection.
            
            SPRITE2A = LASTCH+5
            SPRITE2B = LASTCH+6
            SPRITE2C = LASTCH+7
            SPRITE2D = LASTCH+8
            
            BLENDCH = LASTCH+9
            ;Level 0 1 2 3 4 5 6 7 8 9 A B
LevelsPer:  .byte  5,4,3,3,2,2,2,2,1,1,1,1
LevelsBomb: .byte  3,2,3,2,2,3,2,1,4,3,2,1           

NUMLEVEL   = 12 ; Total number of levels.

EndMemMrk:  .byte 0

            