;         V I C   A L I E N - I N V A D E R S
;
;             by Davide Bucci, April-May 2018
;
; 
;
; The assembler used is ca65
;

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

        CharCode = LAB_03   ; Employed in DrawChar
        PosX = LAB_04
        PosY = LAB_05
        Colour = LAB_06     ; Colour to be used by the printing routines
        tmp4 = LAB_0A



; VIC-chip addresses
        VICSCRHO = $9000    ; Horisontal position of the screen
        VICSCRVE = $9001    ; Vertical position of the screen
        VICCOLNC = $9002    ; Screen width in columns and video memory addr.
        VICROWNC = $9003    ; Screen height, 8x8 or 8x16 chars, scan line addr.
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


        VOICE1  = GEN1
        VOICE2  = GEN2
        EFFECTS = GEN3

.export main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

; Main program here.

main:
            jsr Init        ; Init the game (load graphic chars, etc...)
            jsr CLS
            ldx #$0
            ldy #$8
            lda #BLACK
            jsr PaintColour
            lda #YELLOW
            sta Colour
            lda #<TitleGame
            sta LAB_01
            lda #>TitleGame
            sta LAB_02
            jsr PrintStr
            ldx #$0
            ldy #$12
            lda #<Credits
            sta LAB_01
            lda #>Credits
            sta LAB_02
            jsr PrintStr
            ldx #$0
            ldy #$14
            lda #<L1
            sta LAB_01
            lda #>L1
            sta LAB_02
            jsr PrintStr
            iny
            lda #<L2
            sta LAB_01
            lda #>L2
            sta LAB_02
            jsr PrintStr
            iny
            lda #<L3
            sta LAB_01
            lda #>L3
            sta LAB_02
            jsr PrintStr
mainloop:   
@continue4: jmp mainloop


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
            lda #$FF        ; Move the character generator address to $1C00
            sta VICCHGEN    ; while leaving ch. 128-255 to their original pos.
            jsr MovCh       ; Load the graphic chars
            lda #$0F        ; Turn up the volume
            sta VOLUME
            sei             ; Configure the interrupt handler
            lda #<IrqHandler
            sta $0314
            lda #>IrqHandler
            sta $0315
            cli
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
            jsr Music1
            jsr Music2
            pla             ; Restore registers
            tay
            pla
            tax
            pla
            jmp $EABF       ; Jump to the standard IRQ handling routine

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
; to multiply times 16 and then add the X contents. It uses CharCode and PosX.
; Colour indicates the colour code of the character. It uses 3 bytes in the
; stack and does not change registers.

DrawChar:   sta CharCode
            stx PosX
            sty PosY
            cpx #16         ; Check if the X value is out of range
            bcs @exit       ; Exit if X greater than 16 (no of columns)
            cpy #31         ; Check if the Y value is out of range
            bcs @exit       ; Exit if Y greater than 31 (no of rows)
            tya
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            bcc @tophalf    ; we need to write in the bottom-half of the screen
            clc
            adc PosX
            tay
            lda Colour
            sta MEMCLR+256,Y
            lda CharCode
            sta MEMSCR+256,Y
            jmp @exit
@tophalf:   adc PosX
            tay
            lda Colour
            sta MEMCLR,Y
            lda CharCode
            sta MEMSCR,Y
@exit:      ldy PosY
            rts

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

; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA
;
; Data and configuration settings.
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

; Music data. Much is loop-based, to reduce mem occupation. 
; IMPROVE DESC!
; The code for a loop is as follows:
; 1 byte: 10xx xxxx where the xxx xxx represent the number of times the loop
;                   should be repeated
; Special code: 1111 1111 repeat from start
; The code for a note is as follows:
;         01 zz zzzz where zz zzzz represents the distance in semitones from C
;         01 11 1111 is a silence
; Special codes for note durations:
;         00 ss ssss specify that the following notes should have the given
;            duration in 1/60's of seconds
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
            ; a simple diatonic scale
            .byte notecode + 0, notecode + 2, notecode + 4, notecode + 5
            .byte notecode + 7, notecode + 9, notecode + 11, notecode + 12
            .byte endloop
            
            .byte loopcode + 2
            ; a simple diatonic scale
            .byte notecode + 24, notecode + 26, notecode + 28, notecode + 29
            .byte notecode + 31, notecode + 33, notecode + 35, notecode + 36
            .byte endloop
            
            .byte repeatm

Voice2data: .byte duracode + 15, 12
            .byte loopcode + 8
            ; a simple diatonic scale
            .byte notecode + 40, silence
            .byte notecode + 48, silence
            .byte endloop

            .byte loopcode + 16
            ; a simple diatonic scale
            .byte notecode + 32, silence
            .byte endloop
            
            .byte repeatm

            .charmap 'A',(1+$80)
            
            ;       "                      "
TitleGame:  .asciiz "    ALIEN INVASION"
Credits:    .asciiz "   C. 2018 D. BUCCI"
L1:         .asciiz "  TARGETS:"
L2:         .byte " ", ALIEN1,ALIEN2,ALIEN3,ALIEN4, " 10 pt",0
L3:         .byte " ", MOTHER1,MOTHER2,MOTHER3,    "  50 pt",0



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

            