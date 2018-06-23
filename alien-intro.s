;         V I C   A L I E N - I N V A D E R S
;
;             by Davide Bucci, April-May 2018
;
; A small loader showing an instruction screen and playing a two voice tune 
; (J.S. Bach, Fantasia of Partita 3, BWV 827).
;
; The code is way less polished than the game, so do not be too harsh judging
; here...
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
        secs = LAB_07       ; Write in the second half of the screen



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


        VOICE1  = GEN3
        VOICE2  = GEN1
        VOICE3  = GEN2
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
            lda #32
            jsr CLS
            ldx #$0
            ldy #$0
            lda #BLACK
            stx secs
            jsr PaintColour
            lda #YELLOW
            sta Colour
            lda #<TitleGame
            sta LAB_01
            lda #>TitleGame
            sta LAB_02
            jsr PrintStr
            lda #<ScoreDesc
            sta LAB_01
            lda #>ScoreDesc
            sta LAB_02
            lda #CYAN
            sta Colour
            jsr PrintStr
            
            ldx #0
            ldy #0
            lda #1
            sta secs
            lda #<Keys
            sta LAB_01
            lda #>Keys
            sta LAB_02
            lda #MAGENTA
            sta Colour
            jsr PrintStr
            lda #YELLOW
            sta MEMCLR+10*22+5
            sta MEMCLR+10*22+6
            sta MEMCLR+10*22+7
            lda #RED
            sta MEMCLR+8*22+6
            lda #GREEN
            sta MEMCLR+8*22+8
            
@loopchar:  jsr GETIN       ; Main loop waiting for keyboard events
            beq @loopchar
            lda #$00
            sta GEN1
            sta GEN2
            sta GEN3
            sta VOLUME
            sta 646         ; Change current colour to black
            sei             ; Configure the interrupt handler
            lda #<$EABF
            sta $0314
            lda #>$EABF
            sta $0315
            cli
            lda #8
            sta 36879
            lda #240
            sta 36869
            jsr $E55F           ; Clear the screen and home cursor
            ldx #21
            ldy #$0
            lda #$0
            stx secs
            lda #<LoadStr       ; Write a load command
            sta LAB_01
            lda #>LoadStr
            sta LAB_02
            lda #BLACK
            sta Colour
            jsr PrintStr
            lda 186             ; Get the last opened device
            clc
            cmp #01
            beq @cassette
            cmp #08
            beq @disk8
            cmp #09
            beq @disk9
            cmp #10
            beq @disk10
            cmp #11
            beq @disk11
            
@loadc:     jsr DrawChar        ; Complete the load command
            inx
            lda #<EndL
            sta LAB_01
            lda #>EndL
            sta LAB_02
            lda #BLACK
            sta Colour
            jsr PrintStr
            lda #<Loading
            sta LAB_01
            lda #>Loading
            sta LAB_02
            lda #WHITE
            sta Colour
            lda #01
            sta secs
            ldx #30
            ldy #0
            jsr PrintStr
            ldy #0
@loop:      lda LoadCmd,Y       ; Write in the keyboard buffer
            sta 631,Y
            iny
            cpy #LoadCmdLen
            bne @loop
            lda #LoadCmdLen
            sta 198
            rts

@cassette:
@disk8:
@disk9:     clc
            adc #48
            jmp @loadc

@disk10:    lda #49
            jsr DrawChar
            inx
            lda #48
            jmp @loadc

@disk11:    lda #49
            jsr DrawChar
            inx
            lda #49
            jmp @loadc


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
            lda #100        ; Disable RUN/STOP and RESTORE
            sta 808         ; One may argue why to deactivate this (usually
            cli             ; considered a sort of "copy protection") in an
            rts             ; open source game. It happens that the game uses
                            ; plenty of page 0 variables that interfere badly
                            ; with BASIC normal variables. After RUN/STOP and
                            ; RESTORE, the computer was basically inusable
                            ; anyway

; Copy the graphic chars.

MovCh:
            ldx #(LASTCH+1)*8+1
@loop:      lda DefChars-1,x
            sta GRCHARS1-1,x
            dex
            bne @loop

ClearSpc:   lda #$00
            ldx #8
@loop:      lda #$00
            sta GRCHARS1-1+32*8,x
            dex
            bne @loop
            rts

; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ
;
; Basically, only handles music here.
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
            sta VOICE3
@dec:       dey 
            sty Voice1ctr
            rts

@playnext:  ldx Voice1ptr
            lda Voice1data,x
            cmp #repeatm
            beq @repeat
            and #maskcode
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice1ptr
            rts

@note:      lda Voice1data,x
            cmp #silence
            bne @normal
            lda #$00
@normal:    sta VOICE1
            sta VOICE3
            ldy Voice1drt
            sty Voice1ctr
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

@repeat:    ldx #0
            stx Voice1ptr
            stx Voice2ptr
            stx Voice2ctr
            stx Voice1ctr
            rts


; Music driver for voice 2. It should be called every IRQ to handle music

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
            and #maskcode
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice2ptr
            rts

@note:      lda Voice2data,x
            cmp #silence
            bne @normal
            lda #$00
@normal:    sta VOICE2
            ldy Voice2drt
            sty Voice2ctr
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

@repeat:    ldx #0
            stx Voice1ptr
            stx Voice2ptr
            rts

; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. It uses CharCode and PosX.
; Colour indicates the colour code of the character. It uses 3 bytes in the
; stack and does not change registers.

DrawChar:   sta CharCode
            stx PosX
            sty PosY
            tya
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            lda secs
            cmp #$01
            bne @tophalf    ; we need to write in the bottom-half of the screen
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
            rts

; Clear the screen. This maybe is too slow to be used in an interrupt handler.
; Draw everywhere the character contained in the A register. Employs X.

CLS:
            size=22*23/4+1
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
notecode = %10000000
silence  = %11111110
duracode = %00000000
repeatm  = %11111111
maskcode = %10000000
unmask   = %01111111

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

quaver = 31
quaverd = 20

semiquaver = 15
semiquaverd = 10

do0=128
dod0=134
re0=141
red0=147
mi0=153
fa0=159
fad0=164
sol0=170
sold0=174
la0=179
lad0=183
si0=187
do1=191
dod1=195
re1=198
red1=201
mi1=204
fa1=207
fad1=210
sol1=213
sold1=215
la1=217
lad1=219
si1=221
do2=223
dod2=225
re2=227
red2=229
mi2=230
fa2=231
fad2=232        ; Quite out of tune on higher pitches
sol2=234        ; ...
sold2=235       ;
la2=236
lad2=237
si2=238
do3=239
dod3=240

; Music data for J.S. Bach, Fantasia of Partita 3, BWV 827

Voice1data: ; Measures 1 - 6
            .byte duracode + semiquaver, semiquaverd
            .byte silence,la1,si1,sold1,la1,do2,re1,si1,mi1,re1,do1,si0
            .byte duracode + quaver, quaverd
            .byte do1,mi1,la1,la1,sold1,si1
            .byte duracode + semiquaver, semiquaverd
            .byte si1,la1,sold1,la1,si1,do2,re2,fa2,si1,fa2,mi2,re2
            
            ; Measures 7 - 12
            .byte duracode + quaver, quaverd
            .byte do2
            .byte duracode + semiquaver, semiquaverd
            .byte si1, la1
            .byte duracode + quaver, quaverd
            .byte fa2,fa2
            .byte duracode + semiquaver, semiquaverd
            .byte mi2,re2
            .byte duracode + quaver, quaverd
            .byte sol2
            .byte duracode + semiquaver, semiquaverd
            .byte mi2,do2,re2,si1,do2,mi2
            .byte sol2,si1,do2,la1,si1,re2
            .byte sol2,la1,si1,sold1,la1,do2
            .byte duracode + quaver, quaverd
            .byte fa2,re2,sol1
            
            ; Measures 13 - 18
            .byte mi2,do2,fa1
            .byte duracode + semiquaver, semiquaverd
            .byte re2,mi1,fa1,re1,mi1,sold1
            .byte duracode + quaver, quaverd
            .byte do2,si1,la1
            .byte duracode + semiquaver, semiquaverd
            .byte sold1,la1,si1,fa1,mi1,re1
            .byte do1,mi1,fa1,re1,mi1,la1
            .byte si1,mi1,fa1,re1,mi1,si1
            
            ; Measures 19-24
            .byte do2,mi1,fa1,re1,mi1,do2
            .byte si1,la1,sold1,si1
            .byte duracode +quaver, quaverd, mi1
            .byte duracode + semiquaver, semiquaverd
            .byte silence,mi2,fa2,re2,mi2,do2
            .byte si1,mi2,fa2,re2,mi2,si1
            .byte la1,mi2,fa2,re2,mi2,la1
            .byte sol1,si1,mi1,do2,re2,si1
            
            ; Measures 25-30
            .byte do2,mi2,la1,sold1,la1,do2
            .byte fad1,la1,re1,si1,do2,la1
            .byte si1,re2,sol1,fad1,sol1,si1
            .byte mi1,sol1,do1,la1,si1,sol1
            .byte la1,do2,fad1,mi1,fad1,la1
            .byte red1,fad1,si0,sol1,la1,fad1
            
            ; Measures 31-36
            .byte duracode +quaver, quaverd
            .byte sol1,si1,mi2,mi2,red2,fad2
            .byte duracode +semiquaver, semiquaverd
            .byte fad2,mi2,fad2,red2,mi2,sol2
            .byte la1,fad2,si1,la1,sol1,fad1
            .byte duracode +quaver, quaverd
            .byte sol1
            .byte duracode +semiquaver, semiquaverd
            .byte fad1,mi1
            .byte duracode +quaver, quaverd
            .byte do2
            .byte do2
            .byte duracode +semiquaver, semiquaverd
            .byte si1,la1
            .byte duracode +quaver, quaverd
            .byte re2
            
            ; Measures 37 - 42
            .byte duracode +semiquaver, semiquaverd
            .byte si1,do2,re2,mi2,fad2,sol2
            .byte la2,do3,fad2,do3,si2,la2
            .byte duracode +quaver, quaverd
            .byte sol2, si2,mi2,do2,la2,re2
            .byte si1,sol2,do2
            .byte repeatm
            
            ; Had to truncate here, because of the 256 events limit
 ;            
;             .byte duracode +semiquaver, semiquaverd
;             .byte la1,fad1,sol1,mi1,fad1,la1
;             
;             ; Measures 43 - 48
;             .byte re2,mi1,fad1,red1,mi1,sol1
;             .byte do2,red1,mi1,dod1,red1,fad1
;             .byte si1,mi1,la1,si0,do1,la0
;             .byte si0,red1,fad1,la1,sol1,fad1
;             .byte sol1,si1,do2,la1,si1,mi2
;             .byte fad2,si1,do1,la1,si1,fad2
;             
;             ; Measures 49 - 54
;             .byte sol2,si1,do2,la1,si1,sol2
;             .byte fad2,mi2,red1,fad2
;             .byte duracode +quaver, quaverd
;             .byte si1,si1
;             .byte duracode +semiquaver, semiquaverd
;             .byte mi2,fad2
;             .byte duracode +quaver, quaverd
;             .byte sol2
;             .byte la1
;             .byte duracode +semiquaver, semiquaverd
;             .byte sol1,fad1,sol1,la1
;             .byte si1,sol1,la1,fad1,sol1,mi2
;             .byte mi1,mi1,mi1,mi1,mi1,mi1
; 
;             .byte repeatm

Voice2data: ; Measures 1 - 6
            .byte duracode +quaver, quaverd
            .byte la1,179,la1,la1,sold1,mi1
            .byte duracode+semiquaver,semiquaverd
            .byte silence, la1,si1,sold1,la1,do2,re1,si1,mi1,re1,do1,si0
            .byte duracode +quaver, quaverd
            .byte do1,mi1,la1,la1,sold1,si1
            ; Measures 7 - 12
            .byte duracode+semiquaver,semiquaverd
            .byte si1,la1,sold1,la1,si1,do2
            .byte re2,fa2,si1,fa2,mi2,re2
            .byte duracode +quaver, quaverd
            .byte do2,mi2,la1
            .byte fa2,re2,sol1
            .byte mi2,do2,207
            .byte duracode+semiquaver,semiquaverd
            .byte re2,si1,do2,la1,si1,re2
            
            ; Measures 13-18
            .byte sol2,la1,si1,sol1,la1,do2
            .byte fa2, sold1,la1,fad1,sold1,si1
            .byte mi2,la1,re2,mi1,fa1,re1
            .byte duracode +quaver, quaverd
            .byte mi1,sold1,mi1
            .byte la1,la0,la1
            .byte duracode+semiquaver,semiquaverd
            .byte la1,sold1,la1,fad1,sold1,mi1
            
            ; Measures 19 - 24
            .byte la1,sold1,la1,si1,do2,re2
            .byte mi2,fa2,mi2,re2,do2,si1
            .byte duracode +quaver, quaverd
            .byte la1,la0,la1
            .byte sol1,sol0,sol1
            .byte fa1,fa0,fa1
            .byte mi1,sold0,mi0
            
            ; Measures 25 - 30
            .byte la0,si0,do1
            .byte re1,fad1,re1
            .byte sol1,la1,si1
            .byte do2,mi1,do1
            .byte fad1,sol1,la1
            .byte si1,red1,si0

            ; Measures 31 - 36
            .byte duracode +semiquaver, semiquaverd
            .byte mi1,mi2,fad2,red2,mi2,sol2
            .byte la1,fad2,si1,la1,sol1,fad1
            .byte duracode +quaver, quaverd
            .byte sol1,si0,mi1
            .byte mi1,red1,fad1
            .byte duracode +semiquaver, semiquaverd
            .byte fad1,mi1,red1,mi1,fad1,sol1
            .byte la1,do2,fad1,do2,si1,la1
            
            ; Measures 37 - 42
            .byte duracode +quaver, quaverd
            .byte sol1
            .byte duracode +semiquaver, semiquaverd
            .byte fad1,mi1
            .byte duracode +quaver, quaverd
            .byte do2,do2
            .byte duracode +semiquaver, semiquaverd
            .byte si1,la1
            .byte duracode +quaver, quaverd
            .byte re2
            .byte duracode +semiquaver, semiquaverd
            .byte si1,sol1,la1,fad1,sol1,si1
            .byte mi2,fad1,sol1,mi1,fad1,la1
            .byte re2,mi1,fad1,re1,mi1,sol1
            .byte duracode +quaver, quaverd
            .byte do2,la1,re1
            ; 
;             ; Measures 43 - 48
;             .byte si1,sol1,do1
;             .byte duracode +semiquaver, semiquaverd
;             .byte la1,si0,do1,la0,si1,red1
;             .byte duracode +quaver, quaverd
;             .byte sol1,fad1,mi1,red1,si1,red1
;             .byte mi1,mi0,mi1
;             .byte duracode +semiquaver, semiquaverd
;             .byte mi1,red1,mi1,dod1,red1,si1
;             
;             ; Measures 49 - 54
;             .byte duracode +quaver, quaverd
;             .byte mi1,mi0,mi1
;             .byte duracode +semiquaver, semiquaverd
;             .byte si0,do1,si0,la0,sol0,fad0
;             .byte sol0,si0,do1,la0,si0,sol0
;             .byte fad0,si0,do1,la0,si0,fad0
;             .byte mi0,si0,do1,la0,si0,mi0
;             .byte red0,fad0,si0,sol0,la0,fad0
;             
            .byte la1,la1,la1,la1

FileName:   .byte "alien-inv"
FileNameLen = 9
            ;     "                      "
TitleGame:  .byte "                      "
            .byte "                      "
            .byte "                      "
            .byte "    ",('A'-'@'),('L'-'@'),('I'-'@')
            .byte ('E'-'@'),('N'-'@')," ",('I'-'@')
            .byte ('N'-'@'),('V'-'@'),('A'-'@'),('S'-'@')
            .byte ('I'-'@'),('O'-'@'),('N'-'@')
            .byte "    "
            .byte "                      "
            .byte "   ",('C'-'@'),(46+$80)," ", (50+$80),(48+$80),(49+$80)
            .byte 56+$80," ",('D'-'@'),(46+$80)," ", ('B'-'@'), ('U'-'@')
            .byte ('C'-'@'),('C'-'@'),('I'-'@'),"   ",0

ScoreDesc:  .byte "                      "
            .byte "                      "
            .byte "    ", ALIEN2," ",ALIEN3," ",ALIEN4, " ",(49+$80),(48+$80)
            .byte " ", ('P'-'@'),('T'-'@'),"       "
            .byte "                      "
            .byte "     ", MOTHER1,MOTHER2,MOTHER3,"  ",(53+$80),(48+$80)
            .byte " ",('P'-'@'),('T'-'@'), "        "
            .byte "                      ",0

Keys:       .byte  "            ",27+128,('Z'-'@'),29+128, "   ",  ('L'-'@')
            .byte ('E'-'@') 
            .byte ('F'-'@'), ('T'-'@'),"        "
            .byte "    ", 27+128,('X'-'@'),29+128, "   ",  ('R'-'@'), ('I'-'@')
            .byte ('G'-'@')
            .byte  ('H'-'@'),('T'-'@'),"         "
            .byte 27+128,('S'-'@'),('P'-'@'), ('A'-'@'), ('C'-'@'), ('E'-'@')
            .byte 29+128," "
            .byte ('F'-'@'), ('I'-'@'), ('R'-'@'), ('E'-'@'),"            "
            .byte "                      "
            .byte  ('O'-'@'),  ('R'-'@')," ", ('J'-'@'), ('O'-'@')
            .byte  ('Y'-'@'),('S'-'@'),('T'-'@'),('I'-'@'), ('C'-'@')
            .byte ('K'-'@')
            .byte "         "
            .byte "                      "
            .byte "  "
            .byte  27+128,('M'-'@'), 29+128," ", ('T'-'@'),('O'-'@'), ('G'-'@')
            .byte ('G'-'@')
            .byte  ('L'-'@'), ('E'-'@'), " ", ('M'-'@'), ('U'-'@'), ('S'-'@')
            .byte  ('I'-'@'), ('C'-'@'), "   "
            .byte  27+128,('R'-'@'),('E'-'@'),('T'-'@'),('U'-'@'),('R'-'@')
            .byte ('N'-'@'),29+128
            .byte " ", ('R'-'@'),('E'-'@'), ('S'-'@'),('T'-'@'),('A'-'@')
            .byte ('R'-'@'), ('T'-'@'),"   "
            .byte 0

LoadStr:    .byte " ",('l'-'@'),('o'-'@'),('a'-'@'),('d'-'@'),34,('a'-'@')
            .byte ('l'-'@'),('i'-'@'),('e'-'@'),('n'-'@'),45
            .byte ('i'-'@'),('n'-'@'),('v'-'@'),34,44,0
            
EndL:       .byte "                      "           
            .byte "                      "
            .byte "                      "
            .byte "                      "
            .byte "                      "
            .byte "     " 
RunStr:     .byte ('r'-'@'),('u'-'@'),('n'-'@'),0
Loading:    .byte ('L'-'@'),('O'-'@'),('A'-'@'),('D'-'@'),('I'-'@')
            .byte ('N'-'@'),('G'-'@'),0

LoadCmdLen=5
LoadCmd:    .byte 131,13,13

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

            