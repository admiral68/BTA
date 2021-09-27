    bsr DecodeOldMan

    INCDIR ""
    INCLUDE "photon/PhotonsMiniWrapper1.04!.S"
    INCLUDE "photon/Blitter-Register-List.S"

*******************************************************************************
* DEFINES
*******************************************************************************

w               =320
h               =256
bplsize         =w*h/8

bpls            =3                      ;handy values:
bpl             =w/16*2                 ;byte-width of 1 bitplane line


om_bp_offset    = $100
om_upr_px_b_off = $20


    *-----------------*
    * logo dimensions *
    *-----------------*

logow           =320
;logomargin      =(320-logow)/2
logobpl         =logow/8
logobwid        =logobpl*3

    *-----------------*
    * palettes        *
    *-----------------*

logobgcol       =$44f



    *-----------------*
    * registers       *
    *-----------------*


INTREQR         = $1F
COP1LCH         = $80
DIWSTRT         = $8E
DIWSTOP         = $90
INTENA          = $9A
INTREQ          = $9C


*******************************************************************************
* MACROS
*******************************************************************************

logocolors:macro
    dc.w $068e,$0adf,$0dff
    dc.w $09bf,$056d,$044b,$033a
    endm

copper_pal_03:macro
    dc.w $0180,$0b87,$0182,$0754,$0184,$0975,$0186,$0ca8
    dc.w $0188,$0ed8,$018a,$0fff,$018c,$0060,$018e,$0090
    dc.w $0190,$00e0,$0192,$0777,$0194,$0aaa,$0196,$0747
    dc.w $0198,$0868,$019a,$0a8a,$019c,$0cac,$019e,$0111
    endm

WAITBLIT:macro
    tst DMACONR(a6)                         ;for compatibility
    btst #6,DMACONR(a6)
    bne.s *-6
    endm

*******************************************************************************
* GAME
*******************************************************************************

Init:
    movem.l d0-a6,-(sp)



    lea Screen,a1
    bsr.w ClearScreen

;    lea Logo,a0                             ;ptr to first bitplane of logo
;    lea CopBplP,a1                          ;where to poke the bitplane pointer words.
;    move #3-1,d0
;.bpll:
;    move.l a0,d1
;    swap d1
;    move.w d1,2(a1)                         ;hi word
;    swap d1
;    move.w d1,6(a1)                         ;lo word
;
;    addq #8,a1                              ;point to next bpl to poke in copper
;    lea logobpl(a0),a0
;    dbf d0,.bpll

    bsr DecodeOldMan

    lea DecodedGraphic,a0                   ;ptr to first bitplane of logo
    lea CopBplP,a1                          ;where to poke the bitplane pointer words.
    move #4-1,d0

.bpl7:
    move.l a0,d1
    swap d1
    move.w d1,2(a1)                         ;hi word
    swap d1
    move.w d1,6(a1)                         ;lo word

    addq #8,a1                              ;point to next bpl to poke in copper
    lea 40(a0),a0                           ;apparently every 40 bytes we'll have new bitplane data
    dbf d0,.bpl7



    movem.l (sp)+,d0-a6
    rts

*******************************************************************************
* START
*******************************************************************************

StartGame:
    bsr.w Init

    lea $DFF000,a6
    move.w #$87C0,DMACON(a6)                ;SET+BLTPRI+DMAEN+BPLEN+COPEN+BLTEN

    move.l #Copper,COP1LCH(a6)
    move.l #VBint,$6c(a4)                   ;set vertb interrupt vector compatibly.
    move.w #$c020,INTENA(a6)                ;enable interrupts generally
                                            ;and vertb specifically.


    bsr.s Main

    rts

*******************************************************************************
* MAIN
*******************************************************************************

Main:
    movem.l d0-a6,-(sp)

.WaitMouse
    btst #6,$bfe001
    bne.s .WaitMouse

    movem.l (sp)+,d0-a6
    rts

*******************************************************************************
* ROUTINES
*******************************************************************************

Extract8PixelPaletteValues:
    ;INPUT: d2 - source word (packed bytes)
    ;       a2 - 8 decoded palette indexes ptr
    ;USES:  d3
    ;OUTPUT: a2 - decoded pixels (8)

    ;BPL 1
    move.l d1,d3
    btst #0,d2
    beq .check_2

    or.b 3(a2),d3           ;3
    move.b d3,3(a2)

.check_2
    move.l d1,d3
    btst #1,d2
    beq .check_4

    or.b 2(a2),d3           ;2
    move.b d3,2(a2)

.check_4
    move.l d1,d3
    btst #2,d2
    beq .check_8

    or.b 1(a2),d3           ;1
    move.b d3,1(a2)

.check_8
    move.l d1,d3
    btst #3,d2
    beq .next_byte

    or.b (a2),d3            ;0
    move.b d3,(a2)

.next_byte
    move.l d1,d3
    ror d3
    ror d3
    ror d3
    ror d3
    btst #8,d2
    beq .check_2a

    or.b 7(a2),d3           ;7
    move.b d3,7(a2)

.check_2a
    move.l d1,d3
    ror d3
    ror d3
    ror d3
    ror d3
    btst #9,d2
    beq .check_4a

    or.b 6(a2),d3           ;6
    move.b d3,6(a2)

.check_4a
    move.l d1,d3
    ror d3
    ror d3
    ror d3
    ror d3
    btst #10,d2
    beq .check_8a

    or.b 5(a2),d3           ;5
    move.b d3,5(a2)

.check_8a
    move.l d1,d3
    ror d3
    ror d3
    ror d3
    ror d3
    btst #11,d2
    beq .next_bytea

    or.b 4(a2),d3           ;4
    move.b d3,4(a2)

.next_bytea

    ;BPL 0

    asr d1
    move.l d1,d3
    btst #4,d2
    beq .check_2b

    or.b 11(a2),d3           ;3
    move.b d3,11(a2)

.check_2b
    move.l d1,d3
    btst #5,d2
    beq .check_4b

    or.b 10(a2),d3           ;2
    move.b d3,10(a2)

.check_4b
    move.l d1,d3
    btst #6,d2
    beq .check_8b

    or.b 9(a2),d3           ;1
    move.b d3,9(a2)

.check_8b
    move.l d1,d3
    btst #7,d2
    beq .next_byteb

    or.b 8(a2),d3            ;0
    move.b d3,8(a2)

.next_byteb

    move.l d1,d3
    ror d3
    ror d3
    ror d3
    ror d3
    btst #12,d2
    beq .check_2c

    or.b 15(a2),d3           ;7
    move.b d3,15(a2)

.check_2c
    move.l d1,d3
    ror d3
    ror d3
    ror d3
    ror d3
    btst #13,d2
    beq .check_4c

    or.b 14(a2),d3           ;6
    move.b d3,14(a2)

.check_4c
    move.l d1,d3
    ror d3
    ror d3
    ror d3
    ror d3
    btst #14,d2
    beq .check_8c

    or.b 13(a2),d3           ;5
    move.b d3,13(a2)

.check_8c
    move.l d1,d3
    ror d3
    ror d3
    ror d3
    ror d3
    btst #15,d2
    beq .end

    or.b 12(a2),d3           ;4
    move.b d3,12(a2)

.end
    rts


DecodeRowOf16Pixels:
    ;INPUT: a1 - source bytes ptr
    ;USES:  d2
    ;OUTPUT: a2 - decoded pixels (8)
    ;        a3 - destination

    move.l 0,0(a2)
    move.l 0,4(a2)
                                                    ;leftmost columns of 8 pixels

    move.l #$20,d1
    move.w om_bp_offset(a1),d2                      ;om_bp_offset = offset to bitplanes 0 and 1 in source
                                                    ;Bitplane 01 - lower nybble; Bitplane 00 - upper nybble
    bsr Extract8PixelPaletteValues                  ;returns DecodedBitplaneBytes in a2

    move.l #$80,d1
    lea 2(a2),a2
    move.w (a1),d2
                                                    ;Bitplane 03 - lower nybble; Bitplane 02 - upper nybble
    bsr Extract8PixelPaletteValues                  ;returns DecodedBitplaneBytes in a2

    move.l #$20,d1
    lea 2(a2),a2
    move.w om_bp_offset+om_upr_px_b_off(a1),d2      ;add $20 to get to the src of the rightmost 8 pixel columns

    bsr Extract8PixelPaletteValues

    move.l #$80,d1
    lea 2(a2),a2
    move.w om_upr_px_b_off(a1),d2

    bsr Extract8PixelPaletteValues
    lea -6(a2),a2
    rts

;TILE 0x2E9                  0 (1)                       1 (2)                       2 (4)                       3 (8)
;00 00 00 00 | 12 35 55 54   00 00 00 00 | 10 11 11 10   00 00 00 00 | 01 10 00 00   00 00 00 00 | 00 01 11 11   00 00 00 00 | 00 00 00 00
;00 00 00 01 | 23 44 44 44   00 00 00 01 | 01 00 00 00   00 00 00 00 | 11 00 00 00   00 00 00 00 | 00 11 11 11   00 00 00 00 | 00 00 00 00
;02 00 00 01 | 23 33 44 44   00 00 00 01 | 01 11 00 00   01 00 00 00 | 11 11 00 00   00 00 00 00 | 00 00 11 11   00 00 00 00 | 00 00 00 00
;00 10 00 01 | 23 44 55 54   00 10 00 01 | 01 00 11 10   00 00 00 00 | 11 00 00 00   00 00 00 00 | 00 11 11 11   00 00 00 00 | 00 00 00 00
;00 30 00 01 | 22 22 22 33   00 10 00 01 | 00 00 00 11   00 10 00 00 | 11 11 11 11   00 00 00 00 | 00 00 00 00   00 00 00 00 | 00 00 00 00
;00 00 01 F9 | A5 55 5A 93   00 00 01 11 | 01 11 10 11   00 00 00 10 | 10 00 01 01   00 00 00 10 | 01 11 10 00   00 00 00 11 | 10 00 10 10
;33 22 12 9A | AA AA AA A2   11 00 10 10 | 00 00 00 00   11 11 01 01 | 11 11 11 11   00 00 00 00 | 00 00 00 00   00 00 00 11 | 11 11 11 10
;12 11 21 12 | 21 11 F9 F2   10 11 01 10 | 01 11 11 10   01 00 10 01 | 10 00 10 11   00 00 00 00 | 00 00 10 10   00 00 00 00 | 00 00 11 10
;21 1F 21 12 | 9A F9 A9 F3   01 11 01 10 | 10 11 01 11   10 01 10 01 | 01 10 10 11   00 01 00 00 | 00 10 00 10   00 01 00 00 | 11 11 11 10
;02 1F 21 12 | 21 11 11 14   01 10 01 10 | 01 11 11 10   01 01 10 01 | 10 00 00 00   00 01 00 00 | 00 00 00 01   00 01 00 00 | 00 00 00 00
;11 1F 12 12 | 32 22 22 14   11 11 10 10 | 10 00 00 10   00 01 01 01 | 11 11 11 00   00 01 00 00 | 00 00 00 01   00 01 00 00 | 00 00 00 00
;FF FF F1 11 | 23 43 32 35   11 11 11 11 | 01 01 10 11   11 11 10 00 | 11 01 11 10   11 11 10 00 | 00 10 00 01   11 11 10 00 | 00 00 00 00
;22 22 22 F9 | 12 32 21 25   00 00 00 11 | 10 10 01 01   11 11 11 10 | 01 11 10 10   00 00 00 10 | 00 00 00 01   00 00 00 11 | 00 00 00 00
;00 00 00 9A | A1 22 19 91   00 00 00 10 | 01 00 11 11   00 00 00 01 | 10 11 00 00   00 00 00 00 | 00 00 00 00   00 00 00 11 | 10 00 01 10
;00 00 00 9A | AA 99 AA A9   00 00 00 10 | 00 11 00 01   00 00 00 01 | 11 00 11 10   00 00 00 00 | 00 00 00 00   00 00 00 11 | 11 11 11 11
;10 00 00 AA | 5A A5 55 5A   10 00 00 00 | 10 01 11 10   00 00 00 11 | 01 10 00 01   00 00 00 00 | 10 01 11 10   00 00 00 11 | 01 10 00 01
;                                     00 | BE                     00 | 60                     00 | 1F                     00 | 00
;                                     01 | 40                     00 | C0                     00 | 3F                     00 | 00
;                                     01 | 70                     40 | F0                     00 | 0F                     00 | 00
;                                     21 | 4E                     00 | C0                     00 | 3F                     00 | 00
;                                     21 | 03                     20 | FF                     00 | 00                     00 | 00
;                                     07 | 7B                     02 | 85                     02 | 78                     03 | 8A
;                                     CA | 00                     F5 | FF                     00 | 00                     03 | FE
;                                     B6 | 7E                     49 | 8B                     00 | 0A                     00 | 0E
;                                     76 | B7                     99 | 6B                     10 | 22                     10 | FE
;                                     66 | 7E                     59 | 80                     10 | 01                     10 | 00
;                                     FA | 82                     15 | FC                     10 | 01                     10 | 00
;                                     FF | 5B                     F8 | DE                     F8 | 21                     F8 | 00
;                                     03 | A5                     FE | 7A                     02 | 01                     03 | 00
;                                     02 | 4F                     01 | B0                     00 | 00                     03 | 86
;                                     02 | 31                     01 | CE                     00 | 00                     03 | FF
;                                     80 | 9E                     03 | 61                     00 | 9E                     03 | 61
;00 BE 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 00 00 00 00 00 00 00
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 60 00 C0 40 F0 00 C0 20 FF 02 85 F5 FF 49 8B
;99 6B 59 80 15 FC F8 DE FE 7A 01 B0 01 CE 03 61
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 1F 00 3F 00 0F 00 3F 00 00 02 78 00 00 00 0A
;10 22 10 01 10 01 F8 21 02 01 00 00 00 00 00 9E
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 00 00 00 00 00 00 00 00 00 03 8A 03 FE 00 0E
;10 FE 10 00 10 00 F8 00 03 00 03 86 03 FF 03 61
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00


DecodeOldMan:

    lea DecodedGraphic,a0

    move.l #$2000,d0
.l0:
    clr.l (a0)+
    dbf d0,.l0

    lea DecodedGraphic,a3
    lea Oldguy,a1

    move #$0F,d0
.extract_tile_01:

    lea DecodedBitplaneBytes,a2
    bsr DecodeRowOf16Pixels

    move.w, 2(a2),(a3)          ;bitplane 0
    move.w, (a2),40(a3)         ;bitplane 1
    move.w, 6(a2),80(a3)        ;bitplane 2
    move.w, 4(a2),120(a3)       ;bitplane 3
    ;;move.w #$FFFF,(a3)            ;alone gives color #1
    ;move.w #$FFFF,40(a3)       ;alone gives color #2
    ;;move.w #$FFFF,80(a3)       ;alone gives color #4
    ;move.w #$FFFF,120(a3)      ;alone gives color #8

    lea $a0(a3),a3
    lea $02(a1),a1

    dbf d0,.extract_tile_01

    lea $20(a1),a1
    lea DecodedGraphic+2,a3
    move #$0F,d0
.extract_tile_02:

    lea DecodedBitplaneBytes,a2
    bsr DecodeRowOf16Pixels

    move.w, 2(a2),(a3)
    move.w, (a2),40(a3)
    move.w, 6(a2),80(a3)
    move.w, 4(a2),120(a3)
     ;;move.w #$FFFF,(a3)            ;alone gives color #1
    ;move.w #$FFFF,40(a3)       ;alone gives color #2
    ;;move.w #$FFFF,80(a3)       ;alone gives color #4
    ;move.w #$FFFF,120(a3)      ;alone gives color #8

    lea $a0(a3),a3
    lea $02(a1),a1

    dbf d0,.extract_tile_02

    lea $20(a1),a1
    lea DecodedGraphic+(160*16),a3
    move #$0F,d0
.extract_tile_03:

    lea DecodedBitplaneBytes,a2
    bsr DecodeRowOf16Pixels

    move.w, 2(a2),(a3)
    move.w, (a2),40(a3)
    move.w, 6(a2),80(a3)
    move.w, 4(a2),120(a3)

    ;move.w #$FFFF,0(a3)            ;alone gives color #1
    ;;move.w #$C000,2(a3)           ;alone gives color #1
    ;;move.w #$FFFF,40(a3)       ;alone gives color #2
    ;move.w #$FFFF,80(a3)       ;alone gives color #4
    ;;move.w #$C000,82(a3)       ;alone gives color #4
    ;;move.w #$FFFF,120(a3)      ;alone gives color #8

    lea $a0(a3),a3
    lea $02(a1),a1

    dbf d0,.extract_tile_03

    lea $20(a1),a1
    lea DecodedGraphic+(160*16)+2,a3
    move #$0F,d0
.extract_tile_04:

    lea DecodedBitplaneBytes,a2
    bsr DecodeRowOf16Pixels

    move.w, 2(a2),(a3)
    move.w, (a2),40(a3)
    move.w, 6(a2),80(a3)
    move.w, 4(a2),120(a3)
    ;move.w #$FFFF,0(a3)            ;alone gives color #1
    ;;move.w #$C000,2(a3)           ;alone gives color #1
    ;;move.w #$FFFF,40(a3)       ;alone gives color #2
    ;move.w #$FFFF,80(a3)       ;alone gives color #4
    ;;move.w #$C000,82(a3)       ;alone gives color #4
    ;;move.w #$FFFF,120(a3)      ;alone gives color #8

    lea $a0(a3),a3
    lea $02(a1),a1

    dbf d0,.extract_tile_04

    lea DecodedGraphic,a3
    lea Packed,a2

    move.l #$0c80,d0
.loadit
    move.b (a2)+,(a3)+

    dbf d0,.loadit

    rts

ClearScreen:                                ;a1=screen destination address to clear
    WAITBLIT
    clr.w $66(a6)                           ;destination modulo
    move.l #$01000000,$40(a6)               ;set operation type in BLTCON0/1
    move.l a1,$54(a6)                       ;destination address
    move.w #h*bpls*64+bpl/2,$58(a6)         ;blitter operation size
    rts

VBint:                                      ;Blank template VERTB interrupt
    movem.l d0-a6,-(sp)                     ;Save used registers
    lea $DFF000,a6
    btst #5,INTREQR(a6)                     ;INTREQR check if it's our vertb int.
    beq.s .notvb

    moveq #$20,d0                           ;poll irq bit
    move.w d0,INTREQ(a6)
    move.w d0,INTREQ(a6)

.notvb:
    movem.l (sp)+,d0-a6                     ;restore
    rte

    even

*******************************************************************************
* DATA (FASTMEM)
*******************************************************************************

SkyBufferL:
    dc.l 0
    dc.l 0
SkyBufferLE:

DecodedBitplaneBytes:
    dc.b 0,0,0,0,0,0,0,0

*******************************************************************************
* CHIPMEM
*******************************************************************************

    SECTION AllData,DATA_C

Copper:
    dc.w $01fc,0                            ;slow fetch mode, AGA compatibility
    dc.w $0100,$0200
    dc.b 0,$8e,$2c,$81
    dc.b 0,$90,$2c,$c1
    dc.w $0092,$38
    dc.w $0094,$d0

    dc.w $0108,(320/8)*3
    dc.w $010a,(320/8)*3

    dc.w $0102,0
    dc.w $0104,0

    copper_pal_03

CopBplP:
    dc.w $00e0,0                            ;1
    dc.w $00e2,0
    dc.w $00e4,0                            ;2
    dc.w $00e6,0
    dc.w $00e8,0                            ;3
    dc.w $00ea,0
    dc.w $00ec,0                            ;4
    dc.w $00ee,0
;   dc.w $00f0,0                            ;5
;   dc.w $00f2,0
;   dc.w $00f4,0                            ;6
;   dc.w $00f6,0

    dc.w $0100,$4200
    dc.w $4f07,$fffe

    copper_pal_03

    dc.w $9207,$fffe

    dc.w $0180,$044f,$0182,$068e,$0184,$0adf,$0186,$0dff
    dc.w $0188,$09bf,$018a,$056d,$018c,$044b,$018e,$033a

    dc.w $ffff,$fffe
CopperE:

Oldguy: INCBIN "gfx/testgrfx.bin"
    dcb.b logobwid*6,0

Packed: INCBIN "gfx/interleavedpic.bin"
    dcb.b logobwid*6,0

Logo:   INCBIN "gfx/sky3centered.raw"
LogoE:
    dcb.b logobwid*6,0

    EVEN

*******************************************************************************
* BUFFERS
*******************************************************************************

    SECTION AllBuffers,BSS_C

Screen:
    ds.b bplsize*4
ScreenE:

    EVEN

DecodedGraphic:
    ds.b $2800            ;Define storage for graphic
DecodedGraphicE:


