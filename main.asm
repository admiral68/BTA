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

Extract8BitplaneBytesFromTwoSourceBytes:
    ;INPUT: d2 - source word (packed bytes) ;d2.w = zeroAndOneByte1 & zeroAndOneByte2 OR twoAndThreeByte1 & twoAndThreeByte2
    ;       a2 - 8 decoded palette indexes ptr
    ;USES:
    ;OUTPUT: a2 - decoded bitplane bytes (1 & 0 or 3 & 2)

    ;first byte of d2   => bitToSet => 0x10 and higher
    ;second byte of d2  => bitToSet => 0x01 and higher

    ;var destIndex = (rowIndex % 0x10) * 2 + (rowIndex >= 0x10 ? 1 : 0);

    ;BITS 0-7  are in srcByte2
    ;BITS 8-15 are in srcByte1

    ;BPL 1
    btst #0,d2
    beq .check_2

    or.b #$01,(a2)

.check_2
    btst #1,d2
    beq .check_4

    or.b #$02,(a2)

.check_4
    btst #2,d2
    beq .check_8

    or.b #$04,(a2)

.check_8
    btst #3,d2
    beq .next_byte

    or.b #$08,(a2)

.next_byte
    btst #8,d2
    beq .check_2a

    or.b #$10,(a2)

.check_2a
    btst #9,d2
    beq .check_4a

    or.b #$20,(a2)

.check_4a
    btst #10,d2
    beq .check_8a

    or.b #$40,(a2)

.check_8a
    btst #11,d2
    beq .next_bytea

    or.b #$80,(a2)

.next_bytea

    ;BPL 0

    btst #4,d2
    beq .check_2b

    or.b #$01,1(a2)

.check_2b
    btst #5,d2
    beq .check_4b

    or.b #$02,1(a2)

.check_4b
    btst #6,d2
    beq .check_8b

    or.b #$04,1(a2)

.check_8b
    btst #7,d2
    beq .next_byteb

    or.b #$08,1(a2)
    or.b 8(a2),d3            ;0
    move.b d3,8(a2)

.next_byteb
    btst #12,d2
    beq .check_2c

    or.b #$10,1(a2)

.check_2c
    btst #13,d2
    beq .check_4c

    or.b #$20,1(a2)

.check_4c
    btst #14,d2
    beq .check_8c

    or.b #$40,1(a2)

.check_8c
    btst #15,d2
    beq .end

    or.b #$80,1(a2)

.end
    rts


DecodeRowOf16Pixels:
    ;INPUT: a1 - source bytes ptr
    ;USES:  d2
    ;OUTPUT: a2 - bitplane data
    ;        a3 - destination

    move.l 0,0(a2)                                  ;leftmost column destinations
    move.l 0,4(a2)                                  ;rightmost column destinations


                                                    ;leftmost columns of 8 pixels

    move.w om_bp_offset(a1),d2                      ;om_bp_offset = offset to bitplanes 0 and 1 in source
                                                    ;d2.w = zeroAndOneByte1 & zeroAndOneByte2
                                                    ;a2 => Bitplane 01 - lower byte; Bitplane 00 - upper byte
    bsr Extract8BitplaneBytesFromTwoSourceBytes     ;returns DecodedBitplaneBytes in a2

    lea 2(a2),a2
    move.w (a1),d2                                  ;d2.w = twoAndThreeByte1 & twoAndThreeByte2
                                                    ;a2 => Bitplane 03 - lower byte; Bitplane 02 - upper byte
    bsr Extract8BitplaneBytesFromTwoSourceBytes     ;returns DecodedBitplaneBytes in a2


                                                    ;rightmost columns of 8 pixels

    lea 2(a2),a2
    move.w om_bp_offset+om_upr_px_b_off(a1),d2      ;add $20 to get to the src of the rightmost 8 pixel columns
                                                    ;d2.w = zeroAndOneByte1 & zeroAndOneByte2
                                                    ;a2 => Bitplane 01 - lower byte; Bitplane 00 - upper byte

    bsr Extract8BitplaneBytesFromTwoSourceBytes

    lea 2(a2),a2
    move.w om_upr_px_b_off(a1),d2                   ;d2.w = twoAndThreeByte1 & twoAndThreeByte2
                                                    ;a2 => Bitplane 03 - lower byte; Bitplane 02 - upper byte

    bsr Extract8BitplaneBytesFromTwoSourceBytes
    lea -6(a2),a2
    rts

DecodeOldMan:
    ;SCREEN LO-RES
    ;W: 320
    ;8 pixels/byte
    ;40 bytes per line/20 words per line

    lea DecodedGraphic,a0

    move.l #$1400,d0
.l0:
    clr.l (a0)+
    dbf d0,.l0

    lea DecodedGraphic,a3
    lea Oldguy,a1

    ; ******************************************************

    move #$0F,d0                ;16 rasterlines at a time; rightmost byte done too
.extract_tile_01:

    lea DecodedBitplaneBytes,a2
    bsr DecodeRowOf16Pixels

    ;    0  1  2  3   4  5  6  7
    ;a2: 3, 2, 1, 0 | 3, 2, 1, 0

    move.b, 3(a2),(a3)          ;bitplane 0
    move.b, 2(a2),40(a3)        ;bitplane 1
    move.b, 1(a2),80(a3)        ;bitplane 2
    move.b, (a2),120(a3)        ;bitplane 3
    move.b, 7(a2),1(a3)         ;bitplane 0
    move.b, 6(a2),41(a3)        ;bitplane 1
    move.b, 5(a2),81(a3)        ;bitplane 2
    move.b, 4(a2),121(a3)       ;bitplane 3

    lea $a0(a3),a3              ;move down one rasterline (a0 = $28 * 4 bitplanes; $28 = bytes in one rasterline for one bitplane)
    lea $02(a1),a1

    dbf d0,.extract_tile_01

    ; ******************************************************

    lea $20(a1),a1
    lea DecodedGraphic+2,a3
    move #$0F,d0
.extract_tile_02:

    lea DecodedBitplaneBytes,a2
    bsr DecodeRowOf16Pixels

    move.b, 3(a2),(a3)          ;bitplane 0
    move.b, 2(a2),40(a3)        ;bitplane 1
    move.b, 1(a2),80(a3)        ;bitplane 2
    move.b, (a2),120(a3)        ;bitplane 3
    move.b, 7(a2),1(a3)         ;bitplane 0
    move.b, 6(a2),41(a3)        ;bitplane 1
    move.b, 5(a2),81(a3)        ;bitplane 2
    move.b, 4(a2),121(a3)       ;bitplane 3

    lea $a0(a3),a3              ;move down one rasterline
    lea $02(a1),a1

    dbf d0,.extract_tile_02

    ; ******************************************************

    lea $20(a1),a1
    lea DecodedGraphic+(160*16),a3
    move #$0F,d0
.extract_tile_03:

    lea DecodedBitplaneBytes,a2
    bsr DecodeRowOf16Pixels

    move.b, 3(a2),(a3)          ;bitplane 0
    move.b, 2(a2),40(a3)        ;bitplane 1
    move.b, 1(a2),80(a3)        ;bitplane 2
    move.b, (a2),120(a3)        ;bitplane 3
    move.b, 7(a2),1(a3)         ;bitplane 0
    move.b, 6(a2),41(a3)        ;bitplane 1
    move.b, 5(a2),81(a3)        ;bitplane 2
    move.b, 4(a2),121(a3)       ;bitplane 3

    lea $a0(a3),a3              ;move down one rasterline
    lea $02(a1),a1

    dbf d0,.extract_tile_03

    ; ******************************************************

    lea $20(a1),a1
    lea DecodedGraphic+(160*16)+2,a3
    move #$0F,d0
.extract_tile_04:

    lea DecodedBitplaneBytes,a2
    bsr DecodeRowOf16Pixels

    move.b, 3(a2),(a3)          ;bitplane 0
    move.b, 2(a2),40(a3)        ;bitplane 1
    move.b, 1(a2),80(a3)        ;bitplane 2
    move.b, (a2),120(a3)        ;bitplane 3
    move.b, 7(a2),1(a3)         ;bitplane 0
    move.b, 6(a2),41(a3)        ;bitplane 1
    move.b, 5(a2),81(a3)        ;bitplane 2
    move.b, 4(a2),121(a3)       ;bitplane 3

    lea $a0(a3),a3              ;move down one rasterline
    lea $02(a1),a1

    dbf d0,.extract_tile_04
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
    ds.b $1400            ;Define storage for graphic
DecodedGraphicE:


