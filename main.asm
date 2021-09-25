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
bwid            =bpls*bpl               ;byte-width of 1 pixel line (all bpls)


om_bp_offset    = $100
om_src_rows     = 2
om_src_cols     = 2
om_tile_b_offs  = $40
om_upr_px_b_off = $20
om_upr_px_offs  = 8


    *-----------------*
    * logo dimensions *
    *-----------------*

logow           =320
logomargin      =(320-logow)/2
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

    lea Logo,a0                             ;ptr to first bitplane of logo
    lea CopBplP,a1                          ;where to poke the bitplane pointer words.
    move #3-1,d0
.bpll:
    move.l a0,d1
    swap d1
    move.w d1,2(a1)                         ;hi word
    swap d1
    move.w d1,6(a1)                         ;lo word

    addq #8,a1                              ;point to next bpl to poke in copper
    lea logobpl(a0),a0
    dbf d0,.bpll

    bsr DecodeOldMan

;    lea DecodedGraphic,a0                   ;ptr to first bitplane of logo
;    lea CopBplP,a1                          ;where to poke the bitplane pointer words.
;    move #4-1,d0
;
;.bpl7:
;    move.l a0,d1
;    swap d1
;    move.w d1,2(a1)                         ;hi word
;    swap d1
;    move.w d1,6(a1)                         ;lo word
;
;    addq #8,a1                              ;point to next bpl to poke in copper
;    lea 4(a0),a0							;apparently every 4 bytes we'll have new bitplane data
;    dbf d0,.bpl7


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
    ;INPUT: d1 - contribution
    ;       d2 - source word (packed bytes)
    ;       a1 - source bytes ptr
    ;       a2 - 8 decoded palette indexes ptr
    ;USES:  d3
    ;OUTPUT: a2 - decoded pixels (8)

    move.l 0,0(a2)
    move.l 0,4(a2)
    move.l 0,8(a2)
    move.l 0,12(a2)

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

    ;swap.b d2                 ;swaps source bytes

    move.l d1,d3

    btst #12,d2
    beq .check_2a

    or.b 7(a2),d3           ;7
    move.b d3,7(a2)

.check_2a
    move.l d1,d3
    btst #13,d2
    beq .check_4a

    or.b 6(a2),d3           ;6
    move.b d3,6(a2)

.check_4a
    move.l d1,d3
    btst #14,d2
    beq .check_8a

    or.b 5(a2),d3           ;5
    move.b d3,5(a2)

.check_8a
    move.l d1,d3
    btst #15,d2
    beq .next_bytea

    or.b 4(a2),d3           ;4
    move.b d3,4(a2)

.next_bytea

    ;swap d2             ;swaps source bytes

    move.l d1,d3
    asr d3

    btst #0,d2
    beq .check_2b

    or.b 3(a2),d3           ;3
    move.b d3,3(a2)

.check_2b
    move.l d1,d3
    btst #1,d2
    beq .check_4b

    or.b 2(a2),d3           ;2
    move.b d3,2(a2)

.check_4b
    move.l d1,d3
    btst #2,d2
    beq .check_8b

    or.b 1(a2),d3           ;1
    move.b d3,1(a2)

.check_8b
    move.l d1,d3
    btst #3,d2
    beq .next_byteb

    or.b (a2),d3            ;0
    move.b d3,(a2)

.next_byteb

    ;swap d2             ;swaps source bytes

    move.l d1,d3
    asr d3

    btst #12,d2
    beq .check_2c

    or.b 7(a2),d3           ;7
    move.b d3,7(a2)

.check_2c
    move.l d1,d3
    btst #13,d2
    beq .check_4c

    or.b 6(a2),d3           ;6
    move.b d3,6(a2)

.check_4c
    move.l d1,d3
    btst #14,d2
    beq .check_8c

    or.b 5(a2),d3           ;5
    move.b d3,5(a2)

.check_8c
    move.l d1,d3
    btst #15,d2
    beq .end

    or.b 4(a2),d3           ;4
    move.b d3,4(a2)

.end
    rts


DecodeRowOfPixels:
    ;INPUT: a1 - source bytes ptr
    ;USES:  d2
    ;OUTPUT: a2 - decoded pixels (16)

    move.w om_bp_offset(a1),d2                      ;om_bp_offset = offset to bitplanes 0 and 1 in source
    move.w #8,d1
    lea DecodedPaletteIndexes,a2


                                                    ;Bitplane 01 - lower nybble; Bitplane 00 - upper nybble
    bsr Extract8PixelPaletteValues                  ;returns DecodedPaletteIndexes in a2

    move.w (a1),d2
    move.w #8,d1
    lea DecodedPaletteIndexes+8,a2


                                                    ;Bitplane 03 - lower nybble; Bitplane 02 - upper nybble
    bsr Extract8PixelPaletteValues                  ;returns DecodedPaletteIndexes in a2
    rts


DecodeOldMan:

	lea DecodedGraphic,a0
	move.l #512,d0
.l0:	clr.l (a0)+
	dbf d0,.l0
	

	lea DecodedGraphic,a3
    lea Oldguy,a1

    move #om_tile_b_offs/2,d0
.extract_tile_01:

    bsr DecodeRowOfPixels
                                ;TODO: PUT RETURNED PALETTE INDEXES INTO NEW BITMAP
                                ;TODO: here's where we need to check cmp.w $10,d0


    dbf d0,.extract_tile_01

    move.l $40(a1),(a1)         ;a1 += om_tile_b_offs; // Advance to next raw source bytes for next tile


    move #om_tile_b_offs/2,d0
.extract_tile_02:

    bsr DecodeRowOfPixels
                                ;TODO: PUT RETURNED PALETTE INDEXES INTO NEW BITMAP
                                ;TODO: here's where we need to check cmp.w $10,d0

    dbf d0,.extract_tile_02

    move.l $40(a1),(a1)         ;a1 += om_tile_b_offs; // Advance to next raw source bytes for next tile



    move #om_tile_b_offs/2,d0
.extract_tile_03:

    bsr DecodeRowOfPixels
                                ;TODO: PUT RETURNED PALETTE INDEXES INTO NEW BITMAP
                                ;TODO: here's where we need to check cmp.w $10,d0

    dbf d0,.extract_tile_03

    move.l $40(a1),(a1)         ;a1 += om_tile_b_offs; // Advance to next raw source bytes for next tile



    move #om_tile_b_offs/2,d0
.extract_tile_04:

    bsr DecodeRowOfPixels
                                ;TODO: PUT RETURNED PALETTE INDEXES INTO NEW BITMAP
                                ;TODO: here's where we need to check cmp.w $10,d0

    dbf d0,.extract_tile_04




;om_bp_offset   = $100
;om_src_rows        = 2
;om_src_cols        = 2
;om_tile_b_offs = $40
;om_upr_px_b_off    = $20
;om_upr_px_offs = 8




    ;cmp.w $10,d0               ;var pxOffset = d0 < (om_upr_px_b_off / 2) ? 0 : om_upr_px_offs;
    ;ble    .part2                  ;pixel columns 0-7 are not offset;

    ;pxOffset = om_upr_px_offs;

    ;pixel columns 8-F are offset by upperPixelsByteOffset

;.part2





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

DecodedPaletteIndexes:
    dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

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

    dc.w $0108,logobwid-logobpl
    dc.w $010a,logobwid-logobpl

    dc.w $0102,$20
    dc.w $0104,0

    dc.w $0180,$044f,$0182,$068e,$0184,$0adf,$0186,$0dff
    dc.w $0188,$09bf,$018a,$056d,$018c,$044b,$018e,$033a

    dc.w $19df,$fffe



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


    ;dc.w $0180,logobgcol


    dc.w $0100,$3200
    dc.w $4a07,$fffe

    dc.w $0180,$0b87,$0182,$0754,$0184,$0975,$0186,$0ca8
    dc.w $0188,$0ed8,$018a,$0fff,$018c,$0060,$018e,$0090
    dc.w $0190,$00e0,$0192,$0777,$0194,$0aaa,$0196,$0747
    dc.w $0198,$0868,$019a,$0a8a,$019c,$0cac,$019e,$0111

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
    ds.b bplsize*3
ScreenE:

	EVEN

DecodedGraphic:
    ds.b 512            ;Define storage for graphic
DecodedGraphicE:


