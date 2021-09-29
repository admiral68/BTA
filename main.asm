    INCDIR ""
    INCLUDE "photon/PhotonsMiniWrapper1.04!.S"
    INCLUDE "photon/Blitter-Register-List.S"

*******************************************************************************
* DEFINES
*******************************************************************************

w                           = 320
h                           = 256
bplsize                     = w*h/8

tile_bitplanes              = 4
tile_height                 = 16

tile_index_mask             = $07ff

tilesrc_row_w               = $200
tilesrc_bp_offset           = $20000
tilesrc_upr_px_b_off        = $20

bitpl_bytes_per_raster_line = 40

bpls                        = 3                             ;handy values:
bpl                         = w/16*2                        ;byte-width of 1 bitplane line

test_vlines_per_graphic     = 48                            ;32

    *-----------------*
    * logo dimensions *
    *-----------------*

bytewidth       =(w/8)*3

    *-----------------*
    * palettes        *
    *-----------------*


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

tile_pal_00:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0000
    dc.w $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0110
    endm

tile_pal_01:macro
    dc.w $0180,$0b87,$0182,$0433,$0184,$0842,$0186,$0a53
    dc.w $0188,$0c64,$018a,$0db8,$018c,$0974,$018e,$0754
    dc.w $0190,$0644,$0192,$0c95,$0194,$0f85,$0196,$0ffa
    dc.w $0198,$0cca,$019a,$0998,$019c,$0666,$019e,$0111
    endm

tile_pal_02:macro
    dc.w $0180,$0b87,$0182,$0544,$0184,$0754,$0186,$0975
    dc.w $0188,$0ca8,$018a,$0eea,$018c,$0fc4,$018e,$0653
    dc.w $0190,$0974,$0192,$0c84,$0194,$0eef,$0196,$0aaa
    dc.w $0198,$0889,$019a,$0778,$019c,$0556,$019e,$0111
    endm

tile_pal_03:macro
    dc.w $0180,$0b87,$0182,$0754,$0184,$0975,$0186,$0ca8
    dc.w $0188,$0ed8,$018a,$0fff,$018c,$0060,$018e,$0090
    dc.w $0190,$00e0,$0192,$0777,$0194,$0aaa,$0196,$0747
    dc.w $0198,$0868,$019a,$0a8a,$019c,$0cac,$019e,$0111
    endm

tile_pal_04:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_05:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_06:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_07:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_08:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_09:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_0a:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_0b:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_0c:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_0d:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_0e:macro
    dc.w $0180,$0xxx,$0182,$0xxx,$0184,$0xxx,$0186,$0xxx
    dc.w $0188,$0xxx,$018a,$0xxx,$018c,$0xxx,$018e,$0xxx
    dc.w $0190,$0xxx,$0192,$0xxx,$0194,$0xxx,$0196,$0xxx
    dc.w $0198,$0xxx,$019a,$0xxx,$019c,$0xxx,$019e,$0xxx
    endm

tile_pal_0f:macro
    dc.w $0180,$0111,$0182,$0FF9,$0184,$0EC7,$0186,$0DA6
    dc.w $0188,$0C85,$018a,$0A74,$018c,$0864,$018e,$0753
    dc.w $0190,$0641,$0192,$0533,$0194,$0431,$0196,$0111
    dc.w $0198,$0111,$019a,$0111,$019c,$0111,$019e,$0110
    endm

WAITBLIT:macro
    tst DMACONR(a6)                                         ;for compatibility
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


; some test code

    ;lea TilesToDecode,a1
    ;move.w #$02b4,(a1)+
    ;move.w #$02bc,(a1)+
    ;move.w #$02a0,(a1)+
    ;move.w #$02e9,(a1)+
    ;move.w #$02ea,(a1)+
    ;move.w #$02bc,(a1)+
    ;move.w #$02ba,(a1)+
    ;move.w #$02a2,(a1)+
    ;move.w #$02a3,(a1)+
    ;move.w #$02f0,(a1)+
    ;move.w #$02f1,(a1)+
    ;move.w #$02f2,(a1)+
    ;move.w #$02f3,(a1)+
    ;move.w #$02a3,(a1)+
    ;move.w #$02aa,(a1)+
    ;move.w #$02ab,(a1)+
    ;move.w #$02f8,(a1)+
    ;move.w #$02f9,(a1)+
    ;move.w #$02fa,(a1)+
    ;move.w #$02fb,(a1)+
    ;move.w #$02ab,(a1)+

    ;In the source, the tile scroll data is formatted in blocks of tiles 16 x 16. These blocks are $200 bytes in size a piece.
    ;"8x4" means 8 of these big blocks wide by 4 of those blocks high

    ;The Black Tiger screen was only 256x224, meaning that a maximum of 16 columns and 14 rows of tiles could be displayed
    ;on screen at once anyway. I need to check to see if the game actually draws that many columns, or if there is other space
    ;reserved for HUD (top/bottom/etc)

    ; HB+(BITS0-2 of LB << 8) = TILE SELECTION  ((0x7 & LB) << 8) + HB    ==> INDEX VALUES BETWEEN 0x000 & 0x7FF
    ; (BITS3-6 of LB)         = PALETTE INDEX                             ==> INDEX VALUES BETWEEN 0x0 & 0xF
    ; BIT 7 of LB:            = FLIP HORIZONTALLY
    ;
    ; ATTR:
    ;   BITS 0-2 = UPPER 3 BITS OF TILE INDEX (ABOVE LOWER 8 bits of index--so we can index up to 0x7FF)
    ;   BITS 3-6 = COLOR INFO (0x0 - 0xF) (16 possible PALETTES) (raw PALETTE index)
    ;   BIT  7   = FLIP TILE (0x0 = NO, 0x1 = YES)

    move.l bitpl_bytes_per_raster_line*tile_bitplanes*test_vlines_per_graphic,d0

    lea TileColumnsToDecode,a1
    move.b #20,(a1)                                         ;7

    lea TileRowsToDecode,a1
    move.b #16,(a1)                                         ;3

    lea DestGraphicVTileOffset,a1                           ;One tile height in destination bitmap
    move.l #bitpl_bytes_per_raster_line*tile_bitplanes*tile_height,(a1)

    lea EncTiles,a0
    lea TileSource,a1
    move.l a0,(a1)

    move.l #0,d2
    move.l #0,d3
    move.l #0,d4

    lea TilesToDecode,a2
    lea ScrollDataLev1,a1
    movea.l a1,a3
    move.l #15,d0

.outer_loop
    move.l #19,d1

.inner_loop
    move.w (a1)+,d2                                         ;load "scroll word" into d2 from a1
    ror #8,d2                                               ;swap bytes; source is little endian
    move.w d2,d3
    and.w #tile_index_mask,d3

    btst #15,d2
    beq .finish_inner_loop

    or.w #$8000,d3                                          ;set "flipped" tile index

.finish_inner_loop
    move.w d3,(a2)+                                         ;poke this into the tile list

    addi.w #1,d4

    cmp.b #16,d4
    bne .skip_1
    lea $1e0(a1),a1

.skip_1
    dbf d1,.inner_loop

    move.l #0,d4
    lea $20(a3),a3
    movea.l a3,a1

    dbf d0,.outer_loop

    bsr DecodeTileGraphicToScreen

    lea DecodedGraphic,a0                                   ;ptr to first bitplane of logo
    lea CopBplP,a1                                          ;where to poke the bitplane pointer words.
    move #4-1,d0

.bpl7:
    move.l a0,d1
    swap d1
    move.w d1,2(a1)                                         ;hi word
    swap d1
    move.w d1,6(a1)                                         ;lo word

    addq #8,a1                                              ;point to next bpl to poke in copper
    lea bitpl_bytes_per_raster_line(a0),a0                  ;apparently every 40 bytes we'll have new bitplane data
    dbf d0,.bpl7

;test code ends

    movem.l (sp)+,d0-a6
    rts

*******************************************************************************
* START
*******************************************************************************

StartGame:
    bsr.w Init

    lea $DFF000,a6
    move.w #$87C0,DMACON(a6)                                ;SET+BLTPRI+DMAEN+BPLEN+COPEN+BLTEN

    move.l #Copper,COP1LCH(a6)
    move.l #VBint,$6c(a4)                                   ;set vertb interrupt vector compatibly.
    move.w #$c020,INTENA(a6)                                ;enable interrupts generally
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
    ;INPUT:  d2 - source word (packed bytes) ;d2.w = zeroAndOneByte1 & zeroAndOneByte2 OR twoAndThreeByte1 & twoAndThreeByte2
    ;        a2 - 8 decoded palette indexes ptr
    ;OUTPUT: a2 - decoded bitplane bytes (1 & 0 or 3 & 2)

    ;first byte of d2   => bitToSet => 0x10 and higher
    ;second byte of d2  => bitToSet => 0x01 and higher

    ;BITS 0-7  are in srcByte2
    ;BITS 8-15 are in srcByte1

    ;BPL 1
    btst #0,d2
    beq .byte_1_bit_1

    or.b #$01,(a2)

.byte_1_bit_1
    btst #1,d2
    beq .byte_1_bit_2

    or.b #$02,(a2)

.byte_1_bit_2
    btst #2,d2
    beq .byte_1_bit_3

    or.b #$04,(a2)

.byte_1_bit_3
    btst #3,d2
    beq .byte_2_bit_0

    or.b #$08,(a2)

.byte_2_bit_0
    btst #8,d2
    beq .byte_2_bit_1

    or.b #$10,(a2)

.byte_2_bit_1
    btst #9,d2
    beq .byte_2_bit_2

    or.b #$20,(a2)

.byte_2_bit_2
    btst #10,d2
    beq .byte_2_bit_3

    or.b #$40,(a2)

.byte_2_bit_3
    btst #11,d2
    beq .byte_1_bit_4

    or.b #$80,(a2)

    ;BPL 0

.byte_1_bit_4
    btst #4,d2
    beq .byte_1_bit_5

    or.b #$01,1(a2)

.byte_1_bit_5
    btst #5,d2
    beq .byte_1_bit_6

    or.b #$02,1(a2)

.byte_1_bit_6
    btst #6,d2
    beq .byte_1_bit_7

    or.b #$04,1(a2)

.byte_1_bit_7
    btst #7,d2
    beq .byte_2_bit_4

    or.b #$08,1(a2)

.byte_2_bit_4
    btst #12,d2
    beq .byte_2_bit_5

    or.b #$10,1(a2)

.byte_2_bit_5
    btst #13,d2
    beq .byte_2_bit_6

    or.b #$20,1(a2)

.byte_2_bit_6
    btst #14,d2
    beq .byte_2_bit_7

    or.b #$40,1(a2)

.byte_2_bit_7
    btst #15,d2
    beq .end

    or.b #$80,1(a2)

.end
    rts
;-----------------------------------------------

DecodeRowOf16Pixels:
    ;INPUT: a1 - source bytes ptr
    ;USES:  d2,a4
    ;OUTPUT: a2 - bitplane data

    move.l 0,0(a2)                                          ;leftmost column destinations
    move.l 0,4(a2)                                          ;rightmost column destinations

    move.l a1,d2
    add.l #tilesrc_bp_offset,d2                             ;tilesrc_bp_offset = offset to bitplanes 0 and 1 in source
    move.l d2,a4

                                                            ;leftmost columns of 8 pixels
    move.l #0,d2
    move.w (a4),d2

                                                            ;d2.w = zeroAndOneByte1 & zeroAndOneByte2
                                                            ;a2 => Bitplane 01 - lower byte; Bitplane 00 - upper byte
    bsr Extract8BitplaneBytesFromTwoSourceBytes             ;returns DecodedBitplaneBytes in a2

    lea 2(a2),a2
    move.l #0,d2
    move.w (a1),d2                                          ;d2.w = twoAndThreeByte1 & twoAndThreeByte2


                                                            ;a2 => Bitplane 03 - lower byte; Bitplane 02 - upper byte
    bsr Extract8BitplaneBytesFromTwoSourceBytes             ;returns DecodedBitplaneBytes in a2


                                                            ;rightmost columns of 8 pixels

    lea 2(a2),a2
    move.l #0,d2
    move.w tilesrc_upr_px_b_off(a4),d2                      ;add $20 to get to the src of the rightmost 8 pixel columns


                                                            ;d2.w = zeroAndOneByte1 & zeroAndOneByte2
                                                            ;a2 => Bitplane 01 - lower byte; Bitplane 00 - upper byte

    bsr Extract8BitplaneBytesFromTwoSourceBytes

    lea 2(a2),a2
    move.l #0,d2
    move.w tilesrc_upr_px_b_off(a1),d2                      ;d2.w = twoAndThreeByte1 & twoAndThreeByte2


                                                            ;a2 => Bitplane 03 - lower byte; Bitplane 02 - upper byte

    bsr Extract8BitplaneBytesFromTwoSourceBytes
    lea -6(a2),a2
    rts

;-----------------------------------------------
ExtractTile:
    ;INPUT:  a1 - source bytes ptr
    ;        a3 - destination ptr
    ;        d1 - flipped = 1.b, not flipped = 0.b
    ;USES:   a2
    ;OUTPUT: a3 - destination

    move #$0F,d0                                            ;16 rasterlines at a time; rightmost byte done too
.extract_tile:

    lea DecodedBitplaneBytes,a2                             ;stores intermediate decoded bitplane bytes
    bsr DecodeRowOf16Pixels

    ;    0  1  2  3   4  5  6  7
    ;a2: 3, 2, 1, 0 | 3, 2, 1, 0

    ;To flip the tile horizontally, we need to reverse the 4 bit color indexes,
    ;which means reversing the NYBBLES--not the bits. Reversing the bits gives
    ;us swapping of the odd bitplanes, which messes up the colors

    cmp.b #1,d1
    bne .no_flip

    swap d1

    move.b 3(a2),d6
    move.b 7(a2),d1
    REPT 8
    roxr.b #1,d1
    addx.b d6,d6
    ENDR
    roxr.b #1,d1
    move.b d6,3(a2)
    move.b d1,7(a2)

    move.b 2(a2),d6
    move.b 6(a2),d1
    REPT 8
    roxr.b #1,d1
    addx.b d6,d6
    ENDR
    roxr.b #1,d1
    move.b d6,2(a2)
    move.b d1,6(a2)

    move.b 1(a2),d6
    move.b 5(a2),d1
    REPT 8
    roxr.b #1,d1
    addx.b d6,d6
    ENDR
    roxr.b #1,d1
    move.b d6,1(a2)
    move.b d1,5(a2)

    move.b (a2),d6
    move.b 4(a2),d1
    REPT 8
    roxr.b #1,d1
    addx.b d6,d6
    ENDR
    roxr.b #1,d1
    move.b d6,(a2)
    move.b d1,4(a2)

    swap d1

.no_flip

    move.b, 3(a2),bitpl_bytes_per_raster_line*0(a3)         ;bitplane 0
    move.b, 2(a2),bitpl_bytes_per_raster_line*1(a3)         ;bitplane 1
    move.b, 1(a2),bitpl_bytes_per_raster_line*2(a3)         ;bitplane 2
    move.b, (a2),bitpl_bytes_per_raster_line*3(a3)          ;bitplane 3
    move.b, 7(a2),bitpl_bytes_per_raster_line*0+1(a3)       ;bitplane 0
    move.b, 6(a2),bitpl_bytes_per_raster_line*1+1(a3)       ;bitplane 1
    move.b, 5(a2),bitpl_bytes_per_raster_line*2+1(a3)       ;bitplane 2
    move.b, 4(a2),bitpl_bytes_per_raster_line*3+1(a3)       ;bitplane 3

    lea bitpl_bytes_per_raster_line*tile_bitplanes(a3),a3   ;move down one rasterline (a0 = $28 * 4 bitplanes; $28 = bytes in one rasterline for one bitplane)
    lea $02(a1),a1                                          ;source bytes per rasterline are 2 bytes apart

    dbf d0,.extract_tile
    rts

;-----------------------------------------------
DecodeTileGraphicToScreen:
    ;SCREEN LO-RES
    ;W: 320
    ;8 pixels/byte
    ;40 bytes per line/20 words per line
    ;done to show how to extract Black Tiger-encoded image data
    ;40*32*4

    lea DecodedGraphic,a0
    lea TileDecodeRowDest,a1

    move.l a0,(a1)

.l0:
    clr.l (a0)+
    dbf d0,.l0

    clr.l d0

    lea TilesToDecode,a0                                    ;Starting tile
    lea TileDecodeDest,a2
    lea TileDecodeRowDest,a1

    move.l (a1),(a2)
    move.l #0,d5

.loop_rows
    move.l #0,d4

.loop_columns
    move.l (a2),a3

    move.b #0,d1
    lea TileSource,a4
    move.l (a4),a1
    move.w (a0)+,d0
    btst #15,d0
    beq .no_flip
    move.b #1,d1
.no_flip
    andi.w #tile_index_mask,d0                              ;TODO: USE UPPER BIT TO DENOTE "FLIP" TILE
    asl.l #$06,d0
    lea (a1,d0.l),a1

    bsr ExtractTile

    lea TileColumnsToDecode,a4
    lea TileDecodeDest,a2
    add.l #2,(a2)

    addi.b #1,d4
    cmp.b (a4),d4
    bne .loop_columns

    lea TileDecodeRowDest,a4
    move.l (a4),a1
    lea TileDecodeDest,a2
    lea DestGraphicVTileOffset,a3                           ;One tile height in destination bitmap

    adda.l (a3),a1
    move.l a1,(a2)
    move.l a1,(a4)

    lea TileRowsToDecode,a4
    addi.b #1,d5
    cmp.b (a4),d5
    bne .loop_rows

    rts
;-----------------------------------------------

ClearScreen:                                                ;a1=screen destination address to clear
    WAITBLIT
    clr.w $66(a6)                                           ;destination modulo
    move.l #$01000000,$40(a6)                               ;set operation type in BLTCON0/1
    move.l a1,$54(a6)                                       ;destination address
    move.w #h*bpls*64+bpl/2,$58(a6)                         ;blitter operation size
    rts
;-----------------------------------------------

VBint:                                                      ;Blank template VERTB interrupt
    movem.l d0-a6,-(sp)                                     ;Save used registers
    lea $DFF000,a6
    btst #5,INTREQR(a6)                                     ;INTREQR check if it's our vertb int.
    beq.s .notvb

    moveq #$20,d0                                           ;poll irq bit
    move.w d0,INTREQ(a6)
    move.w d0,INTREQ(a6)

.notvb:
    movem.l (sp)+,d0-a6                                     ;restore
    rte
;-----------------------------------------------

    even

*******************************************************************************
* DATA (FASTMEM)
*******************************************************************************

EncTiles: INCBIN "gfx/gfx2.bin"
    EVEN

ScrollDataLev1: INCBIN "data/lev_1_scroll_data.bin"
    EVEN

TilesToDecode:
    ds.w 21*17*tile_height

TileDecodeDest:
    dc.l 0

TileDecodeRowDest:
    dc.l 0

TileImageStride:
    dc.l 0

TileSource:
    dc.l 0

DestGraphicVTileOffset:
    dc.l 0

DecodedBitplaneBytes:
    dc.b 0,0,0,0,0,0,0,0

TileColumnsToDecode:
    dc.b 0

TileRowsToDecode:
    dc.b 0

    EVEN

*******************************************************************************
* CHIPMEM
*******************************************************************************

    SECTION AllData,DATA_C

Copper:
    dc.w $01fc,0                                            ;slow fetch mode, AGA compatibility
    dc.w $0100,$0200
    dc.b 0,$8e,$2c,$81
    dc.b 0,$90,$2c,$c1
    dc.w $0092,$38
    dc.w $0094,$d0

    dc.w $0108,(320/8)*3
    dc.w $010a,(320/8)*3

    dc.w $0102,0
    dc.w $0104,0

    tile_pal_0f

CopBplP:
    dc.w $00e0,0                                            ;1
    dc.w $00e2,0
    dc.w $00e4,0                                            ;2
    dc.w $00e6,0
    dc.w $00e8,0                                            ;3
    dc.w $00ea,0
    dc.w $00ec,0                                            ;4
    dc.w $00ee,0
;   dc.w $00f0,0                                            ;5
;   dc.w $00f2,0
;   dc.w $00f4,0                                            ;6
;   dc.w $00f6,0

    dc.w $0100,$4200

    ;dc.w $9207,$fffe

    dc.w $ffff,$fffe
CopperE:

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
    ds.b bitpl_bytes_per_raster_line*tile_bitplanes*test_vlines_per_graphic
DecodedGraphicE:


