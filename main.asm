    INCDIR ""
    INCLUDE "photon/PhotonsMiniWrapper1.04!.S"
    INCLUDE "photon/Blitter-Register-List.S"

*******************************************************************************
* DEFINES
*******************************************************************************

    *-----------------*
    * test stuff      *
    *-----------------*

test_tilesrc_bp_offset              = $20000
test_tilesrc_upr_px_b_off           = $20

test_cols_to_decode                 = 128
test_rows_to_decode                 = 16                                    ;TODO: Make it possible to decode more than 16 rows

test_bmp_width_pixels               = 2048
test_bmp_horz_disp_words            = test_bmp_width_pixels/tile_width
test_bmp_bp_bytes_per_raster_line   = test_bmp_horz_disp_words*2
test_bmp_vtile_offset               = test_bmp_bp_bytes_per_raster_line*tile_bitplanes*tile_height
test_bmp_tiles_height               = 16
test_bmp_byte_size                  = test_bmp_vtile_offset*test_bmp_tiles_height

    *-----------------*
    * constants:video *
    *-----------------*

screen_width                        = 320
screen_extrawidth                   = 32
screen_bytes_per_row                = screen_width/8
screen_blit_size                    = h*tile_bitplanes*64+(screen_width*screen_extrawidth)/16
screen_modulo                       =(exp_screen_width/8)*3

exp_screen_width                    = screen_width+screen_extrawidth
exp_screen_bytes_per_row            = exp_screen_width/8

exp_horz_disp_words                 = exp_screen_width/16
exbitpl_bytes_per_raster_line       = exp_horz_disp_words*2

h                                   = 256
bplsize                             = (320+screen_extrawidth)*(h+1)/8

;"tile" here means 16x16 pixels

tile_bitplanes                      = 4
tile_height                         = 16
tile_width                          = 16
tile_plane_lines                    = tile_bitplanes*tile_height
tile_blit_size                      = tile_plane_lines*64+(tile_width/8)

tile_bytes_per_row                  = test_cols_to_decode*2
tiles_per_row                       = test_cols_to_decode

tile_index_mask                     = $07ff

DMA_fetch                           = $28                                   ;$28 for 22 columns;$38 for 20 columns
display_start                       = $91                                   ;$81 for non-scrolling display; $91 otherwise
display_stop                        = $b1                                   ;$c1 for non-scrolling display

horz_disp_words                     = screen_width/16
bitpl_bytes_per_raster_line         = horz_disp_words*2

bpls                                = 3                                     ;handy values:
bpl                                 = screen_width/16*2                     ;byte-width of 1 bitplane line

vlines_per_graphic                  = 48                                    ;32

    *-----------------*
    * palettes        *
    *-----------------*


    *-----------------*
    * registers       *
    *-----------------*


INTREQR         = $1F
COP1LCH         = $80
DIWSTRT         = $8E                                                       ;Start of the screen window
DIWSTOP         = $90                                                       ;End of the screen window
DDFSTRT         = $92                                                       ;Bit=plane DMA Start
DDFSTOP         = $94                                                       ;Bit-Plane DMA Stop
INTENA          = $9A
INTREQ          = $9C

BPLCON0         = $100                                                      ;Bitplane control register 0
BPLCON1         = $102                                                      ;1 (Scroll value)
BPLCON2         = $104                                                      ;2 (Sprite <> Playfield priority)
BPL1MOD         = $108                                                      ;Modulo-Value for odd bit-planes
BPL2MOD         = $10A                                                      ;Modulo-Value for even bit-planes

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
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0000
    dc.w $0190,$0520,$0192,$0730,$0194,$0940,$0196,$0b50
    dc.w $0198,$0d60,$019a,$0f80,$019c,$0000,$019e,$0111
    endm

tile_pal_05:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0455
    dc.w $0190,$0600,$0192,$0900,$0194,$0986,$0196,$0a97
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
    endm

tile_pal_06:macro
    dc.w $0180,$0046,$0182,$089c,$0184,$0789,$0186,$0678
    dc.w $0188,$0567,$018a,$0456,$018c,$0345,$018e,$0540
    dc.w $0190,$0753,$0192,$0864,$0194,$0a75,$0196,$0c86
    dc.w $0198,$0ea7,$019a,$0fc8,$019c,$0ffa,$019e,$0000
    endm

tile_pal_07:macro
    dc.w $0180,$0000,$0182,$0afd,$0184,$07ec,$0186,$00c9
    dc.w $0188,$00a7,$018a,$0086,$018c,$0064,$018e,$0050
    dc.w $0190,$0040,$0192,$0000,$0194,$0000,$0196,$0000
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
    endm

tile_pal_08:macro
    dc.w $0180,$0000,$0182,$0fb9,$0184,$0e98,$0186,$0d86
    dc.w $0188,$0c75,$018a,$0a64,$018c,$0853,$018e,$0640
    dc.w $0190,$0435,$0192,$0857,$0194,$0b75,$0196,$0b5a
    dc.w $0198,$089c,$019a,$0789,$019c,$0046,$019e,$0000
    endm

tile_pal_09:macro
    dc.w $0180,$0000,$0182,$0cb8,$0184,$0ba7,$0186,$0a96
    dc.w $0188,$0985,$018a,$0874,$018c,$0763,$018e,$0650
    dc.w $0190,$0540,$0192,$0430,$0194,$0dd0,$0196,$0d90
    dc.w $0198,$0c70,$019a,$0900,$019c,$0700,$019e,$0000
    endm

tile_pal_0a:macro
    dc.w $0180,$0000,$0182,$0540,$0184,$0750,$0186,$0940
    dc.w $0188,$0e70,$018a,$0340,$018c,$0450,$018e,$0560
    dc.w $0190,$0670,$0192,$0780,$0194,$0990,$0196,$0aa0
    dc.w $0198,$0cc0,$019a,$0de0,$019c,$0ef0,$019e,$0000
    endm

tile_pal_0b:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0455
    dc.w $0190,$0566,$0192,$0776,$0194,$0986,$0196,$0a97
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
    endm

tile_pal_0c:macro
    dc.w $0180,$0000,$0182,$0899,$0184,$0aaa,$0186,$0998
    dc.w $0188,$0887,$018a,$0776,$018c,$0665,$018e,$0554
    dc.w $0190,$0440,$0192,$0000,$0194,$0000,$0196,$0000
    dc.w $0198,$089c,$019a,$0789,$019c,$0046,$019e,$0000
    endm

tile_pal_0d:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0000
    dc.w $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
    endm

tile_pal_0e:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0000
    dc.w $0190,$0035,$0192,$0146,$0194,$0257,$0196,$0368
    dc.w $0198,$0479,$019a,$058a,$019c,$0abc,$019e,$0000
    endm

tile_pal_0f:macro
    dc.w $0180,$0111,$0182,$0FF9,$0184,$0EC7,$0186,$0DA6
    dc.w $0188,$0C85,$018a,$0A74,$018c,$0864,$018e,$0753
    dc.w $0190,$0641,$0192,$0533,$0194,$0431,$0196,$0111
    dc.w $0198,$0111,$019a,$0111,$019c,$0111,$019e,$0110
    endm

obj_pal_00:macro
    dc.w $0180,$0000,$0182,$0343,$0184,$0565,$0186,$0797
    dc.w $0188,$0aca,$018a,$0fff,$018c,$0950,$018e,$0b74
    dc.w $0190,$0da6,$0192,$0fc8,$0194,$0555,$0196,$0777
    dc.w $0198,$0aaa,$019a,$0ccc,$019c,$0000,$019e,$0111
    endm

obj_pal_01:macro
    dc.w $0180,$0000,$0182,$0005,$0184,$0007,$0186,$000a
    dc.w $0188,$006c,$018a,$008d,$018c,$00dd,$018e,$0800
    dc.w $0190,$0555,$0192,$0777,$0194,$0999,$0196,$0bbb
    dc.w $0198,$0600,$019a,$0b00,$019c,$0fff,$019e,$0111
    endm

obj_pal_02:macro
    dc.w $0180,$0000,$0182,$0600,$0184,$0800,$0186,$0a40
    dc.w $0188,$0c60,$018a,$0d90,$018c,$0555,$018e,$0777
    dc.w $0190,$0999,$0192,$0bbb,$0194,$0eee,$0196,$0960
    dc.w $0198,$0b84,$019a,$0da6,$019c,$0fc8,$019e,$0111
    endm

obj_pal_03:macro
    dc.w $0180,$0000,$0182,$0700,$0184,$0a00,$0186,$0c60
    dc.w $0188,$0d90,$018a,$0dd0,$018c,$0666,$018e,$0888
    dc.w $0190,$09aa,$0192,$0bdd,$0194,$0eff,$0196,$0960
    dc.w $0198,$0b84,$019a,$0da6,$019c,$0fc8,$019e,$0111
    endm

obj_pal_04:macro
    dc.w $0180,$0000,$0182,$0700,$0184,$0946,$0186,$0b59
    dc.w $0188,$0e7d,$018a,$0faf,$018c,$0666,$018e,$0888
    dc.w $0190,$09aa,$0192,$0bdd,$0194,$0eff,$0196,$0960
    dc.w $0198,$0b84,$019a,$0da6,$019c,$0fc8,$019e,$0111
    endm

obj_pal_05:macro
    dc.w $0180,$0000,$0182,$0630,$0184,$0950,$0186,$0c85
    dc.w $0188,$0da7,$018a,$0fc9,$018c,$0333,$018e,$0666
    dc.w $0190,$0999,$0192,$0ccc,$0194,$0fff,$0196,$0800
    dc.w $0198,$0b40,$019a,$0e70,$019c,$0fa0,$019e,$0111
    endm

obj_pal_06:macro
    dc.w $0180,$0000,$0182,$0700,$0184,$0a00,$0186,$0c40
    dc.w $0188,$0d60,$018a,$0e90,$018c,$0fe0,$018e,$0555
    dc.w $0190,$0777,$0192,$0999,$0194,$0bbb,$0196,$0060
    dc.w $0198,$0080,$019a,$00b0,$019c,$0eee,$019e,$0111
    endm

obj_pal_07:macro
    dc.w $0180,$0000,$0182,$0610,$0184,$0820,$0186,$0b40
    dc.w $0188,$0e80,$018a,$0fc0,$018c,$0555,$018e,$0777
    dc.w $0190,$0999,$0192,$0bbb,$0194,$0fff,$0196,$0760
    dc.w $0198,$0974,$019a,$0c96,$019c,$0fc8,$019e,$0110
    endm

obj_pal_08:macro
    dc.w $0180,$0000,$0182,$0f00,$0184,$00f0,$0186,$0ff0
    dc.w $0188,$000f,$018a,$0f0f,$018c,$00ff,$018e,$0fff
    dc.w $0190,$0000,$0192,$0f00,$0194,$00f0,$0196,$0ff0
    dc.w $0198,$000f,$019a,$0f0f,$019c,$00ff,$019e,$0fff
    endm

hud_pal_00:macro
    dc.w $0180,$007d,$0182,$0ddd,$0184,$0d00,$0186,$0000
    dc.w $0188,$0000,$018a,$0dd0,$018c,$007f,$018e,$0000
    dc.w $0190,$0111,$0192,$0880,$0194,$0dd0,$0196,$0000
    dc.w $0198,$0111,$019a,$0049,$019c,$009d,$019e,$0000
    endm

hud_pal_01:macro
    dc.w $0180,$0111,$0182,$0a50,$0184,$0e80,$0186,$0000
    dc.w $0188,$0111,$018a,$0880,$018c,$0cc6,$018e,$0000
    dc.w $0190,$0111,$0192,$000c,$0194,$00ce,$0196,$0000
    dc.w $0198,$009e,$019a,$0ddd,$019c,$005d,$019e,$0000
    endm

hud_pal_02:macro
    dc.w $0180,$0ff9,$0182,$0880,$0184,$0cc5,$0186,$0000
    dc.w $0188,$0ddd,$018a,$0d00,$018c,$0009,$018e,$0000
    dc.w $0190,$0ddd,$0192,$0d70,$0194,$004b,$0196,$0000
    dc.w $0198,$0ddd,$019a,$0dd0,$019c,$007f,$019e,$0000
    endm

hud_pal_03:macro
    dc.w $0180,$0111,$0182,$0ddd,$0184,$0d00,$0186,$0d00
    dc.w $0188,$0111,$018a,$0ddd,$018c,$000d,$018e,$000d
    dc.w $0190,$0111,$0192,$0dd6,$0194,$0880,$0196,$0000
    dc.w $0198,$0111,$019a,$0ccc,$019c,$007c,$019e,$0000
    endm

hud_pal_04:macro
    dc.w $0180,$0000,$0182,$0ee0,$0184,$0770,$0186,$0000
    dc.w $0188,$0550,$018a,$0dd0,$018c,$0880,$018e,$0000
    dc.w $0190,$008b,$0192,$00be,$0194,$0008,$0196,$0000
    dc.w $0198,$0111,$019a,$0ddd,$019c,$0000,$019e,$0000
    endm

hud_pal_05:macro
    dc.w $0180,$0555,$0182,$0999,$0184,$0eee,$0186,$0000
    dc.w $0188,$000d,$018a,$0ddd,$018c,$0008,$018e,$0000
    dc.w $0190,$0111,$0192,$0fd4,$0194,$000b,$0196,$0000
    dc.w $0198,$0111,$019a,$0fd4,$019c,$0b00,$019e,$0000
    endm

hud_pal_06:macro
    dc.w $0180,$0fd0,$0182,$0fff,$0184,$0ff8,$0186,$0000
    dc.w $0188,$0111,$018a,$0008,$018c,$000d,$018e,$0000
    dc.w $0190,$0111,$0192,$0808,$0194,$0d0d,$0196,$0000
    dc.w $0198,$0111,$019a,$0884,$019c,$0dd8,$019e,$0000
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

    bsr TESTCode

    bsr DecodeTileGraphicToLongBitmap
    bsr CopyScreenFromDecodedLongBitmap

    lea Screen,a0                                           ;ptr to first bitplane of image
    lea 2(a0),a0                                            ;+2 because we're scrollin' (Skip first column)
    lea CopBplP,a1                                          ;where to poke the bitplane pointer words.
    move #4-1,d0

.bpl7:
    move.l a0,d1
    swap d1
    move.w d1,2(a1)                                         ;hi word
    swap d1
    move.w d1,6(a1)                                         ;lo word

    addq #8,a1                                              ;point to next bpl to poke in copper
    lea exbitpl_bytes_per_raster_line(a0),a0                ;apparently every 40 bytes we'll have new bitplane data
   ;lea bitpl_bytes_per_raster_line(a0),a0                  ;apparently every 40 bytes we'll have new bitplane data
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
* TEST ROUTINES
*******************************************************************************
TESTVBCode:

    ;bsr TESTScroll
    rts

;-----------------------------------------------
TESTCode:
    lea TileColumnsToDecode,a1
    move.b #test_cols_to_decode,(a1)

    lea TileRowsToDecode,a1
    move.b #test_rows_to_decode,(a1)

    lea DestGraphicVTileOffset,a1
    move.l #test_bmp_vtile_offset,(a1)

    lea EncodedTilesSource,a0
    lea TileSource,a1
    move.l a0,(a1)

    bsr TESTLoadLevel1Tiles
    rts

;-----------------------------------------------
TESTLoadLevel1Tiles:
    ;THIS DECODES THE TILES IN MAP LAYOUT (16x16 TILES). 128 tiles horizontally
    ;then wrap to the next row of tiles

    move.l #0,d2
    move.l #0,d3

    lea TilesToDecode,a2
    lea ScrollDataLev1,a1
    move.l a1,a4                                            ;row
    move.l #test_rows_to_decode-1,d0

.outer_loop
    move.l #test_cols_to_decode-1,d1
    move.l #0,d4

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

    ;Here we need a counter, because every 16 tiles, Black Tiger wraps down

    addi #1,d4
    move.l d4,d6
    divu #16,d6                                             ;test_cols_to_decode/16 = 128
    swap d6
    cmp.w #0,d6
    bne .end_inner_loop

    lea $1E0(a1),a1                                         ;down to next 16x16 block of tile indexes

.end_inner_loop
    dbf d1,.inner_loop

    lea $20(a4),a4                                          ;down one row of 16 tile indexes in src
    movea.l a4,a1

    dbf d0,.outer_loop

    rts

;-----------------------------------------------
TESTExtract8BitplaneBytesFromTwoSourceBytes:
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
TESTDecodeRowOf16Pixels:
    ;INPUT: a1 - source bytes ptr
    ;USES:  d2,a4
    ;OUTPUT: a2 - bitplane data

    move.l 0,0(a2)                                          ;leftmost column destinations
    move.l 0,4(a2)                                          ;rightmost column destinations

    move.l a1,d2
    add.l #test_tilesrc_bp_offset,d2                        ;test_tilesrc_bp_offset = offset to bitplanes 0 and 1 in source
    move.l d2,a4

                                                            ;leftmost columns of 8 pixels
    move.l #0,d2
    move.w (a4),d2

                                                            ;d2.w = zeroAndOneByte1 & zeroAndOneByte2
                                                            ;a2 => Bitplane 01 - lower byte; Bitplane 00 - upper byte
    bsr TESTExtract8BitplaneBytesFromTwoSourceBytes         ;returns DecodedBitplaneBytes in a2

    lea 2(a2),a2
    move.l #0,d2
    move.w (a1),d2                                          ;d2.w = twoAndThreeByte1 & twoAndThreeByte2


                                                            ;a2 => Bitplane 03 - lower byte; Bitplane 02 - upper byte
    bsr TESTExtract8BitplaneBytesFromTwoSourceBytes         ;returns DecodedBitplaneBytes in a2


                                                            ;rightmost columns of 8 pixels

    lea 2(a2),a2
    move.l #0,d2
    move.w test_tilesrc_upr_px_b_off(a4),d2                 ;add $20 to get to the src of the rightmost 8 pixel columns


                                                            ;d2.w = zeroAndOneByte1 & zeroAndOneByte2
                                                            ;a2 => Bitplane 01 - lower byte; Bitplane 00 - upper byte

    bsr TESTExtract8BitplaneBytesFromTwoSourceBytes

    lea 2(a2),a2
    move.l #0,d2
    move.w test_tilesrc_upr_px_b_off(a1),d2                 ;d2.w = twoAndThreeByte1 & twoAndThreeByte2


                                                            ;a2 => Bitplane 03 - lower byte; Bitplane 02 - upper byte

    bsr TESTExtract8BitplaneBytesFromTwoSourceBytes
    lea -6(a2),a2
    rts

;-----------------------------------------------
TESTGetXYPositionsForScrollRight:
    ;returns mapx/y in d3
    ;returns x/y in d4

;BLOCKSWIDTH        = bitmapwidth
;BLOCKSBYTESPERROW  = tile_bytes_per_row
;BLOCKSPERROW       = tiles_per_row

    lea MapXYPosition,a3

    move.w 2(a3),d3                                          ;save for mapy
    swap d3
    move.w 2(a3),d3                                          ;mapposx
    asr.w #4,d3                                              ;mapposx / BLOCKWIDTH

    move.w #tile_bytes_per_row,d4

    add.w d4,d3                                             ;mapx = mapposx / BLOCKWIDTH + BITMAPBLOCKSPERROW;
    swap d3
    and.w #15,d3                                            ;mapy = mapposx & (NUMSTEPS - 1);

    clr.l d4

    lea VideoXYPosition,a4

    ;TODO: I think we may want screen_width+screen_extrawidth HERE
    move.w #screen_width+screen_extrawidth,d4

    move.w 2(a4),d5
    and.w #$FFF0,d5

    add.w d5,d4                                             ;x = BITMAPWIDTH + ROUND2BLOCKWIDTH(videoposx);

    swap d4
    move.w d3,d4
    asl.w #6,d4                                             ;y = mapy * tile_height * tile_bitplanes;
    swap d3                                                 ;mapx
    swap d4                                                 ;x

    rts

;-----------------------------------------------
TESTGetXYPositionsForScrollLeft:
    ;returns mapx/y in d3
    ;returns x/y in d4

;BLOCKSWIDTH        = bitmapwidth
;BLOCKSBYTESPERROW  = tile_bytes_per_row
;BLOCKSPERROW       = tiles_per_row

   lea MapXYPosition,a3
   lea VideoXYPosition,a4

   subi.l #1,(a3)                                           ;mapposx--;

.update
   move.l (a3),(a4)                                         ;videoposx = mapposx;


   move.w 2(a3),d3                                         ;mapposx (pixels)
   swap d3
   move.w 2(a3),d3                                         ;mapposx
   asr.w #4,d3                                             ;mapx = mapposx / BLOCKWIDTH;
   swap d3
   and.w #15,d3                                            ;mapy = mapposx & (NUMSTEPS - 1);

   clr.l d4

   move.w 2(a4),d4
   and.w #$FFF0,d4                                         ;x = ROUND2BLOCKWIDTH(videoposx);
   swap d4
   move.w d3,d4
   asl.w #6,d4                                             ;y = mapy * tile_height * tile_bitplanes;
   swap d3                                                 ;mapx
   swap d4                                                 ;x

   rts

;-----------------------------------------------
TESTScroll:

   lea TestScrollCommand,a0                                 ;0=user move right;1=user move left
   lea CopHorzScrollPos,a1                                  ;Copper Horizontal Scroll pos (ptr + 2)
   lea TileXYPosition,a2                                    ;TileXYPosition (upper left of screen)

   move.w 2(a1),d0                                          ;d0=HORIZONTAL SCROLL POS
   and.w #$00ff,d0

;   sub.w #$0011,d0                                          ;subtracting from BPLCON1 makes more data appear
;   and.w #$00ff,d0                                          ;at the right edge
;
;   cmp.w #$ef,d0
;   bne .end
;
;   move.w #$ff,d0                                           ;move bitplane pointers+2
;
;   bra .end

   clr.l d1
   lea MapXYPosition,a3
   move.w 2(a3),d1

   cmp.b #1,(a0)                                            ;user move left... do left
   beq .left

.right
    ;if (mapposx >= (mapwidth * BLOCKWIDTH - SCREENWIDTH - BLOCKWIDTH)) return;
    cmp.w #(test_cols_to_decode*tile_width-screen_width-tile_width),d1              ;1728 - tile_width
    blo .right_not_done

    rts

.right_not_done

   bsr TESTGetXYPositionsForScrollRight

   lea PtrSaveWord,a3
   lea SaveWord,a4
   lea PreviousScrollDir,a5
   cmp.b #1,(a5)                                            ;if (previous_direction == DIRECTION_LEFT)
   bne .rupdate_saveword

   WAITBLIT                                                 ;HardWaitBlit();
   move.w (a4),(a3)                                         ;*savewordpointer = saveword;

.rupdate_saveword
   clr.l d1
   clr.l d2

   lea Screen,a5                                            ;frontbuffer
   move.l a5,d2
   move.w d4,d1                                             ;x
   asr.w #3,d1                                              ;(x / 8)
   add.l d1,d2                                              ;frontbuffer + (x / 8)

   clr.l d1

   swap d4                                                  ;y
   move.w d4,d1
   add.w #tile_plane_lines-1,d1                             ;(y + tile_plane_lines - 1)
   mulu #(screen_width+screen_extrawidth)/2,d1              ;* bitmap_bytes_per_row
   add.l d1,d2
   move.l d2,a3                                             ;savewordpointer = (WORD *)(frontbuffer + (y + tile_plane_lines - 1) * bitmap_bytes_per_row + (x / 8));
   move.w (a3),(a4)                                         ;saveword = *savewordpointer;

   swap d4                                                  ;x

   bsr DrawTile                                             ;DrawBlock(x,y,mapx,mapy);
   move.w d6,d5


   lea MapXYPosition,a3
   lea VideoXYPosition,a4

   cmp.l #(test_cols_to_decode*tile_width),(a3)
   bne .add

   move.l #0,(a3)
   bra .update

.add
   addi.l #1,(a3)                                           ;mapposx++;

.update
   move.l (a3),(a4)                                         ;videoposx = mapposx;
   lea PreviousScrollDir,a3
   move.b #0,(a3)                                           ;previous_direction = DIRECTION_RIGHT;



;   sub.w #$0011,d0                                          ;subtracting from BPLCON1 makes more data appear
;   and.w #$00ff,d0                                          ;at the right edge
;
;   cmp.w #$ef,d0
;   bne .end
;
;   move.w #$ff,d0                                           ;move bitplane pointers+2
;
;   bra .end



;now do the scroll

   sub.w #$0011,d0                                          ;subtracting from BPLCON1 makes more data appear
   and.w #$00ff,d0                                          ;at the right edge

   move.b #0,d2
   cmp.w #$ef,d0
   bne .end

   move.w #$ff,d0                                           ;move bitplane pointers+2

   addi.w #1,(a2)
   cmp.w #128,(a2)

   bne .rupdate_copper

   move.w #0,d0
   bra .switch_direction

.rupdate_copper
   lea CopBplP,a2                                           ;where to poke the bitplane pointer words.
   move #4-1,d1

   clr.l d6
   move.w d5,d6                                             ;mapx
   divu #exp_screen_bytes_per_row,d6
   swap d6
   cmp.w #0,d6                                              ;remainder of 0 ($0)
   bne .right_update_loop

   lea Screen,a3                                            ;point to column 0 again

.rreset:
   move.l a3,d4
   swap d4
   move.w d4,2(a2)                                         ;hi word
   swap d4
   move.w d4,6(a2)                                         ;lo word

   addq #8,a2                                              ;point to next bpl to poke in copper
   lea exbitpl_bytes_per_raster_line(a3),a3                ;apparently every 44 bytes we'll have new bitplane data
   dbf d1,.rreset

   bra .end

.right_update_loop
   clr.l d3

   move.w 2(a2),d3                                          ;hi word
   swap d3
   move.w 6(a2),d3                                          ;lo word

   add.l #2,d3

   move.w d3,6(a2)                                          ;lo word
   swap d3
   move.w d3,2(a2)                                          ;hi word

   addq #8,a2                                               ;point to next bpl to poke in copper
   dbf d1,.right_update_loop

   bra .end

.left
    cmp.w #-1,d1
    bgt .left_not_done
    rts                                                     ;if (mapposx < 1) return;

.left_not_done

   bsr TESTGetXYPositionsForScrollLeft

   lea PtrSaveWord,a3
   lea SaveWord,a4
   lea PreviousScrollDir,a5
   cmp.b #0,(a5)                                            ;if (previous_direction == DIRECTION_RIGHT)
   bne .lupdate_saveword

   WAITBLIT                                                 ;HardWaitBlit();
   move.w (a4),(a3)                                         ;*savewordpointer = saveword;

.lupdate_saveword
   clr.l d1
   clr.l d2
   lea Screen,a5
   move.l a5,d2                                             ;frontbuffer
   move.w d4,d1                                             ;x
   asr.w #3,d1                                              ;(x / 8)
   add.l d1,d2                                              ;frontbuffer + (x / 8)

;TODO: CONVERT y (in d4) TO SCREEN BUFFER COORDS; RIGHT NOW IT IS IN BIG BITMAP COORDS
   clr.l d1
   swap d4                                                  ;y
   move.w d1,d4
   mulu #(screen_width+screen_extrawidth)/2,d1              ;* bitmap_bytes_per_row
   add.l d1,d2
   move.l d2,a3                                             ;savewordpointer = (WORD *)(frontbuffer + y * bitmap_bytes_per_row + (x / 8));
   move.w (a3),(a4)                                         ;saveword = *savewordpointer;
   swap d4                                                  ;x

   bsr DrawTile                                             ;DrawBlock(x,y,mapx,mapy);
   move.w d6,d5

   lea PreviousScrollDir,a3
   move.b #1,(a3)                                           ;previous_direction = DIRECTION_LEFT;

;now do the scroll

   add.w #$0011,d0                                          ;adding to BPLCON1 makes more data appear
   and.w #$00ff,d0                                          ;at the left edge

   move.b #1,d2
   cmp.w #$0010,d0
   bne .end

   move.w #0,d0                                             ;move bitplane pointers-2

   addi.w #1,(a2)
   cmp.w #128,(a2)
   bne .lupdate_copper

   move.w #$ff,d0
   bra .switch_direction

.lupdate_copper
   lea CopBplP,a2                                           ;where to poke the bitplane pointer words.
   move #4-1,d1

   clr.l d6
   move.w d5,d6                                             ;mapx
   divu #exp_screen_bytes_per_row,d6
   swap d6
   cmp.w #(exp_screen_bytes_per_row-1),d6                    ;remainder of 19 ($13) (21 $15)
   bne .left_update_loop

   move.l #2,(a0)
   rts

   lea Screen,a3
   move.l a3,d4
   add.l #(exp_screen_bytes_per_row-1),d4                    ;point to column 19 again
   move.l d4,a3

.lreset:
   move.l d4,a3
   swap d4
   move.w d4,2(a2)                                         ;hi word
   swap d4
   move.w d4,6(a2)                                         ;lo word

   addq #8,a2                                              ;point to next bpl to poke in copper
   lea exbitpl_bytes_per_raster_line(a3),a3                ;apparently every 44 bytes we'll have new bitplane data
   dbf d1,.lreset

   bra .end

.left_update_loop:
   clr.l d3

   move.w 2(a2),d3                                          ;hi word
   swap d3
   move.w 6(a2),d3                                          ;lo word

   sub.l #2,d3

   move.w d3,6(a2)                                          ;lo word
   swap d3
   move.w d3,2(a2)                                          ;hi word

   addq #8,a2                                               ;point to next bpl to poke in copper
   dbf d1,.left_update_loop

   bra .end

.switch_direction

   move.b #0,(a2)
   move.b d2,(a0)

.end
   lea CopHorzScrollPos,a1                                  ;Copper Horizontal Scroll pos (ptr + 2)
   move.w d0,2(a1)                                          ;update copper
   rts

*******************************************************************************
* ROUTINES
*******************************************************************************
VBCode:

    rts

;-----------------------------------------------
CopyScreenFromDecodedLongBitmap:
;Does a rectangular blit to the the visible screen, then a strip blit from the edge
    lea DecodedGraphic,a3
    lea Screen,a4
    lea 2(a4),a4                                            ;initially skip first column of pixels
    move.l a3,d3
    move.l a4,d4

    move.w #$09F0,BLTCON0(a6)                               ;use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)
    move.w #$FFFF,BLTAFWM(a6)
    move.w #$FFFF,BLTALWM(a6)
    move.w #214,BLTAMOD(a6)                                 ;skip 107 columns (copy 21)
    move.w #2,BLTDMOD(a6)                                   ;skip 1 column (copy 21)
    move.l d3,BLTAPTH(a6)
    move.l d4,BLTDPTH(a6)

    move.w #(exp_screen_width-tile_width)/16,BLTSIZE(a6)    ;no "h" term needed since it's 1024. Thanks ross @eab!

    WAITBLIT

    add.l #127*2,d3                                         ;move source to last column
    sub.l #2,d4

    move.w #254,BLTAMOD(a6)                                 ;skip 127 columns (copy 1)
    move.w #42,BLTDMOD(a6)                                  ;skip 21 columns (copy 1)
    move.l d3,BLTAPTH(a6)
    move.l d4,BLTDPTH(a6)

    move.w #1,BLTSIZE(a6)                                   ;one column
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
    bsr TESTDecodeRowOf16Pixels                             ;This can be changed for a version where we decode differently

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

    ;this interleaves the bytes. Maybe we can do something that doesn't
    ;interleave the bytes

    move.b, 3(a2),test_bmp_bp_bytes_per_raster_line*0(a3)         ;bitplane 0
    move.b, 2(a2),test_bmp_bp_bytes_per_raster_line*1(a3)         ;bitplane 1
    move.b, 1(a2),test_bmp_bp_bytes_per_raster_line*2(a3)         ;bitplane 2
    move.b, (a2),test_bmp_bp_bytes_per_raster_line*3(a3)          ;bitplane 3
    move.b, 7(a2),test_bmp_bp_bytes_per_raster_line*0+1(a3)       ;bitplane 0
    move.b, 6(a2),test_bmp_bp_bytes_per_raster_line*1+1(a3)       ;bitplane 1
    move.b, 5(a2),test_bmp_bp_bytes_per_raster_line*2+1(a3)       ;bitplane 2
    move.b, 4(a2),test_bmp_bp_bytes_per_raster_line*3+1(a3)       ;bitplane 3

    lea test_bmp_bp_bytes_per_raster_line*tile_bitplanes(a3),a3   ;move down one rasterline ($400 = $100 * 4 bitplanes; $100 = bytes in one rasterline for one bitplane)
    lea $02(a1),a1                                          ;source bytes per rasterline are 2 bytes apart

    dbf d0,.extract_tile
    rts

;-----------------------------------------------
DecodeTileGraphicToLongBitmap:
    ;SCREEN LO-RES
    ;W: 2048; H=256
    ;8 pixels/byte
    ;32 bytes per line/16 words per line
    ;done to show how to extract Black Tiger-encoded image data
    ;32*32*4

    lea DecodedGraphic,a0
    lea TileDecodeRowDest,a1

    move.l a0,(a1)
    move.l (DecodedGraphicE-DecodedGraphic)/4,d0

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
    andi.w #tile_index_mask,d0
    asl.l #$06,d0
    lea (a1,d0.l),a1

    bsr ExtractTile

    lea TileColumnsToDecode,a4
    lea TileDecodeDest,a2
    add.l #2,(a2)
    addi.b #1,d4

.check_loop
    cmp.b (a4),d4
    bne .loop_columns

    lea TileDecodeRowDest,a4
    move.l (a4),a1
    lea TileDecodeDest,a2
    lea DestGraphicVTileOffset,a3                          ;One tile height in destination bitmap

    adda.l (a3),a1
    move.l a1,(a2)
    move.l a1,(a4)

    lea TileRowsToDecode,a4
    addi.b #1,d5
    cmp.b (a4),d5
    bne .loop_rows

    rts

;-----------------------------------------------
DrawTile:
;INPUT: mapx/y in d3
;       x/y in d4
;       x = in pixels
;       y = in "planelines" (1 realline = BLOCKSDEPTH planelines)
;OUTPUT: mapx in d6

    move.w d4,d5
    asr.w #3,d5                                             ;(x / 8)
    and.w #$fffe,d5                                         ;x = (x / 8) & 0xFFFE;

    swap d5
    swap d4

    move.w d4,d5                                            ;y
    ;mulu #40,d5                                             ;y = y * BITMAPBYTESPERROW;
    ;mulu #tile_bytes_per_row,d5                             ;y = y * BITMAPBYTESPERROW;
    mulu #(screen_width+screen_extrawidth)/2,d1              ;* bitmap_bytes_per_row

    move.l d5,d4

    ;TODO: ORDINARILY, WE'D HAVE THE BLOCK DATA AS A TILE SHEET IN MEMORY
    ;      WE MAY NOT WANT TO DO THAT. RIGHT NOW, I JUST HAVE A SINGLE COPY OF THE
    ;      BITMAP ALREADY LOADED INTO A LONG SCREEN BUFFER
    ;      THE TILE BITMAP IS HERE: EncodedTilesSource

    clr.l d5

    swap d3
    move.w d3,d5                                            ;mapy
    asl.w #7,d5                                             ;mapy * mapwidth
    swap d3
    add.w d3,d5                                             ;block = mapdata[mapy * mapwidth + mapx]
    divu #tiles_per_row,d5                                  ;block / tiles_per_row
    swap d5                                                 ;block % tiles_per_row
    move.w d5,d3
    asl.w #1,d3                                             ;mapx = (block % BLOCKSPERROW) * (BLOCKWIDTH / 8);

    swap d3                                                 ;mapy
    swap d5

    move.w d5,d3                                            ;(block / tiles_per_row)

    clr.l d5
    move.w d3,d5
    asl.w #6,d5                                             ;(block / tiles_per_row) * (BLOCKPLANELINES)
    mulu #(tile_bytes_per_row*4),d5                         ;mapy = (block / BLOCKSPERROW) * (BLOCKPLANELINES * BLOCKSBYTESPERROW)
    move.w d5,d3                                            ;mapy

    WAITBLIT                                                ;HardWaitBlit();

    lea DecodedGraphic,a3
    lea Screen,a4                                           ;TODO: CHECK TO SEE IF THIS NEEDS AN OFFSET

    move.l a3,d5                                            ;A source (blocksbuffer)
    move.l a4,d1                                            ;D dest (frontbuffer)

    clr.l d6
    clr.l d2
    add.w d3,d2
    add.l d2,d5
    clr.w d3
    swap d3
    move.w d3,d6                                            ;preserve mapx
    add.l d3,d5                                             ;blocksbuffer + mapy + mapx

    clr.l d2
    add.w d4,d2
    add.l d2,d1
    clr.w d4
    swap d4
    add.l d4,d1                                             ;frontbuffer + y + x

    move.w #$09F0,BLTCON0(a6)                               ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)                               ;custom->bltcon1 = 0;
    move.w #$FFFF,BLTAFWM(a6)                               ;custom->bltafwm = 0xFFFF;
    move.w #$FFFF,BLTALWM(a6)                               ;custom->bltalwm = 0xFFFF;
    move.w #tile_bytes_per_row-2,BLTAMOD(a6)                ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    ;move.w #screen_bytes_per_row-2,BLTDMOD(a6)              ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.w #exp_screen_bytes_per_row-2,BLTDMOD(a6)          ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l d5,BLTAPTH(a6)                                   ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l d1,BLTDPTH(a6)                                   ;custom->bltdpt  = frontbuffer + y + x;

    move.w #(tile_plane_lines*64+1),BLTSIZE(a6)             ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);
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

    bsr TESTVBCode
    bsr VBCode

.notvb:
    movem.l (sp)+,d0-a6                                     ;restore
    rte
;-----------------------------------------------

    even

*******************************************************************************
* DATA (FASTMEM)
*******************************************************************************

EncodedTilesSource: INCBIN "gfx/gfx2.bin"
    EVEN

ScrollDataLev1: INCBIN "data/lev_1_scroll_data.bin"
    EVEN

TilesToDecode:
    ds.w (test_cols_to_decode+1)*(test_rows_to_decode+1)*tile_height

TileXYPosition:
    dc.l 0

TileDecodeDest:
    dc.l 0

TileDecodeRowDest:
    dc.l 0

TileImageStride:
    dc.l 0

TileSource:
    dc.l 0

MapXYPosition:
    dc.l 0

VideoXYPosition:
    dc.l 0

PtrSaveWord
    dc.l 0

DestGraphicVTileOffset:
    dc.l 0

SaveWord:
    dc.w 0

DecodedBitplaneBytes:
    dc.b 0,0,0,0,0,0,0,0

TileColumnsToDecode:
    dc.b 0

TileRowsToDecode:
    dc.b 0

TestScrollCommand:
    dc.b 0

PreviousScrollDir:
    dc.b 0

    EVEN

*******************************************************************************
* CHIPMEM
*******************************************************************************

    SECTION AllData,DATA_C

Copper:
    dc.w $01fc,0                                            ;slow fetch mode, AGA compatibility
    dc.w BPLCON0,$0200
    dc.b 0,DIWSTRT,$2c,display_start
    dc.b 0,DIWSTOP,$2c,display_stop
    dc.w DDFSTRT,DMA_fetch                                  ;$38 for no scrolling
    dc.w DDFSTOP,$d0

    dc.w BPL1MOD,screen_modulo                              ;(320/8)*3 for no scrolling
    dc.w BPL2MOD,screen_modulo                              ;(320/8)*3 for no scrolling

CopHorzScrollPos:
    dc.w BPLCON1,$00
    dc.w BPLCON2,0

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

    dc.w BPLCON0,$4200

    ;dc.w $9207,$fffe

    dc.w $ffff,$fffe
CopperE:

    EVEN

*******************************************************************************
* BUFFERS
*******************************************************************************

    SECTION AllBuffers,BSS_C

Screen:
    ds.b screen_blit_size                                   ;bplsize*4
ScreenE:

    EVEN

Screen2:
    ds.b screen_blit_size                                   ;bplsize*4
Screen2E:

    EVEN

Screen3:
    ds.b screen_blit_size                                   ;bplsize*4
Screen3E:

    EVEN

DecodedGraphic:
    ds.b $40000                                             ;bitmapwidth/16*tile_bitplanes*vlines_per_graphic
DecodedGraphicE:


