*******************************************************************************
* MACROS
*******************************************************************************
;INPUT: source ptr in d5; dest ptr in d1; + (rows,cols)
;EX: BLIT_ROWS_AND_COLS 1,1
BLIT_ROWS_AND_COLS:macro
    move.w  v_map_source_bytes_per_row(a0),d6
    sub.w   #2*(\2),d6
    move.w  #$09F0,BLTCON0(a6)                              ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w  #$0000,BLTCON1(a6)                              ;custom->bltcon1 = 0;
    move.w  #$FFFF,BLTAFWM(a6)                              ;custom->bltafwm = 0xFFFF;
    move.w  #$FFFF,BLTALWM(a6)                              ;custom->bltalwm = 0xFFFF;
    move.w  d6,BLTAMOD(a6)                                  ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    move.w  #screen_bpl_bytes_per_row-2*(\2),BLTDMOD(a6)    ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l  d5,BLTAPTH(a6)                                  ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l  d1,BLTDPTH(a6)                                  ;custom->bltdpt  = frontbuffer + y + x;
    move.w  #(tile_plane_lines*(\1)*64+1*(\2)),BLTSIZE(a6)  ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);

;    cmp.l #$3C056,d3                                        ;can stop after a particular tile is blitted by
;    bne .end                                                ;doing something like this
;
;    lea TestScrollCommand,a0                                ;0=r;1=l;d=2;u=3;rd=4;ru=5;ld=6;lu=7
;    move.b #16,(a0)

;.end

.end
    rts
    endm

*******************************************************************************
* ROUTINES
*******************************************************************************

TileDrawHorizontalJumpTable:
    dc.l TileNoDraw
    dc.l TileDraw
    dc.l TileDrawTwoHorizontal
    dc.l TileDrawThreeHorizontal
    dc.l TileDrawFourHorizontal
    dc.l TileDrawFiveHorizontal
    dc.l TileDrawSixHorizontal
    dc.l TileDrawSevenHorizontal
    dc.l TileDrawEightHorizontal
    dc.l TileDrawNineHorizontal
    dc.l TileDrawTenHorizontal
    dc.l TileDrawElevenHorizontal
    dc.l TileDrawTwelveHorizontal
    dc.l TileDrawThirteenHorizontal
    dc.l TileDrawFourteenHorizontal
    dc.l TileDrawFifteenHorizontal

TileDrawVerticalJumpTable:
    dc.l TileNoDraw
    dc.l TileDraw
    dc.l TileDrawTwoVertical
    dc.l TileDrawThreeVertical
    dc.l TileDrawFourVertical
    dc.l TileDrawFiveVertical
    dc.l TileDrawSixVertical
    dc.l TileDrawSevenVertical
    dc.l TileDrawEightVertical
    dc.l TileDrawNineVertical
    dc.l TileDrawTenVertical
    dc.l TileDrawElevenVertical
    dc.l TileDrawTwelveVertical
    dc.l TileNoDraw
    dc.l TileNoDraw
    dc.l TileNoDraw
    ;dc.l TileDrawThirteenVertical
    ;dc.l TileDrawFourteenVertical
    ;dc.l TileDrawFifteenVertical

;-----------------------------------------------
TileNoDraw:
    rts
;-----------------------------------------------
TileDraw:
    BLIT_ROWS_AND_COLS 1,1
;-----------------------------------------------
TileDrawTwoHorizontal:
    BLIT_ROWS_AND_COLS 1,2
;-----------------------------------------------
TileDrawThreeHorizontal:
    BLIT_ROWS_AND_COLS 1,3
;-----------------------------------------------
TileDrawFourHorizontal:
    BLIT_ROWS_AND_COLS 1,4
;-----------------------------------------------
TileDrawFiveHorizontal:
    BLIT_ROWS_AND_COLS 1,5
;-----------------------------------------------
TileDrawSixHorizontal:
    BLIT_ROWS_AND_COLS 1,6
;-----------------------------------------------
TileDrawSevenHorizontal:
    BLIT_ROWS_AND_COLS 1,7
;-----------------------------------------------
TileDrawEightHorizontal:
    BLIT_ROWS_AND_COLS 1,8
;-----------------------------------------------
TileDrawNineHorizontal:
    BLIT_ROWS_AND_COLS 1,9
;-----------------------------------------------
TileDrawTenHorizontal:
    BLIT_ROWS_AND_COLS 1,10
;-----------------------------------------------
TileDrawElevenHorizontal:
    BLIT_ROWS_AND_COLS 1,11
;-----------------------------------------------
TileDrawTwelveHorizontal:
    BLIT_ROWS_AND_COLS 1,12
;-----------------------------------------------
TileDrawThirteenHorizontal:
    BLIT_ROWS_AND_COLS 1,13
;-----------------------------------------------
TileDrawFourteenHorizontal:
    BLIT_ROWS_AND_COLS 1,14
;-----------------------------------------------
TileDrawFifteenHorizontal:
    BLIT_ROWS_AND_COLS 1,15
;-----------------------------------------------
TileDrawTwoVertical:
    BLIT_ROWS_AND_COLS 2,1
;-----------------------------------------------
TileDrawThreeVertical:
    BLIT_ROWS_AND_COLS 3,1
;-----------------------------------------------
TileDrawFourVertical:
    BLIT_ROWS_AND_COLS 4,1
;-----------------------------------------------
TileDrawFiveVertical:
    BLIT_ROWS_AND_COLS 5,1
;-----------------------------------------------
TileDrawSixVertical:
    BLIT_ROWS_AND_COLS 6,1
;-----------------------------------------------
TileDrawSevenVertical:
    BLIT_ROWS_AND_COLS 7,1
;-----------------------------------------------
TileDrawEightVertical:
    BLIT_ROWS_AND_COLS 8,1
;-----------------------------------------------
TileDrawNineVertical:
    BLIT_ROWS_AND_COLS 9,1
;-----------------------------------------------
TileDrawTenVertical:
    BLIT_ROWS_AND_COLS 10,1
;-----------------------------------------------
TileDrawElevenVertical:
    BLIT_ROWS_AND_COLS 11,1
;-----------------------------------------------
TileDrawTwelveVertical:
    BLIT_ROWS_AND_COLS 12,1
;-----------------------------------------------
;TileDrawThirteenVertical:
;    BLIT_ROWS_AND_COLS 13,1
;;-----------------------------------------------
;TileDrawFourteenVertical:
;    BLIT_ROWS_AND_COLS 14,1
;;-----------------------------------------------
;TileDrawFifteenVertical:
;    BLIT_ROWS_AND_COLS 15,1
;-----------------------------------------------
TileExtractFromSourceIntoMapBitmap:
;INPUT:  a1 - source bytes ptr
;        a3 - destination ptr
;        a5 - DecodedBitplaneBytes
;        d1 - flipped = 1.b, not flipped = 0.b
;OUTPUT: a3 - destination

    move    #$0F,d0                                         ;16 rasterlines at a time; rightmost byte done too

.extract_tile:

    move.w  (a1),(a5)
    move.w  map_source_tile_bpl_bytes_per_row*1(a1),2(a5)
    move.w  map_source_tile_bpl_bytes_per_row*2(a1),4(a5)
    move.w  map_source_tile_bpl_bytes_per_row*3(a1),6(a5)
    move.w  map_source_tile_bpl_bytes_per_row*4(a1),8(a5)

    ;To flip the tile horizontally, we need to reverse the 4 bit color indexes,
    ;which means reversing the NYBBLES--not the bits. Reversing the bits gives
    ;us swapping of the odd bitplanes, which messes up the colors

    cmp.b   #1,d1
    bne     .no_flip

    swap    d1

    move.b  (a5),d6
    move.b  1(a5),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,(a5)
    move.b  d1,1(a5)

    move.b  2(a5),d6
    move.b  3(a5),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,2(a5)
    move.b  d1,3(a5)

    move.b  4(a5),d6
    move.b  5(a5),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,4(a5)
    move.b  d1,5(a5)

    move.b  6(a5),d6
    move.b  7(a5),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,6(a5)
    move.b  d1,7(a5)

    move.b  8(a5),d6
    move.b  9(a5),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,8(a5)
    move.b  d1,9(a5)

    swap    d1

.no_flip

    move.w  (a5),(a3)
    move.w  2(a5),eight_by_four_map_bpl_bytes_per_row*1(a3)
    move.w  4(a5),eight_by_four_map_bpl_bytes_per_row*2(a3)
    move.w  6(a5),eight_by_four_map_bpl_bytes_per_row*3(a3)
    move.w  8(a5),eight_by_four_map_bpl_bytes_per_row*4(a3)

    lea     eight_by_four_map_bytes_per_row(a3),a3                  ;move down one rasterline in dest   ($500 = $100 * 5 bitplanes; $100 = bytes in one rasterline for one bitplane)
    lea     map_source_tile_bytes_per_row(a1),a1                    ;move down one rasterline in source ($A0  = $20  * 5 bpl)

    dbf     d0,.extract_tile

    rts
;-----------------------------------------------
