*******************************************************************************
* MACROS
*******************************************************************************
;INPUT: source ptr in d5; dest ptr in d1; + (rows,cols)
;EX: BLIT_ROWS_AND_COLS 1,1
BLIT_ROWS_AND_COLS:macro
    move.w #$09F0,BLTCON0(a6)                               ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)                               ;custom->bltcon1 = 0;
    move.w #$FFFF,BLTAFWM(a6)                               ;custom->bltafwm = 0xFFFF;
    move.w #$FFFF,BLTALWM(a6)                               ;custom->bltalwm = 0xFFFF;
    move.w #tile_bytes_per_row-2*(\2),BLTAMOD(a6)           ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    move.w #screen_bpl_bytes_per_row-2*(\2),BLTDMOD(a6)     ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l d5,BLTAPTH(a6)                                   ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l d1,BLTDPTH(a6)                                   ;custom->bltdpt  = frontbuffer + y + x;
    move.w #(tile_plane_lines*(\1)*64+1*(\2)),BLTSIZE(a6)   ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);

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
    dc.l TileDrawThirteenVertical
    dc.l TileDrawFourteenVertical
    dc.l TileDrawFifteenVertical

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
TileDrawThirteenVertical:
    BLIT_ROWS_AND_COLS 13,1
;-----------------------------------------------
TileDrawFourteenVertical:
    BLIT_ROWS_AND_COLS 14,1
;-----------------------------------------------
TileDrawFifteenVertical:
    BLIT_ROWS_AND_COLS 15,1
;-----------------------------------------------
