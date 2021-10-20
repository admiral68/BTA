TileDrawHorizontalJumpTable:
    dc.l TileNoDraw
    dc.l TileDraw
    dc.l TileDrawTwoHorizontal

TileDrawVerticalJumpTable:
    dc.l TileNoDraw
    dc.l TileDraw
    dc.l TileDrawTwoVertical

;-----------------------------------------------
TileNoDraw:
    rts

;-----------------------------------------------
TileDraw:
;INPUT: mapx/y in d3
;       x/y in d4
;OUTPUT: source ptr in d5; dest ptr in d1

;    cmp.w #$28,d2
;    bne .skip_breakpoint
;    mcgeezer_special2
;.skip_breakpoint
    move.w #$09F0,BLTCON0(a6)                               ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)                               ;custom->bltcon1 = 0;
    move.w #$FFFF,BLTAFWM(a6)                               ;custom->bltafwm = 0xFFFF;
    move.w #$FFFF,BLTALWM(a6)                               ;custom->bltalwm = 0xFFFF;
    move.w #tile_bytes_per_row-2,BLTAMOD(a6)                ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    move.w #screen_bpl_bytes_per_row-2,BLTDMOD(a6)          ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l d5,BLTAPTH(a6)                                   ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l d1,BLTDPTH(a6)                                   ;custom->bltdpt  = frontbuffer + y + x;

    move.w #(tile_plane_lines*64+1),BLTSIZE(a6)             ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);

;    cmp.l #$3C056,d3                                        ;can stop after a particular tile is blitted by
;    bne .end                                                 ;doing something like this
;
;    lea TestScrollCommand,a0                                ;0=r;1=l;d=2;u=3;rd=4;ru=5;ld=6;lu=7
;    move.b #16,(a0)

.end
    rts

;-----------------------------------------------
TileDrawTwoHorizontal:
;INPUT: mapx/y in d3
;       x/y in d4
;OUTPUT: source ptr in d5; dest ptr in d1
;    mcgeezer_special2
    move.w #$09F0,BLTCON0(a6)                               ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)                               ;custom->bltcon1 = 0;
    move.w #$FFFF,BLTAFWM(a6)                               ;custom->bltafwm = 0xFFFF;
    move.w #$FFFF,BLTALWM(a6)                               ;custom->bltalwm = 0xFFFF;
    move.w #tile_bytes_per_row-4,BLTAMOD(a6)                ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    move.w #screen_bpl_bytes_per_row-4,BLTDMOD(a6)          ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l d5,BLTAPTH(a6)                                   ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l d1,BLTDPTH(a6)                                   ;custom->bltdpt  = frontbuffer + y + x;

    move.w #(tile_plane_lines*64+2),BLTSIZE(a6)             ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);
.end
    rts

;-----------------------------------------------
TileDrawThreeHorizontal:
;INPUT: mapx/y in d3
;       x/y in d4
;OUTPUT: source ptr in d5; dest ptr in d1

    move.w #$09F0,BLTCON0(a6)                               ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)                               ;custom->bltcon1 = 0;
    move.w #$FFFF,BLTAFWM(a6)                               ;custom->bltafwm = 0xFFFF;
    move.w #$FFFF,BLTALWM(a6)                               ;custom->bltalwm = 0xFFFF;
    move.w #tile_bytes_per_row-6,BLTAMOD(a6)                ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    move.w #screen_bpl_bytes_per_row-6,BLTDMOD(a6)          ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l d5,BLTAPTH(a6)                                   ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l d1,BLTDPTH(a6)                                   ;custom->bltdpt  = frontbuffer + y + x;

    move.w #(tile_plane_lines*64+3),BLTSIZE(a6)             ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);
.end
    rts

;-----------------------------------------------
TileDrawFourHorizontal:
;INPUT: mapx/y in d3
;       x/y in d4
;OUTPUT: source ptr in d5; dest ptr in d1

    move.w #$09F0,BLTCON0(a6)                               ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)                               ;custom->bltcon1 = 0;
    move.w #$FFFF,BLTAFWM(a6)                               ;custom->bltafwm = 0xFFFF;
    move.w #$FFFF,BLTALWM(a6)                               ;custom->bltalwm = 0xFFFF;
    move.w #tile_bytes_per_row-8,BLTAMOD(a6)                ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    move.w #screen_bpl_bytes_per_row-8,BLTDMOD(a6)          ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l d5,BLTAPTH(a6)                                   ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l d1,BLTDPTH(a6)                                   ;custom->bltdpt  = frontbuffer + y + x;

    move.w #(tile_plane_lines*64+4),BLTSIZE(a6)             ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);
.end
    rts

;-----------------------------------------------
TileDrawFiveHorizontal:
;INPUT: mapx/y in d3
;       x/y in d4
;OUTPUT: source ptr in d5; dest ptr in d1

    move.w #$09F0,BLTCON0(a6)                               ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)                               ;custom->bltcon1 = 0;
    move.w #$FFFF,BLTAFWM(a6)                               ;custom->bltafwm = 0xFFFF;
    move.w #$FFFF,BLTALWM(a6)                               ;custom->bltalwm = 0xFFFF;
    move.w #tile_bytes_per_row-10,BLTAMOD(a6)                ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    move.w #screen_bpl_bytes_per_row-10,BLTDMOD(a6)          ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l d5,BLTAPTH(a6)                                   ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l d1,BLTDPTH(a6)                                   ;custom->bltdpt  = frontbuffer + y + x;

    move.w #(tile_plane_lines*64+5),BLTSIZE(a6)             ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);
.end
    rts

;-----------------------------------------------
TileDrawTwoVertical:
;INPUT: mapx/y in d3
;       x/y in d4
;OUTPUT: source ptr in d5; dest ptr in d1

    move.w #$09F0,BLTCON0(a6)                               ;custom->bltcon0 = 0x9F0;   // use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)                               ;custom->bltcon1 = 0;
    move.w #$FFFF,BLTAFWM(a6)                               ;custom->bltafwm = 0xFFFF;
    move.w #$FFFF,BLTALWM(a6)                               ;custom->bltalwm = 0xFFFF;
    move.w #tile_bytes_per_row-2,BLTAMOD(a6)                ;custom->bltamod = BLOCKSBYTESPERROW - (BLOCKWIDTH / 8);
    move.w #screen_bpl_bytes_per_row-2,BLTDMOD(a6)          ;custom->bltdmod = BITMAPBYTESPERROW - (BLOCKWIDTH / 8);
    move.l d5,BLTAPTH(a6)                                   ;custom->bltapt  = blocksbuffer + mapy + mapx;
    move.l d1,BLTDPTH(a6)                                   ;custom->bltdpt  = frontbuffer + y + x;

    move.w #(tile_plane_lines*2*64+1),BLTSIZE(a6)           ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);
.end
    rts

;-----------------------------------------------
