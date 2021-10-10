    INCDIR ""
    INCLUDE "photon/PhotonsMiniWrapper1.04!.S"
    INCLUDE "photon/Blitter-Register-List.S"

    INCLUDE "common.s"
    INCLUDE "tile.s"
    INCLUDE "test.asm"
    INCLUDE "scroll.asm"

*******************************************************************************
* DEFINES
*******************************************************************************

*******************************************************************************
* GAME
*******************************************************************************
Init:
    movem.l d0-a6,-(sp)

    lea Screen,a1
    lea screen_bytes_per_row*tile_height(a1),a1
    bsr.w ClearScreen

; some test code

    bsr TESTCode

    bsr DecodeTileGraphicToLongBitmap
    bsr CopyScreenFromDecodedLongBitmap

    lea Screen,a0                                           ;ptr to first bitplane of image
    lea 2+screen_bytes_per_row*tile_height(a0),a0           ;+2 because we're scrollin' (Skip first column)
    move.l a0,ScrollScreen

    lea CopBplP,a1                                          ;where to poke the bitplane pointer words.
    move #4-1,d0

.bpl7:
    move.l a0,d1
    swap d1
    move.w d1,2(a1)                                         ;hi word
    swap d1
    move.w d1,6(a1)                                         ;lo word

    addq #8,a1                                              ;point to next bpl to poke in copper
    lea screen_bp_bytes_per_raster_line(a0),a0              ;every 44 bytes we'll have new bitplane data
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
    bsr TESTScroll
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

    lea TilesToDecode,a2
    lea ScrollDataLev1,a1
    bsr TESTLoadLevel1Tiles
    rts

;-----------------------------------------------
TESTUpdatePaletteDuringScroll:
    movem.l d0-a6,-(sp)

    lea TestScrollCommand,a0                                ;0=r;1=l;d=2;u=3;rd=4;ru=5;ld=6;lu=7
    lea TileXYPosition,a2                                   ;TileXYPosition (upper left of screen)

    cmp.b #0,(a0)
    bne .check_left

    cmp.w #66,2(a2)                                         ;palette switch column
    blo .continue

    lea CopPalPtrs,a2

    move.w #$0b87,2(a2)
    move.w #$0754,6(a2)
    move.w #$0975,10(a2)
    move.w #$0ca8,14(a2)
    move.w #$0ed8,18(a2)
    move.w #$0fff,22(a2)
    move.w #$0060,26(a2)
    move.w #$0090,30(a2)
    move.w #$00e0,34(a2)
    move.w #$0777,38(a2)
    move.w #$0aaa,42(a2)
    move.w #$0747,46(a2)
    move.w #$0868,50(a2)
    move.w #$0a8a,54(a2)
    move.w #$0cac,58(a2)
    move.w #$0111,62(a2)

    bra .continue

.check_left

    cmp.b #1,(a0)
    bne .continue

    cmp.w #66,2(a2)                                         ;palette switch column
    bhi .continue

    lea CopPalPtrs,a2

    move.w #$0111,2(a2)
    move.w #$0FF9,6(a2)
    move.w #$0EC7,10(a2)
    move.w #$0DA6,14(a2)
    move.w #$0C85,18(a2)
    move.w #$0A74,22(a2)
    move.w #$0864,26(a2)
    move.w #$0753,30(a2)
    move.w #$0641,34(a2)
    move.w #$0533,38(a2)
    move.w #$0431,42(a2)
    move.w #$0111,46(a2)
    move.w #$0111,50(a2)
    move.w #$0111,54(a2)
    move.w #$0111,58(a2)
    move.w #$0110,62(a2)

.continue
    movem.l (sp)+,d0-a6
    rts

;-----------------------------------------------
UpdateSaveWordRightScroll:
   lea PtrSaveWord,a3
   lea SaveWord,a4
   lea PreviousScrollDir,a5
   cmp.b #1,(a5)                                            ;if (previous_direction == DIRECTION_LEFT)
   bne .rupdate_saveword

   WAITBLIT                                                 ;HardWaitBlit();
   move.w (a4),(a3)                                         ;*savewordpointer = saveword;

.rupdate_saveword


;TODO: THIS CODE CAUSES A GURU MEDITATION :---)

;   clr.l d1
;   clr.l d2
;
;   lea Screen,a5                                            ;frontbuffer
;   move.l a5,d2
;   move.w d4,d1                                             ;x
;   asr.w #3,d1                                              ;(x / 8)
;   add.l d1,d2                                              ;frontbuffer + (x / 8)
;
;   clr.l d1
;
;   swap d4                                                  ;y
;   move.w d4,d1
;   add.w #tile_plane_lines-1,d1                             ;(y + tile_plane_lines - 1)
;   mulu #screen_width/2,d1                                  ;* bitmap_bytes_per_row
;   add.l d1,d2
;   move.l d2,a3                                             ;savewordpointer = (WORD *)(frontbuffer + (y + tile_plane_lines - 1) * bitmap_bytes_per_row + (x / 8));
;   move.w (a3),(a4)                                         ;saveword = *savewordpointer;
;   swap d4                                                  ;x

    rts

;-----------------------------------------------
TESTScrollRight:
;INPUT:a0
   cmp.w #0,d1
   bne .update_horz_scroll_position

   ;tile is completely scrolled through; time to move the pointers

   lea TileXYPosition,a2                                    ;TileXYPosition (upper left of screen)

   addi.w #1,2(a2)
   cmp.w #1,2(a2)                                           ;If we're just starting, skip to the end
   beq .update_horz_scroll_position

   cmp.w #128,2(a2)
   beq .no_update

   move.l #1,d4
   move.l ScrollScreen,d3
   lea CopHorzScrollPos,a1
   lea CopBplP,a2
   bsr ScrollUpdateBitplanePointers
   rts

.no_update
   move.w #0,2(a2)                                          ;Tile position

.update_horz_scroll_position

   lea CopHorzScrollPos,a1                                  ;Copper Horizontal Scroll pos (ptr + 2)
   move.w d0,2(a1)                                          ;update copper

   rts

;-----------------------------------------------
TESTScrollLeft:
;INPUT:a0
   cmp.w #15,d1
   bne .update_horz_scroll_position

   ;tile is completely scrolled through; time to move the pointers

   lea TileXYPosition,a2                                    ;TileXYPosition (upper left of screen)

   subi.w #1,2(a2)

   cmp.w #-1,2(a2)
   beq .no_update

.update
   move.l #$0000FFFF,d4
   move.l ScrollScreen,d3
   lea CopHorzScrollPos,a1
   lea CopBplP,a2
   bsr ScrollUpdateBitplanePointers
   bra .update_horz_scroll_position

.no_update
   move.w #127,(a2)

.update_horz_scroll_position

   lea CopHorzScrollPos,a1                                  ;Copper Horizontal Scroll pos (ptr + 2)
   move.w d0,2(a1)                                          ;update copper

   rts

;-----------------------------------------------
TESTScrollDown:
;INPUT:a0
   lea TileXYPosition,a2                                    ;TileXYPosition (upper left of screen)

   addi.w #1,(a2)

   cmp.w #16,(a2)
   beq .no_update

   move.l #$10000,d4
   move.l ScrollScreen,d3
   lea CopHorzScrollPos,a1
   lea CopBplP,a2
   bsr ScrollUpdateBitplanePointers
   rts

.no_update
   move.w #15,(a2)                                          ;Tile position
   rts

;-----------------------------------------------
TESTScrollUp:
;INPUT:a0
   lea TileXYPosition,a2                                    ;TileXYPosition (upper left of screen)

   subi.w #1,(a2)

   cmp.w #-1,(a2)
   beq .no_update

   move.l #$FFFF0000,d4
   move.l ScrollScreen,d3
   lea CopHorzScrollPos,a1
   lea CopBplP,a2
   bsr ScrollUpdateBitplanePointers
   rts

.no_update
   move.w #0,(a2)                                          ;Tile position
   rts

;-----------------------------------------------
UpdateSaveWordLeftScroll:                                   ;OUTPUT: mapx/y in d3; video x/y in d4
   lea PtrSaveWord,a3
   lea SaveWord,a4
   lea PreviousScrollDir,a5
   cmp.b #0,(a5)                                            ;if (previous_direction == DIRECTION_RIGHT)
   bne .lupdate_saveword

   WAITBLIT                                                 ;HardWaitBlit();
   move.w (a4),(a3)                                         ;*savewordpointer = saveword;

.lupdate_saveword

;TODO: THIS CODE CAUSES A GURU MEDITATION :---)

;   clr.l d1
;   clr.l d2
;   lea Screen,a5
;   move.l a5,d2                                             ;frontbuffer
;   move.w d4,d1                                             ;x
;   asr.w #3,d1                                              ;(x / 8)
;   add.l d1,d2                                              ;frontbuffer + (x / 8)
;
;;TODO: CONVERT y (in d4) TO SCREEN BUFFER COORDS; RIGHT NOW IT IS IN BIG BITMAP COORDS
;   clr.l d1
;   swap d4                                                  ;y
;   move.w d1,d4
;   mulu #screen_width/2,d1                                  ;* bitmap_bytes_per_row
;   add.l d1,d2
;   move.l d2,a3                                             ;savewordpointer = (WORD *)(frontbuffer + y * bitmap_bytes_per_row + (x / 8));
;   move.w (a3),(a4)                                         ;saveword = *savewordpointer;
;   swap d4                                                  ;x

   lea PreviousScrollDir,a3
   move.b #1,(a3)                                           ;previous_direction = DIRECTION_LEFT;

   rts

;-----------------------------------------------
TESTScroll:

   bsr TESTUpdatePaletteDuringScroll
   lea MapXYPosition,a3
   lea ScrollPositions,a5
   bsr ScrollGetStepAndDelay

   lea TestScrollCommand,a0                                 ;0=r;1=l;d=2;u=3;rd=4;ru=5;ld=6;lu=7

   cmp.b #16,(a0)                                           ;16=kill code
   bne .continue
   rts

.continue
   cmp.b #0,(a0)                                            ;user move left... do left
   beq .right

   cmp.b #1,(a0)                                            ;user move left... do left
   beq .left

   cmp.b #2,(a0)                                            ;user move left... do left
   beq .down

   cmp.b #3,(a0)                                            ;user move left... do left
   beq .up

   cmp.b #4,(a0)                                            ;user move left... do left
   beq .rightdown

   cmp.b #5,(a0)                                            ;user move left... do left
   beq .rightup

   cmp.b #6,(a0)                                            ;user move left... do left
   beq .leftdown

   cmp.b #7,(a0)                                            ;user move left... do left
   beq .leftup

   rts

.right
    ;if (mapposx >= (mapwidth * BLOCKWIDTH - SCREENWIDTH - BLOCKWIDTH)) return;
    move.b #1,d3
    cmp.w #(test_cols_to_decode*tile_width-screen_width-tile_width*2),d2              ;2048-352-tile_width*2
    ;cmp.w #129,d2
    blo .scroll_right
    bra .switch_direction

.scroll_right

   bsr TESTScrollRight                                      ;INPUT:d2,a0 (d1)
   lea MapXYPosition,a3
   bsr ScrollGetXYPositionRight
   bsr UpdateSaveWordRightScroll                            ;OUTPUT: mapx/y in d3; video x/y in d4
   bsr CalculateDrawHTile
   bsr DrawTile
   
   lea MapXYPosition,a3
   lea VideoXYPosition,a4
   lea PreviousScrollDir,a5
   bsr ScrollIncrementXPosition                             ;INPUT: mapx/y in d3; x/y in d4
   rts

.left
    move.b #0,d3
    cmp.w #tile_width,d2
    ;cmp.w #95,d2
    bhi .scroll_left
    bra .switch_direction

.scroll_left

   lea MapXYPosition,a3
   lea VideoXYPosition,a4
   lea ScrollPositions,a5
   bsr ScrollDecrementXPosition
   bsr TESTScrollLeft

   lea MapXYPosition,a3
   lea TileYBlitPositions,a5
   bsr ScrollGetXYPositionLeft
   bsr UpdateSaveWordLeftScroll                             ;OUTPUT: mapx/y in d3; video x/y in d4
   bsr CalculateDrawHTile
   bsr DrawTile                                             ;DrawBlock(x,y,mapx,mapy);
   rts

.up
   rts
   
.scroll_up
;   lea MapXYPosition,a3
;   lea VideoXYPosition,a4
;   lea ScrollPositions,a5
;   bsr ScrollDecrementYPosition
   bsr TESTScrollUp

;   lea MapXYPosition,a3
;   lea TileXBlitPositions,a5
;   bsr ScrollGetXYPositionUp
;   bsr CalculateDrawVTile
;   bsr DrawTile                                             ;DrawBlock(x,y,mapx,mapy);
   rts

.down
    move.b #16,d3
    swap d2
    cmp.w #screen_height,d2
    blo .scroll_down
    bra .switch_direction

.scroll_down

   bsr TESTScrollDown                                      ;INPUT:d2,a0 (d1)
   bsr ScrollGetXYPositionDown
;   bsr CalculateDrawVTile
;   bsr DrawTile
;   lea MapXYPosition,a3
;   lea VideoXYPosition,a4
;   lea PreviousScrollDir,a5
;;   bsr ScrollIncrementYPosition                             ;INPUT: mapx/y in d3; x/y in d4
   rts

.rightup
   rts

.rightdown
   rts

.leftup
   rts

.leftdown
   rts

.switch_direction
   move.b d3,(a0)
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
    move.l a3,d3
    move.l a4,d4
    add.l #screen_bytes_per_row*tile_height,d4

    add.l #127*2,d3                                         ;move source to last column

    move.w #$09F0,BLTCON0(a6)                               ;use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)
    move.w #$FFFF,BLTAFWM(a6)
    move.w #$FFFF,BLTALWM(a6)
    move.w #254,BLTAMOD(a6)                                 ;skip 127 columns (copy 1)
    move.w #42,BLTDMOD(a6)                                  ;skip 21 columns (copy 1)
    move.l d3,BLTAPTH(a6)
    move.l d4,BLTDPTH(a6)

    move.w #1,BLTSIZE(a6)                                   ;one column

    WAITBLIT

    lea DecodedGraphic,a3
    lea Screen,a4
    lea 2+screen_bytes_per_row*tile_height(a4),a4           ;initially skip first column of pixels
    move.l a3,d3
    move.l a4,d4

    move.w #214,BLTAMOD(a6)                                 ;skip 107 columns (copy 21)
    move.w #2,BLTDMOD(a6)                                   ;skip 1 column (copy 21)
    move.l d3,BLTAPTH(a6)
    move.l d4,BLTDPTH(a6)

    move.w #(screen_width-tile_width)/16,BLTSIZE(a6)        ;no "h" term needed since it's 1024. Thanks ross @eab!
;
;    WAITBLIT
;
;    lea DecodedGraphic,a3
;    lea Screen,a4
;    add.l #2+screen_bytes_per_row*(screen_height+tile_height),a4
;    move.l a3,d3
;    move.l a4,d4
;
;    move.w #214,BLTAMOD(a6)                                 ;skip 107 columns (copy 21)
;    move.w #2,BLTDMOD(a6)                                   ;skip 1 column (copy 21)
;    move.l d3,BLTAPTH(a6)
;    move.l d4,BLTDPTH(a6)
;
;    move.w #tile_plane_lines*64+(screen_width-tile_width)/16,BLTSIZE(a6)
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

    move.b, 3(a2),test_bmp_bp_bytes_per_raster_line*0(a3)           ;bitplane 0
    move.b, 2(a2),test_bmp_bp_bytes_per_raster_line*1(a3)           ;bitplane 1
    move.b, 1(a2),test_bmp_bp_bytes_per_raster_line*2(a3)           ;bitplane 2
    move.b, (a2),test_bmp_bp_bytes_per_raster_line*3(a3)            ;bitplane 3
    move.b, 7(a2),test_bmp_bp_bytes_per_raster_line*0+1(a3)         ;bitplane 0
    move.b, 6(a2),test_bmp_bp_bytes_per_raster_line*1+1(a3)         ;bitplane 1
    move.b, 5(a2),test_bmp_bp_bytes_per_raster_line*2+1(a3)         ;bitplane 2
    move.b, 4(a2),test_bmp_bp_bytes_per_raster_line*3+1(a3)         ;bitplane 3

    lea test_bmp_bp_bytes_per_raster_line*tile_bitplanes(a3),a3     ;move down one rasterline ($400 = $100 * 4 bitplanes; $100 = bytes in one rasterline for one bitplane)
    lea $02(a1),a1                                                  ;source bytes per rasterline are 2 bytes apart

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
CalculateDrawHTile:
;INPUT: mapx/y in d3
;       x/y in d4
;       x = in pixels
;       y = in "planelines" (1 realline = BLOCKSDEPTH planelines)

    ;SOURCE => d5 (d3=offset)

    clr.l d1
    clr.l d2
    clr.l d5

    swap d3                                                 ;mapy
    swap d4                                                 ;y

    move.w d3,d2
    cmp.w #0,d2
    beq .skip_add
    sub.w #1,d2

.addo                                                       ;mapy * mapwidth
    add.l #$4000,d1
    dbf d2,.addo

.skip_add

    swap d3                                                 ;mapx
    move.w d3,d2

    cmp.b #0,(a0)
    bne .left

    subi #1,d2                                              ;back one column

.left
    asl.w #1,d2                                             ;mapx=col;*2=bp byte offset

    ;FOR DEBUGGING: COMMENT THIS OUT; it will always choose the same source tile
    add.l d2,d1                                             ;source offset = mapy * mapwidth + mapx
    move.l d1,d3                                            ;for debugging purposes

    WAITBLIT                                                ;HardWaitBlit();

    lea DecodedGraphic,a3

    move.l a3,d5                                            ;A source (blocksbuffer)
    add.l d1,d5                                             ;blocksbuffer + mapy + mapx

    ;DESTINATION => d1 (d4)
    move.l ScrollScreen,d1                                     ;D dest (frontbuffer)

    clr.l d2

    cmp.b #0,(a0)
    bne .left2

    lea VideoXBitplaneOffset,a3
    add.w (a3),d2                                           ;always either one bitplane pointer down (because of shift)
                                                            ;or zero
.left2
    lea MapXYPosition,a3
    move.w 2(a3),d4                                         ;x
    and.w #15,d4

    lea ScrollDestinationOffsets,a3
    asl.w #1,d4
    add.w (a3,d4.w),d2

    move.l d2,d4                                            ;(for debugging)
    add.l d2,d1                                             ;frontbuffer + y + x
    rts

;-----------------------------------------------
DrawTile:
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

    move.w #(tile_plane_lines*64+1),BLTSIZE(a6)             ;custom->bltsize = BLOCKPLANELINES * 64 + (BLOCKWIDTH / 16);

;    cmp.l #$3C056,d3                                        ;can stop after a particular tile is blitted by
;    bne .end                                                 ;doing something like this
;
;    lea TestScrollCommand,a0                                ;0=r;1=l;d=2;u=3;rd=4;ru=5;ld=6;lu=7
;    move.b #16,(a0)

.end
    rts

;-----------------------------------------------
DrawTwoHorizontalTiles:
;INPUT: mapx/y in d3
;       x/y in d4
;OUTPUT: source ptr in d5; dest ptr in d1

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
ClearScreen:                                                                            ;a1=screen destination address to clear
    WAITBLIT
    clr.w $66(a6)                                                                       ;destination modulo
    move.l #$01000000,$40(a6)                                                           ;set operation type in BLTCON0/1
    move.l a1,$54(a6)                                                                   ;destination address
    move.w #screen_height*bpls*64+screen_bp_bytes_per_raster_line/2,$58(a6)             ;blitter operation size
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

ScrollScreen:
    dc.l 0

ScrollDestinationOffsets:
    dc.w $0000,$0B00,$1600,$2100,$2C00,$3700,$4200,$4D00
    dc.w $5800,$6300,$6E00,$7900,$8400,$8F00,$9A00,$A500

SaveWord:
    dc.w 0

VideoXBitplaneOffset:
    dc.w screen_bpl_bytes_per_row-2

ScrollPositions:
    dc.b $FF,$EE,$DD,$CC,$BB,$AA,$99,$88
    dc.b $77,$66,$55,$44,$33,$22,$11,$00

TileYBlitPositions:
    dc.b $F0,$E0,$D0,$C0,$B0,$A0,$90,$80
    dc.b $70,$60,$50,$40,$30,$20,$10,$00


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
    dc.w DDFSTRT,DMA_fetch_start                            ;$28 for 22 columns; $38 for 20 columns (etc)
    dc.w DDFSTOP,$d0

    dc.w BPL1MOD,screen_modulo
    dc.w BPL2MOD,screen_modulo

CopHorzScrollPos:
    dc.w BPLCON1,$00
    dc.w BPLCON2,0

CopPalPtrs:
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
    ds.b screen_bpl_bytes_per_row*screen_bitplanes*(screen_buffer_height+2)
ScreenE:

    EVEN

DecodedGraphic:
    ds.b $80000                                             ;bitmapwidth/16*tile_bitplanes*vlines_per_graphic
DecodedGraphicE:


