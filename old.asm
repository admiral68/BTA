;OLD CODE THAT MAY BE USEFUL
;"block" here means 16x16 "block" of tiles

block_width_pixels          = 256
block_horz_disp_words       = block_width_pixels/tile_width
block_bp_bytes_per_raster_line = block_horz_disp_words*2
block_vtile_offset          = block_bp_bytes_per_raster_line*tile_bitplanes*tile_height
block_tiles_height          = 16
block_byte_size             = block_vtile_offset*block_tiles_height

CopyScreenFromDecodedLongBitmapOldButWorked:
;Does a rectangular blit over the entire screen at once, with the source being 2048px wide
    lea DecodedGraphic,a3
    lea Screen,a4
    move.l a3,d3
    move.l a4,d4

    move.w #$09F0,BLTCON0(a6)                               ;use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)
    move.w #$FFFF,BLTAFWM(a6)
    move.w #$FFFF,BLTALWM(a6)
    move.w #216,BLTAMOD(a6)
    move.w #0,BLTDMOD(a6)
    move.l d3,BLTAPTH(a6)
    move.l d4,BLTDPTH(a6)

    move.w #screen_width/16,BLTSIZE(a6)                     ;no "h" term needed since it's 1024. Thanks ross @eab!
    rts

;-----------------------------------------------
ExtractTileOldest:
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
ExtractTileOld:
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

    move.b, 3(a2),block_bp_bytes_per_raster_line*0(a3)         ;bitplane 0
    move.b, 2(a2),block_bp_bytes_per_raster_line*1(a3)         ;bitplane 1
    move.b, 1(a2),block_bp_bytes_per_raster_line*2(a3)         ;bitplane 2
    move.b, (a2),block_bp_bytes_per_raster_line*3(a3)          ;bitplane 3
    move.b, 7(a2),block_bp_bytes_per_raster_line*0+1(a3)       ;bitplane 0
    move.b, 6(a2),block_bp_bytes_per_raster_line*1+1(a3)       ;bitplane 1
    move.b, 5(a2),block_bp_bytes_per_raster_line*2+1(a3)       ;bitplane 2
    move.b, 4(a2),block_bp_bytes_per_raster_line*3+1(a3)       ;bitplane 3

    lea block_bp_bytes_per_raster_line*tile_bitplanes(a3),a3   ;move down one rasterline ($80 = $20 * 4 bitplanes; $20 = bytes in one rasterline for one bitplane)
    lea $02(a1),a1                                          ;source bytes per rasterline are 2 bytes apart

    dbf d0,.extract_tile
    rts

;-----------------------------------------------
DecodeTileGraphicToScreenOldest:
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
    andi.w #tile_index_mask,d0
    asl.l #$06,d0
    lea (a1,d0.l),a1

    bsr ExtractTileOldest

    lea TileColumnsToDecode,a4
    lea TileDecodeDest,a2
    add.l #2,(a2)
    addi.b #1,d4

    move.l d4,d6
    divu #screen_width/16,d6                                 ;turns out screen_width/16 (20?) columns fits exactly into one interleaved bitplane section
    swap d6
    cmp.w #0,d6
    bne .check_loop

    add.l #(screen_width*8-screen_width/8),(a2)               ;16 vertical lines and 4 bitplanes away

.check_loop
    cmp.b (a4),d4
    bne .loop_columns

    ;TODO: HERE WE NEED TO FIGURE OUT HOW MANY BYTES ARE LEFT IN THE INTERLEAVED SECTION
    ;FOR EACH OF THE BITPLANES

    ;SINCE WE DEFINED OUR BITMAP AS 320PX wide, we only have 20 horizontal words to work with
    ;If we decoded fewer than 20 columns (for 320px) then we should make up the difference

    cmp.b #20,d4
    blo .do_ragged_row

    move.l d4,d6
    divu #screen_width/16,d6                                 ;turns out screen_width/16 columns fits exactly into one interleaved bitplane section
    swap d6
    cmp.w #0,d6
    beq .continue_row

.do_ragged_row
    lea TileDecodeRowDest,a4
    move.l (a4),a1
    lea TileDecodeDest,a2
    lea DestGraphicVTileOffset,a3                           ;One tile height in destination bitmap

    adda.l (a3),a1
    move.l a1,(a2)
    move.l a1,(a4)

.continue_row
    lea TileRowsToDecode,a4
    addi.b #1,d5
    cmp.b (a4),d5
    bne .loop_rows

    rts

;-----------------------------------------------
DecodeTileGraphicToScreenOld:
    ;SCREEN LO-RES
    ;W: 320=screen; W=256 for blocks
    ;8 pixels/byte
    ;32 bytes per line/16 words per line
    ;done to show how to extract Black Tiger-encoded image data
    ;32*32*4

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
    andi.w #tile_index_mask,d0
    asl.l #$06,d0
    lea (a1,d0.l),a1

    bsr ExtractTileOld                                          ;TODO: THIS needs to work with the correct stride

    lea TileColumnsToDecode,a4
    lea TileDecodeDest,a2
    add.l #2,(a2)
    addi.b #1,d4

    move.l d4,d6
    divu #block_width_pixels/16,d6                            ;block_width_pixels/16 = 16
    swap d6
    cmp.w #0,d6
    bne .check_loop

    add.l #(block_width_pixels*8-block_width_pixels/8),(a2)   ;16 vertical lines and 4 bitplanes away

.check_loop
    cmp.b (a4),d4
    bne .loop_columns

    ;TODO: HERE WE NEED TO FIGURE OUT HOW MANY BYTES ARE LEFT IN THE INTERLEAVED SECTION
    ;FOR EACH OF THE BITPLANES

    ;We only have 16 horizontal words to work with for the tile block
    ;If we decoded fewer than 16 columns (for 256px) then we should make up the difference

    cmp.b #16,d4
    blo .do_ragged_row

    move.l d4,d6
    divu #block_width_pixels/16,d6                          ;block_width_pixels/16 = 16
    swap d6
    cmp.w #0,d6
    beq .continue_row

.do_ragged_row
    lea TileDecodeRowDest,a4
    move.l (a4),a1
    lea TileDecodeDest,a2
    lea DestGraphicVTileOffset2,a3                          ;One tile height in destination bitmap

    adda.l (a3),a1
    move.l a1,(a2)
    move.l a1,(a4)

.continue_row
    lea TileRowsToDecode,a4
    addi.b #1,d5
    cmp.b (a4),d5
    bne .loop_rows

    rts

;-----------------------------------------------
CopyScreenFromDecodedGraphicOldest:
;Does a rectangular blit over the entire screen at once
    lea DecodedGraphic,a3
    lea Screen,a4
    move.l a3,d3
    move.l a4,d4

    ;TODO: REMEMBER THESE ARE NOT THE SAME WIDTH NOW!
    ;HAVE TO DO TWO BLITS OR SOMETHING

    move.w #$09F0,BLTCON0(a6)                               ;use A and D. Op: D = A
    move.w #$0000,BLTCON1(a6)
    move.w #$FFFF,BLTAFWM(a6)
    move.w #$0000,BLTALWM(a6)
    move.w #0,BLTAMOD(a6)
    move.w #0,BLTDMOD(a6)
    move.l d3,BLTAPTH(a6)
    move.l d4,BLTDPTH(a6)

    move.w #screen_width/16,BLTSIZE(a6)                     ;no "h" term needed since it's 1024. Thanks ross @eab!
    rts

;-----------------------------------------------
CopyScreenFromDecodedGraphicOlder:
;Does a square blit and a strip blit

    lea DecodedGraphic,a3
    lea Screen,a4

    move.l a3,d3
    move.l a4,d4

    move.w #$09F0,BLTCON0(a6)                               ;use A, D. Op: D = A
    move.w #$0000,BLTCON1(a6)
    move.w #$FFFF,BLTAFWM(a6)
    move.w #$FFFF,BLTALWM(a6)

    move.w #0,BLTAMOD(a6)                                   ;256px wide (16 tiles)
    move.w #8,BLTDMOD(a6)                                   ;320px wide (20 tiles) - 4 tiles
    move.l d3,BLTAPTH(a6)
    move.l d4,BLTDPTH(a6)

    move.w #$10,BLTSIZE(a6)                                 ;16 tiles/cols

    WAITBLIT

    add.l #$8000,d3                                        ;Move to next 256x256 px tile block
    add.l #32,d4                                           ;Move to right side of screen

    move.w #24,BLTAMOD(a6)                                 ;256px wide; first 4 tiles
    move.w #32,BLTDMOD(a6)                                 ;320px wide; last 4 cols/tiles
    move.l d3,BLTAPTH(a6)
    move.l d4,BLTDPTH(a6)

    move.w #$4,BLTSIZE(a6)                                 ;4 tiles/cols

    rts

;-----------------------------------------------
TESTLoadLevel1Tiles2:
    ;THIS DECODES THE TILES IN BIG BLOCK LAYOUT (16x16 TILES; HORIZONTALLY) (8x4 BIG BLOCKS). 16 tiles horizontally
    ;then wrap to the next row of tiles

    ;In the source, the tile scroll data is formatted in blocks of tiles 16 x 16. These blocks are $200 bytes in size a piece.
    ;"8x4" means 8 of these big blocks wide by 4 of those blocks high

    ;The Black Tiger screen was only 256x224, meaning that a maximum of 16 columns and 14 rows of tiles could be displayed
    ;on screen at once anyway. I checked and there are only 16 tile columns and 13 tile rows used.
    ;The top tile row is skipped, so the first (visible) map tiles start on row 1 (not row 0).

    ; HB+(BITS0-2 of LB << 8) = TILE SELECTION  ((0x7 & LB) << 8) + HB    ==> INDEX VALUES BETWEEN 0x000 & 0x7FF
    ; (BITS3-6 of LB)         = PALETTE INDEX                             ==> INDEX VALUES BETWEEN 0x0 & 0xF
    ; BIT 7 of LB:            = FLIP HORIZONTALLY
    ;
    ; ATTR:
    ;   BITS 0-2 = UPPER 3 BITS OF TILE INDEX (ABOVE LOWER 8 bits of index--so we can index up to 0x7FF)
    ;   BITS 3-6 = COLOR INFO (0x0 - 0xF) (16 possible PALETTES) (raw PALETTE index)
    ;   BIT  7   = FLIP TILE (0x0 = NO, 0x1 = YES)

    lea TileColumnsToDecode,a1
    move.b #test_cols_to_decode,(a1)

    lea TileRowsToDecode,a1
    move.b #test_rows_to_decode,(a1)

    lea DestGraphicVTileOffset,a1                           ;One tile height in destination bitmap
    move.l #bitpl_bytes_per_raster_line*tile_bitplanes*tile_height,(a1)

    lea DestGraphicVTileOffset2,a1
    move.l #block_vtile_offset,(a1)

    lea DestGraphicVTileOffset3,a1
    move.l #test_bmp_vtile_offset,(a1)

    lea EncodedTilesSource,a0
    lea TileSource,a1
    move.l a0,(a1)

    ;BELOW HERE

    move.l #0,d2
    move.l #0,d3

    lea TilesToDecode,a2
    lea ScrollDataLev1,a1
    movea.l a1,a3
    move.l #test_rows_to_decode-1,d0

.outer_loop
    move.l #test_cols_to_decode-1,d1

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
    dbf d1,.inner_loop

    move.l #0,d4
    lea $100(a3),a3
    movea.l a3,a1

    dbf d0,.outer_loop

    rts

;-----------------------------------------------
TESTLoadLevel1Tiles:
    ;THIS DECODES THE TILES IN ONE LONG STRIP SO THAT IT FOLLOWS THE MAP LAYOUT (2048x1024). 256 tiles horizontally
    ;then wrap to the next row of tiles

    lea TileColumnsToDecode,a1
    move.b #test_cols_to_decode,(a1)

    lea TileRowsToDecode,a1
    move.b #test_rows_to_decode,(a1)

    lea DestGraphicVTileOffset,a1                           ;One tile height in destination bitmap
    move.l #bitpl_bytes_per_raster_line*tile_bitplanes*tile_height,(a1)

    lea DestGraphicVTileOffset2,a1
    move.l #block_vtile_offset,(a1)

    lea DestGraphicVTileOffset3,a1
    move.l #test_bmp_vtile_offset,(a1)

    lea EncodedTilesSource,a0
    lea TileSource,a1
    move.l a0,(a1)

    move.l #0,d2
    move.l #0,d3
    move.l #0,d4

    lea TilesToDecode,a2
    lea ScrollDataLev1,a1
    movea.l a1,a3
    move.l #test_rows_to_decode-1,d0

.outer_loop
    move.l #test_cols_to_decode-1,d1

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
    move.l #0,d4

.skip_1
    dbf d1,.inner_loop

    move.l #0,d4
    lea $20(a3),a3
    movea.l a3,a1

    dbf d0,.outer_loop

    rts

;-----------------------------------------------
DestGraphicVTileOffset:
    dc.l 0

DestGraphicVTileOffset2:
    dc.l 0

ScrollUpdateSaveWordRight:
   cmp.b #1,v_scroll_previous_direction(a0)                 ;if (previous_direction == DIRECTION_LEFT)
   bne .rupdate_saveword

   WAITBLIT                                                 ;HardWaitBlit();
   move.w v_scroll_saveword(a0),v_scroll_ptr_saveword(a0)   ;*savewordpointer = saveword;

.rupdate_saveword


;TODO: THIS CODE CAUSES A GURU MEDITATION :---)

;   clr.l d1
;   clr.l d2
;
;   lea Screen,a2                                            ;frontbuffer
;   move.l a2,d2
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
;   move.l d2,a4                                             ;savewordpointer = (WORD *)(frontbuffer + (y + tile_plane_lines - 1) * bitmap_bytes_per_row + (x / 8));
;   move.w (a4),v_scroll_saveword(a3)                        ;saveword = *savewordpointer;
;   swap d4                                                  ;x

    rts

;-----------------------------------------------
ScrollUpdateSaveWordLeft:                                   ;OUTPUT: mapx/y in d3; video x/y in d4
   cmp.b #0,v_scroll_previous_direction(a0)                 ;if (previous_direction == DIRECTION_RIGHT)
   bne .lupdate_saveword

   WAITBLIT                                                 ;HardWaitBlit();
   move.w v_scroll_saveword(a0),v_scroll_ptr_saveword(a0)   ;*savewordpointer = saveword;

.lupdate_saveword

;TODO: THIS CODE CAUSES A GURU MEDITATION :---)

;   clr.l d1
;   clr.l d2
;   lea Screen,a2
;   move.l a2,d2                                             ;frontbuffer
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
;   move.l d2,a4                                             ;savewordpointer = (WORD *)(frontbuffer + y * bitmap_bytes_per_row + (x / 8));
;   move.w (a4),v_scroll_saveword(a0)                        ;saveword = *savewordpointer;
;   swap d4                                                  ;x

   move.b #1,v_scroll_previous_direction(a0)                 ;previous_direction = DIRECTION_LEFT;

   rts

;-----------------------------------------------
ScrollGetVTileOffsetsOld:
;INPUT: mapx/y in d3
;       y step in d4

    ;SOURCE => d5 (d3=offset)

    clr.l d1
    clr.l d2
    clr.l d5

    swap d3                                                 ;mapy

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

    asl.w #1,d2                                             ;mapx=col;*2=bp byte offset

    ;FOR DEBUGGING: COMMENT THE NEXT LINE OUT; it will always choose the same source tile
    ;add.l d2,d1                                             ;source offset = mapy * mapwidth + mapx

    move.l d1,d3                                            ;for debugging purposes

    WAITBLIT                                                ;TODO: PUT BACK IN WHEN NOT DEBUGGING

    move.l a3,d5                                            ;A source (blocksbuffer)
    add.l d1,d5                                             ;blocksbuffer + mapy + mapx

****************** DESTINATION **************************

    ;DESTINATION => d1 (d4)
    clr.l d3
    move.w d4,d3                                            ;step;keep this for blit
    move.l v_scroll_screen_split(a0),d1                     ;D dest (frontbuffer)

*************** ADJUST PIXEL ROW ************************
; subtract d4*screen_bytes_per_row because we're moving *

    cmp.w #0,d3
    beq .get_relative_location

    swap d4
    move.w d3,d4
    sub.w #1,d4

.adjust_pixel_row
    sub.l #screen_bytes_per_row,d1
    dbf d4,.adjust_pixel_row

    swap d4

************* GET RELATIVE LOCATION *********************
.get_relative_location

    clr.l d2
    move.l v_screen(a0),d2

    btst.b #1,v_scroll_command(a0)
    beq .up2

    sub.l #screen_bytes_per_row*tile_height,d1              ;top fill row
    bra .check_past_end_of_buffer

.up2
    add.l #screen_bytes_per_row*screen_height,d1            ;bottom fill row

*********** CHECK PAST END OF BUFFER ********************
.check_past_end_of_buffer
    add.l #screen_bytes_per_row*screen_buffer_height,d2

    cmp.l d2,d1
    blt .add_column_offsets

    sub.l d2,d1
    add.l v_screen(a0),d1

************** ADD COLUMN OFFSETS ***********************
.add_column_offsets
    clr.l d2
    move.w v_map_y_position(a0),d4
    swap d4
    move.w v_map_y_position(a0),d4
    and.l #$000F000F,d4

    asl.w #1,d4
    add.w v_scrolly_dest_offset_table(a0,d4.w),d2

    move.l d2,d4                                            ;(for debugging)
    add.l d2,d1                                             ;frontbuffer + y + x

********** CHECK BEFORE START OF BUFFER *****************
.check_before_start_of_buffer
    clr.l d2
    move.l v_screen(a0),d2

    cmp.l d2,d1
    bge .figure_out_num_blocks_to_blit

    sub.l d1,d2
    beq .figure_out_num_blocks_to_blit

    move.l #screen_bytes_per_row*screen_buffer_height,d1

    add.l v_screen(a0),d1
    sub.l d2,d1

.figure_out_num_blocks_to_blit
    cmp.w #5,d3
    ble .single

    and.w #1,d3
    bne .single

    moveq #1,d3
    rts

.single
    moveq #0,d3
    rts

;-----------------------------------------------
ScrollGetXYPositionDown2:
;returns mapx/y in d3
    ;get source ptrs

    clr.l d4
    move.w v_map_y_position(a0),d3                          ;save for mapy
    move.w d3,d4
    and.w #15,d4
    swap d3
    move.w v_map_x_position(a0),d3                          ;mapposx
    asr.w #4,d3                                             ;mapposx / BLOCKWIDTH

    swap d3

    asr.w #4,d3                                             ;mapposy / BLOCKHEIGHT

    cmp.w #map_tile_height,d3                               ;This is because the
    ble .save_mapy                                          ;source bitmap is only map_tile_height blocks high

    sub.w #map_tile_height,d3

.save_mapy

    swap d4
    move.w d3,d4                                            ;mapy in d4
    swap d4

    add.w #screen_rows+1,d3                                 ;mapy+17--row under visible screen
    cmp.w #screen_buffer_rows,d3
    blt .end

    sub.w #screen_buffer_rows,d3

.end

    swap d3
    rts

;-----------------------------------------------
ScrollGetXYPositionUp2:
;returns mapx/y in d3
    clr.l d4
    move.w v_map_y_position(a0),d3                          ;save for mapy
    move.w d3,d4
    and.w #15,d4
    swap d3
    move.w v_map_x_position(a0),d3                          ;mapposx
    asr.w #4,d3                                             ;mapposx / BLOCKWIDTH

    swap d3

    asr.w #4,d3                                             ;mapposy / BLOCKHEIGHT
    sub.w #1,d3                                             ;-1--row above visible screen
    bpl .end

    add.w #map_tile_height,d3

.end
    swap d3
    rts

;-----------------------------------------------
    cmp.w #0,d2                                             ;first split--reset split pointer
    bne .update_split

    ;if there is no split, the bitplane pointers should be reset
    ;the code gets here first for scroll down


    bra .update_split ;temporarily skip this code





    move.l d1,d6

    move.l v_scroll_screen_top(a0),v_scroll_screen_split(a0)

    move.l #tile_height*2*screen_bytes_per_row,d7

    ;TODO: If scrolling down, add screen_bytes_per_row "scroll y velocity" times
    ;here we're just adding it once (scroll y velocity=1 pixel)
    btst.b #1,v_scroll_command(a0)                          ;if downward scroll, move down a scan row
    beq .add_offset

    add.l #screen_bytes_per_row,d7                          ;just for down scroll, bitplane pointer is off by one row

.add_offset
    add.l d7,d6
    add.l d7,v_scroll_screen_split(a0)
;-----------------------------------------------
TESTScrollRight:
;INPUT:a0(FAST DATA)
    cmp.w #0,d1
    bne .update_horz_scroll_position

   ;tile is completely scrolled through; time to move the pointers

    addi.w #1,v_tile_x_position(a0)
    cmp.w #1,v_tile_x_position(a0)                          ;If we're just starting, skip to the end
    beq .update_horz_scroll_position

    move.l #1,d4
    lea Copper,a1
    bsr ScrollUpdateBitplanePointers

.update_horz_scroll_position

    ;lea Copper,a1                                           ;Copper Horizontal Scroll pos
    ;move.w d0,c_horizontal_scroll_pos_01(a1)                ;update copper

    rts
;-----------------------------------------------
TESTScrollLeft:
;INPUT:a0
    cmp.w #15,d1
    bne .update_horz_scroll_position

    ;tile is completely scrolled through; time to move the pointers

    subi.w #1,v_tile_x_position(a0)

    move.l #$0000FFFF,d4
    lea Copper,a1
    bsr ScrollUpdateBitplanePointers

.update_horz_scroll_position

    ;lea Copper,a1                                           ;Copper Horizontal Scroll pos
    ;move.w d0,c_horizontal_scroll_pos_01(a1)                ;update copper

    rts
TESTCopyScreenFromDecodedLongBitmapForRightScroll:
;Does a rectangular blit to the the visible screen, then a strip blit from the edge
    ;lea DecodedGraphic,a3
    ;lea Screen,a4

    move.l  a3,d3
    move.l  a4,d4
    add.l   #2+screen_bytes_per_row*tile_height,d4          ;blits to the upper left corner of the visible area - 2

    add.l   #127*2,d3                                       ;move source to last column

    move.w  #$09F0,BLTCON0(a6)                              ;use A and D. Op: D = A
    move.w  #$0000,BLTCON1(a6)
    move.w  #$FFFF,BLTAFWM(a6)
    move.w  #$FFFF,BLTALWM(a6)
    move.w  #254,BLTAMOD(a6)                                ;skip 127 columns (copy 1)
    move.w  #42,BLTDMOD(a6)                                 ;skip 21 columns (copy 1)
    move.l  d3,BLTAPTH(a6)
    move.l  d4,BLTDPTH(a6)

    move.w  #1,BLTSIZE(a6)                                  ;one column

    WAITBLIT

    move.l  a3,d3
    move.l  a4,d4
    add.l   #4+screen_bytes_per_row*tile_height,d4

    move.w  #214,BLTAMOD(a6)                                ;skip 107 columns (copy 21)
    move.w  #2,BLTDMOD(a6)                                  ;skip 1 column (copy 21)
    move.l  d3,BLTAPTH(a6)
    move.l  d4,BLTDPTH(a6)

    move.w  #(screen_width-tile_width)/16,BLTSIZE(a6)       ;no "h" term needed since it's 1024. Thanks ross @eab!
    rts
;-----------------------------------------------
TESTUpdatePaletteDuringScroll:
    movem.l d0-a6,-(sp)

    btst.b #3,v_joystick_value(a0)                          ;left?
    bne .check_left

    btst.b #0,v_joystick_value(a0)                          ;right?
    beq .continue

    cmp.w #66,v_tile_x_position(a0)                         ;palette switch column
    blo .continue

    lea Copper,a2

    move.w #$0b87,c_palette_01(a2)
    move.w #$0754,c_palette_01+4(a2)
    move.w #$0975,c_palette_01+8(a2)
    move.w #$0ca8,c_palette_01+12(a2)
    move.w #$0ed8,c_palette_01+16(a2)
    move.w #$0fff,c_palette_01+20(a2)
    move.w #$0060,c_palette_01+24(a2)
    move.w #$0090,c_palette_01+28(a2)
    move.w #$00e0,c_palette_01+32(a2)
    move.w #$0777,c_palette_01+36(a2)
    move.w #$0aaa,c_palette_01+40(a2)
    move.w #$0747,c_palette_01+44(a2)
    move.w #$0868,c_palette_01+48(a2)
    move.w #$0a8a,c_palette_01+52(a2)
    move.w #$0cac,c_palette_01+56(a2)
    move.w #$0111,c_palette_01+60(a2)

    bra .continue

.check_left

    cmp.w #66,v_tile_x_position(a0)                         ;palette switch column
    bhi .continue

    lea Copper,a2

    move.w #$0111,c_palette_01(a2)
    move.w #$0FF9,c_palette_01+4(a2)
    move.w #$0EC7,c_palette_01+8(a2)
    move.w #$0DA6,c_palette_01+12(a2)
    move.w #$0C85,c_palette_01+16(a2)
    move.w #$0A74,c_palette_01+20(a2)
    move.w #$0864,c_palette_01+24(a2)
    move.w #$0753,c_palette_01+28(a2)
    move.w #$0641,c_palette_01+32(a2)
    move.w #$0533,c_palette_01+36(a2)
    move.w #$0431,c_palette_01+40(a2)
    move.w #$0111,c_palette_01+44(a2)
    move.w #$0111,c_palette_01+48(a2)
    move.w #$0111,c_palette_01+52(a2)
    move.w #$0111,c_palette_01+56(a2)
    move.w #$0110,c_palette_01+60(a2)

.continue
    movem.l (sp)+,d0-a6
    rts

;-----------------------------------------------
DecodeAndAssembleSourceTilesIntoMapSourceBitmap:
    ;SCREEN LO-RES
    ;W: 2048; H=256
    ;8 pixels/byte
    ;32 bytes per line/16 words per line
    ;done to show how to extract Black Tiger-encoded image data
    ;32*32*4

    lea MapSourceBitmap,a0
    lea FastData,a1

    move.l a0,v_tile_map_row_dest(a1)
    move.l (MapSourceBitmapE-MapSourceBitmap)/4,d0

.l0:
    clr.l (a0)+
    dbf d0,.l0

    clr.l d0

    lea Map,a0                                    ;Starting tile
    lea FastData,a2
    lea v_tile_map_row_dest(a2),a1

    move.l (a1),v_tile_map_dest(a2)
    move.l #0,d5

.loop_rows
    move.l #0,d4

.loop_columns
    move.l v_tile_map_dest(a2),a3

    move.b #0,d1
    move.l v_tile_source(a2),a1
    move.w (a0)+,d0
    btst #15,d0
    beq .no_flip
    move.b #1,d1
.no_flip
    andi.w #tile_index_mask,d0
    asl.l #$06,d0
    lea (a1,d0.l),a1

    lea FastData,a2
    lea v_decoded_bitplane_bytes(a2),a5                     ;stores intermediate decoded bitplane bytes
    bsr TESTExtractTile

    lea FastData,a2
    lea v_current_map_columns(a2),a4
    add.l #2,v_tile_map_dest(a2)
    addi.b #1,d4

.check_loop
    cmp.b (a4),d4
    bne .loop_columns

    lea FastData,a2
    move.l v_tile_map_row_dest(a2),a1
    lea v_map_bytes_per_tile_row(a2),a3                     ;One tile height in destination bitmap

    adda.l (a3),a1
    move.l a1,v_tile_map_dest(a2)
    move.l a1,v_tile_map_row_dest(a2)

    lea v_current_map_rows(a2),a4
    addi.b #1,d5
    cmp.b (a4),d5
    bne .loop_rows

    rts

;-----------------------------------------------
;where current Y-BLOCK


;TODO: FIX THIS SO IT IS THE SAME AS WHAT WE GET WHEN BLITTING



;    move.w  v_map_previous_step(a0),d3
;    move.w  v_map_y_position(a0),d6
;    asr.w   #4,d6                                           ;current Y-block


;is not the same as with last X movement

;    cmp.w   v_map_previous_map_y_block(a0),d6               ;Y-block of last x movement
;    beq     .continue
;    blt     .makeup_tiles_below_current_row
;
;;makeup tiles blitted will be above current row--we've moved down
;
;    ;mcgeezer_special2
;    sub.w   v_map_previous_map_y_block(a0),d6               ;number of Y blocks to blit. If > 15, blit the entire column!
;
;;MOVING LOWER, WE SUBTRACT STEPS
;    sub.w   d6,d3
;    and.w   #15,d3
;    bra     .makeup_tiles_blit
;
;;makeup tiles blitted will be below current row--we've moved up
;.makeup_tiles_below_current_row
;
;    ;mcgeezer_special2
;    move.w  v_map_previous_map_y_block(a0),d7               ;Y-block of last x movement
;    sub.w   d6,d7
;    exg     d6,d7
;
;;MOVING HIGHER, WE ADD STEPS
;    add.w   d6,d3
;    and.w   #15,d3
;
;.makeup_tiles_blit



;THE FOLLOWING IS EVEN LESS LIKELY TO WORK



;    mcgeezer_special
;
;    clr.l   d7
;
;    bsr     ScrollGetDirectionalVectors
;
;    move.w  v_previous_scroll_vector(a0),d1
;    cmp.w   v_scroll_vector_x(a0),d1                        ;movement same as last frame?
;    beq     .continue
;
;    tst.b   v_previous_scroll_vector(a0)                    ;any x movement in the last frame?
;    beq     .continue
;
;    tst.b   v_previous_scroll_vector+1(a0)                  ;any y movement in the last frame? If not, we don't need to fill anything
;    beq     .continue
;
;    ;fill unblitted blocks based on previous position
;
;   mcgeezer_special2
;
;   move.w  v_scroll_vector_x(a0),d2
;   move.w  d1,v_scroll_vector_x(a0)
;   move.w  d2,v_previous_scroll_vector(a0)
;
;    move.w  v_map_x_position(a0),d7
;    and.w   #15,d7
;
;    tst.w   d7                                              ;we were moving horizontally on a zero boundary; all blocks were blitted
;    beq     .continue                                       ;skip this section
;
;    cmp.b   #1,v_scroll_vector_x(a0)                        ;were we moving right?
;    bne     .unblitted_set_left_position
;    bsr     ScrollDecrementXPosition
;    bra     .unblitted_set_sources
;
;.unblitted_set_left_position
;
;    move.w  v_map_x_position(a0),d7
;    move.w  d7,v_map_previous_x_position(a0)
;    and.w   #$FFF0,d7
;    move.w  d7,v_map_x_position(a0)
;
;   ;these lines cause down scroll to blit one row higher and up scroll to blit one row lower
;   move.b  #16,d6
;   sub.b   v_scroll_vector_y(a0),d6
;   move.b  d6,v_scroll_vector_y(a0)
;
;.unblitted_set_sources
;    bsr     ScrollGetStepAndDelay
;    bsr     ScrollGetMapXYForHorizontal2
;
;    lea     MapSourceBitmap,a3
;    lea     MapSourceBitmapE,a5
;    bsr     ScrollGetHTileOffsets2
;
;    cmp.b   #1,v_scroll_vector_x(a0)                        ;were we moving right?
;    bne     .unblitted_compute_left
;
;    move.w  v_map_x_position(a0),d7
;    and.w   #15,d7
;
;    bsr     ScrollIncrementXPosition
;
;    move.l  d7,d6
;    move.l  #15,d7
;    sub.l   d6,d7
;    clr.l   d6
;    move.w  #screen_bpl_bytes_per_row,d6
;    sub.l   d6,d1
;   sub.l   d6,d5
;
;    bra     .blit_unblitted
;
;.unblitted_compute_left
;
;    move.w  #screen_bpl_bytes_per_row,d6
;    add.l   d6,d1
;   add.l   d6,d5
;
;    cmp.b   #15,v_scroll_vector_y(a0)                      ;were we also going up?
;   bne     .skip_unblitted_left_adjustment
;    sub.w  #2,d1
;.skip_unblitted_left_adjustment
;    move.w  v_map_previous_x_position(a0),d7
;    move.w  d7,v_map_x_position(a0)
;    and.w   #15,d7
;
;    ;switch directions back
;   move.b  #16,d6
;   sub.b   v_scroll_vector_y(a0),d6
;   move.b  d6,v_scroll_vector_y(a0)
;
;.blit_unblitted
;    ;mcgeezer_special2
;    ;move.l  a3,d5
;    ;add.l   #$2d000,d5
;    asl.w   #2,d7
;    lea     TileDrawVerticalJumpTable,a4
;    move.l  (a4,d7.w),a4
;    jsr     (a4)
;
;   move.w v_previous_scroll_vector(a0),d1
;   move.w  v_scroll_vector_x(a0),d2
;   move.w  d1,v_scroll_vector_x(a0)
;   move.w  d2,v_previous_scroll_vector(a0)

TileDrawVerticalHandleVSplit:
;INPUT:d7 (number of blocks to blit);d1 DEST;d5 SRC
.start
    mcgeezer_special2
    move.w  d7,d2                                           ;counter
    move.l  d1,d7

    cmp.w   #13,d2
    bge     .handle_13_plus

    move.l  #-1,d6

.loop_count
    add.l   #1,d6
    add.l   #screen_tile_bytes_per_row,d7
    cmp.l   v_screen_end(a0),d7
    bge     .first_blit_counted
    dbf     d2,.loop_count

.first_blit_counted
    cmp.w   #-1,d2
    bne     .skip_set_to_zero
    move.l  #0,d2

.skip_set_to_zero
    swap    d6
    move.w  d2,d6
    move.l  d6,d2

    asl.w   #2,d6
    lea     TileDrawVerticalJumpTable,a4
    move.l  (a4,d6.w),a4
    jsr     (a4)

    move.l  d2,d6
    swap    d2
    tst.w   d2
    beq     .end

    ;second part of blit, starting from top of buffer

    mcgeezer_special2

    WAITBLIT

    sub.w	#1,d6
    clr.l   d7

.loop_find_top_of_split
    sub.l   #screen_tile_bytes_per_row,d1
    cmp.l   v_screen(a0),d1
    bge     .loop_find_top_of_split

    add.l   #screen_tile_bytes_per_row,d1
	
    move.w  v_map_bytes_per_tile_row(a0),d7
	
.loop_find_source_row	
    add.l   d7,d5
	dbf		d6,.loop_find_source_row

    cmp.l   a5,d5
    blt     .do_second_blit
    sub.l   v_map_bytes(a0),d5

.do_second_blit
    swap    d6
    asl.w   #2,d6
    lea     TileDrawVerticalJumpTable,a4
    move.l  (a4,d6.w),a4
    jmp     (a4)

.handle_13_plus

    cmp.w   #13,d2
    bgt     .try_14

    bsr     TileDraw
    bra     .blit_12

.try_14
    cmp.w   #14,d2
    bgt     .try_15

    bsr     TileDraw
    bsr     .move_down_a_row
    bsr     TileDraw
    bra     .blit_12

.try_15
    bsr     TileDraw
    bsr     .move_down_a_row
    bsr     TileDraw
    bsr     .move_down_a_row
    bsr     TileDraw

.blit_12

    bsr     .move_down_a_row
    move.w  #12,d7
    bra     .start

.move_down_a_row

    WAITBLIT

    clr.l   d7
    move.w  v_map_bytes_per_tile_row(a0),d7

    add.l   #screen_tile_bytes_per_row,d1
    add.l   d7,d5

    cmp.l   v_screen_end(a0),d1
    blt     .skip_screen_adjustment

    sub.l   #screen_buffer_bytes,d1

.skip_screen_adjustment

    cmp.l   a5,d5
    blt     .end
    sub.l   v_map_bytes(a0),d5

.end
    rts

;------------------------------------------------------
    *********************************
    *****  BLIT DIRTY BLOCKS    *****
    *********************************

;ALGORITHM: New X movement where Y-BLOCK is not the same as with last X movement

    bsr     ScrollGetDirectionalVectors

;New

    move.w  v_previous_scroll_vector(a0),d1
    cmp.w   v_scroll_vector_x(a0),d1                        ;movement same as last frame?
    beq     .continue

;X movement where

    tst.b   v_scroll_vector_x(a0)                           ;any x movement in the current frame?
    beq     .continue

;Y-BLOCK
    move.w  v_map_y_position(a0),d6
    asr.w   #4,d6                                           ;current Y-block

;is not the same as with last X movement

    cmp.w   v_map_previous_map_y_block(a0),d6               ;Y-block of last x movement
    beq     .continue

    bsr     ScrollGetMapXYForHorizontal2
    move.w  #0,d3                                           ;start at first step and reblit blocks up to x-step
    move.w  v_map_x_position(a0),d6                         ;Number of blocks to blit = x-step
    and.w   #15,d6

;d6=number of blocks to blit;d3(H):starting Y block;d3(L):step

    cmp.b   #1,v_scroll_vector_x(a0)                        ;are we moving right?
    bne     .makeup_tiles_set_sources
    bsr     ScrollDecrementXPosition

.makeup_tiles_set_sources
    lea     MapSourceBitmap,a3
    lea     MapSourceBitmapE,a5
    bsr     ScrollGetHTileOffsets2

    cmp.b   #15,v_scroll_vector_x(a0)                        ;left?
    bne     .makeup_tiles_blit

    ;mcgeezer_special2
    move.l  a3,d5           ;DEBUG: SETS CLIMBING BONES AS TILE BLIT SOURCE
    ;add.l   #$5007e,d5      ;DEBUG: SETS CLIMBING BONES AS TILE BLIT SOURCE
    add.l   #$2d000,d5      ;DEBUG: SETS STONE WALL AS TILE BLIT SOURCE

.makeup_tiles_blit
    move.w  d6,d7
    tst.w   d7
    beq     .makeup_tiles_skip_blit

    bsr     TileDrawVerticalHandleVSplit
    ;asl.w   #2,d6
    ;lea     TileDrawVerticalJumpTable,a4                    ;TODO: FIX JUMP TABLE TO SEPARATE TALL BLITS ON VERTICAL SPLIT
    ;move.l  (a4,d6.w),a4
    ;jsr     (a4)

.makeup_tiles_skip_blit
    cmp.b   #1,v_scroll_vector_x(a0)                        ;are we moving right?
    bne     .continue

    bsr     ScrollIncrementXPosition

;-------------------------------------------------
    *****  DOWN RIGHT  *****

    cmp.b   #1,v_scroll_vector_x(a0)                        ;right?
    bne     .check_left

    tst.w   d4
    bne     .single

    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?
    bne     .adjust_right

    move.w  v_map_x_position(a0),d2
    and.w   #15,d2
    sub.w   #1,d2
    beq     .adjust_right

    and.w   #15,d2
    move.w  d2,d3
    add.w   d2,d2

    cmp.w   #4,d3
    blt     .subtract

    cmp.w   #14,d3
    bge     .subtract

    cmp.w   #11,d3
    blt     .account_for_double_blocks_down_right
    add.w   #2,d2

.account_for_double_blocks_down_right
    add.w   #2,d2

.subtract
    sub.w   d2,d5
    sub.w   d2,d1

.adjust_right
    add.w   v_video_x_bitplane_offset(a0),d5
    add.w   v_video_x_bitplane_offset(a0),d1
    move.l  a3,d5
    add.l   #$2d000,d5
    bra     .single

    *****   UP LEFT    *****

.check_left
    cmp.b   #15,v_scroll_vector_x(a0)                       ;left?
    bne     .single

    cmp.w   #$F,d4
    bne     .single

    cmp.b   #15,v_scroll_vector_y(a0)                        ;up?
    bne     .adjust_left_down

    move.w  v_map_x_position(a0),d2
    add.w   #1,d2
    and.w   #15,d2

    add.w   d2,d2
    move.w  v_scrolly_dest_offset_table(a0,d2.w),d2

    sub.w   d2,d5
    sub.w   d2,d1
    move.l  a3,d5
    add.l   #$2d000,d5
    bra     .single

.adjust_left_down
    add.w   #2,d1
    add.w   #2,d5
    sub.w   v_video_x_bitplane_offset(a0),d5
    sub.w   v_video_x_bitplane_offset(a0),d1
    move.l  a3,d5
    add.l   #$2d000,d5
    bra     .single
;-------------------------------------
ScrollGetMapXYForHorizontal:
;INPUT: fast data (a0)
;returns mapx/y in d3

    move.w  v_map_x_position(a0),d3                         ;save for mapy
    and.w   #15,d3                                          ;mapy = mapposx & (NUMSTEPS - 1);
    swap    d3

    move.w  v_map_x_position(a0),d3                         ;mapposx
    asr.w   #4,d3                                           ;mapx = mapposx / BLOCKWIDTH

    rts
;-------------------------------------
ScrollGetMapXYForVertical:
;returns mapx/mapy (source blocks) in d3
;        scroll step/mapy (dest block) in d4

;at this point, the scroll bitplane pointer has already moved down
;and the vertical split has been calculated

    clr.l   d4
    move.w  v_map_y_position(a0),d3                         ;save for mapy
    move.w  d3,d4                                           ;mapposy (if scrolling down, we're one behind)
    and.w   #15,d4                                          ;scroll step y
    swap    d3

    move.w  v_map_x_position(a0),d3                         ;mapposx
    asr.w   #4,d3                                           ;mapx (block)

    swap    d3                                              ;mapposy
    asr.w   #4,d3                                           ;mapy (block)

    swap    d4
    move.w  d3,d4                                           ;mapy (block) in d4

    cmp.b   #1,v_scroll_vector_y(a0)                        ;if downward scroll, continue
    bne     .do_upward

    add.w   #15,d4
    cmp.b   v_map_tile_height(a0),d4
    blt     .save_mapy

    clr.l   d7
    move.b  v_map_tile_height(a0),d7
    sub.w   d7,d4
    bra     .save_mapy

.do_upward
    sub.w   #1,d4
    bpl     .save_mapy

    clr.w   d4
    move.b  v_map_tile_height(a0),d4
    sub.w  #1,d4


.save_mapy

    swap    d4                                              ;scroll step y

;destination block in d3

.end
    swap    d3                                              ;mapx (block)
    rts

;-----------------------------------------------
ScrollGetHTileOffsets:
;INPUT: mapx/y in d3
;       x = in pixels
;       y = in "planelines" (1 realline = BLOCKSDEPTH planelines)
;       MapSourceBitmap=a3;MapSourceBitmapE=a5
;SOURCE => d5 (d3=offset)

****************************************
***           SOURCE                 ***
****************************************

    clr.l   d1
    clr.l   d2
    clr.l   d5
    clr.l   d6

    move.w  v_map_x_position(a0),d4

    move.w  v_map_bytes_per_tile_row(a0),d5
    move.w  d5,d6

    move.w  d4,d7
    and.w   #15,d7                                          ;x-step

    move.w  v_map_y_position(a0),d2                         ; TASK (2)
    asr.w   #4,d2                                           ; TASK (2)
    tst.w   d2                                              ; TASK (2)
    beq     .skip_add_base                                  ; TASK (2)

;    cmp.b   #15,v_scroll_vector_y(a0)                       ;up?  TASK (2)
;    bne     .start_add_base                                 ; TASK (2)
    move.w  v_map_y_position(a0),d2                         ; TASK (2)
    sub.w   #1,d2                                           ; TASK (2)
    asr.w   #4,d2                                           ; TASK (2)
    add.w   #1,d2                                           ; TASK (2)

.start_add_base
    sub.w   #1,d2                                           ; TASK (2)

.add_base                                                   ; TASK (2)
    add.l   d5,d1                                           ; TASK (2)
    dbf     d2,.add_base                                    ; TASK (2)

.skip_add_base                                              ; TASK (2)

    swap    d3                                              ;mapy

    move.w  v_map_x_position(a0),d3                         ;save for mapy ; TASK (2)
    and.w   #15,d3                                          ;mapy = mapposx & (NUMSTEPS - 1); ; TASK (2)

    move.w  d3,d2

.check_add

    tst.w   d2
    beq     .find_source_column

    sub.w   #1,d2

.addo                                                       ;mapy * mapwidth
    add.l   d5,d1
    dbf     d2,.addo

.find_source_column
    clr.l   d5
    swap    d3                                              ;mapx
    move.w  d3,d2

    cmp.b   #1,v_scroll_vector_x(a0)
    bne     .left

    subi    #1,d2                                           ;back a column

.left
    subi    #1,d2
    bpl     .asl
    clr.w   d2
.asl
    asl.w   #1,d2                                           ;mapx=col;*2=bp byte offset

    ;FOR DEBUGGING: COMMENT THE NEXT LINE OUT; it will always choose the same source tile
    add.l   d2,d1                                           ;source offset = mapy * mapwidth + mapx
    move.l  d1,d3                                           ;for debugging purposes

    WAITBLIT                                                ;HardWaitBlit();

    move.l  a3,d5                                           ;A source (blocksbuffer)
    add.l   d1,d5                                           ;blocksbuffer + mapy + mapx

    sub.l   d6,d5                                           ;We're starting one source row higher

    ;IF the source pointer is out of range, skip the blit
    cmp.l   a3,d5
    bge     .check_past_end_of_source

    add.l   d6,d5
    add.l   v_map_bytes(a0),d5

.check_past_end_of_source
    cmp.l   a5,d5
    blt     .destination
    sub.l   v_map_bytes(a0),d5

****************************************
***         DESTINATION              ***
****************************************

.destination

    ;DESTINATION => d1
    move.l  v_scroll_screen(a0),d1                          ;D dest (frontbuffer)

    moveq   #0,d2

    move.w  v_map_x_position(a0),d3                         ; TASK (2)
    and.w   #15,d3

    move.w  v_map_y_position(a0),d2                         ; TASK (2)
    asr.w   #4,d2                                           ; TASK (2)    ; mapy block

    tst.w   d2                                              ; TASK (2)
    beq     .continue_add_dest_offset                       ; TASK (2)

;    cmp.b   #15,v_scroll_vector_y(a0)                       ;up?  TASK (2)
;    bne     .continue_add_dest_offset                       ; TASK (2)
    ;mcgeezer_special2
    move.w  v_map_y_position(a0),d2                         ; TASK (2)
    sub.w   #1,d2                                           ; TASK (2)
    asr.w   #4,d2                                           ; TASK (2)    ; mapy block
    add.w   #1,d2                                           ; TASK (2)

.continue_add_dest_offset
    add.w   d2,d4                                           ; TASK (2)    ; step + mapy block
    moveq   #0,d2                                           ; TASK (2)
    and.w   #15,d4                                          ;x-step

    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?   TASK (2)
    bne     .check_up                                       ; TASK (2)

    tst.w     d3                                            ; TASK (2) SKIP FIRST BLIT
    bne     .continue_to_blit                               ; TASK (2)

.none                                                       ; TASK (2)
    moveq   #0,d7                                           ; TASK (2)
    rts                                                     ; TASK (2)

.check_up
    cmp.b   #15,v_scroll_vector_y(a0)                       ;up?  TASK (2)
    bne     .continue_to_blit                               ; TASK (2)

    cmp.b   #15,d3                                          ; TASK (2) SKIP LAST BLIT
    beq     .none                                           ; TASK (2)

.continue_to_blit
    move.w  d4,d6

    asl.w   #1,d4
    add.w   v_scrollx_dest_offset_table(a0,d4.w),d2

    cmp.b   #1,v_scroll_vector_x(a0)                        ;moving right?
    bne     .left2

    add.w   v_video_x_bitplane_offset(a0),d2                ;VideoXBitplaneOffset: always either one bitplane pointer down (because of shift)
                                                            ;or zero
    bra     .get_step

.left2
    tst.w   d7
    beq     .get_step
    sub.l   #2,d2                                           ;last column

.get_step

    move.l  d2,d4                                           ;(for debugging)
    add.l   d2,d1                                           ;frontbuffer + y + x

.single
    moveq   #1,d7
    asl.w   #2,d7
    rts

;-----------------------------------------------
ScrollGetVTileOffsets:
;INPUT: mapx/mapy(offset for dest) in d3
;       y step/actual mapy(source) in d4

    ;SOURCE => d5 (d3=offset)

    clr.l   d1
    clr.l   d2
    clr.l   d5

    move.w  v_map_bytes_per_tile_row(a0),d5

    move.l  d3,d6                                           ;for destination

    swap    d4                                              ;actual mapy(source)
    move.w  d4,d2
    swap    d4                                              ;y step

    cmp.w   #0,d2
    beq     .skip_add
    sub.w   #1,d2

.addo                                                       ;mapy * mapwidth
    add.l   d5,d1
    dbf     d2,.addo

.skip_add
    clr.l   d5

    move.w  d3,d2                                           ;mapx
    asl.w   #1,d2                                           ;mapx=col;*2=bp byte offset

    ;FOR DEBUGGING: COMMENT THE NEXT LINE OUT; it will always choose the same source tile
    sub.l   #2,d1                                           ;NEW CODE--takes care of column 0

    add.l   d2,d1                                           ;source offset = mapy * mapwidth + mapx
    move.w  d3,d2                                           ;mapx
    move.l  d1,d3                                           ;for debugging purposes

    WAITBLIT                                                ;TODO: PUT BACK IN WHEN NOT DEBUGGING

    move.l  a3,d5                                           ;A source (blocksbuffer)
    ;FOR DEBUGGING: COMMENT THE NEXT LINE OUT; it will always choose the same source tile
    add.l   d1,d5                                           ;blocksbuffer + mapy + mapx

;IF the source pointer is out of range, skip the blit

    moveq   #0,d7

; TASK (2)
    ;tst.w   d2                                              ;blitting into first column?
    ;bne     .destination

    ;tst.w   d4                                              ;If on the first step and blitting into column 0,
    ;beq     .none                                           ;skip the blit
; TASK (2)

****************** DESTINATION **************************

.destination
    ;DESTINATION => d1 (d4)
    clr.l   d3
    move.w  d4,d3                                           ;y step;keep this for blit
    move.l  v_scroll_screen(a0),d1                          ;D dest (frontbuffer)

    swap    d6                                              ;mapy(offset for dest)
    move.w  d6,d2
    and.w   #15,d2                                          ; TASK (2)

    move.w  #1,d6
    cmp.b   #1,v_scroll_vector_y(a0)                        ;scrolling down?
    beq     .subtract
    add.w   #1,d6

.subtract
    sub.w   d6,d2
    bpl     .add_rows
    add.w   #screen_buffer_rows,d2

************* CONVERT MAPY TO VIDEOY ********************

.add_rows
    tst.w   d2
    beq     .add_column_offsets

    move.w  d2,d6                                           ;debug
    sub.w   #1,d2

.loop_add_tile_row                                          ;videoy * screenwidth

    add.l   #screen_tile_bytes_per_row,d1
    dbf     d2,.loop_add_tile_row

************** ADD COLUMN OFFSETS ***********************
.add_column_offsets
    clr.l   d2

    move.w  v_map_y_position(a0),d4
    swap    d4
    move.w  v_map_y_position(a0),d4
    and.l   #$000F000F,d4

    asl.w   #1,d4
    add.w   v_scrolly_dest_offset_table(a0,d4.w),d2
    add.l   d2,d1                                           ;destination offset = mapy * mapwidth + mapx

    move.w  v_map_x_position(a0),d2                         ;when X is on an uneven tile boundary, compensate
    and.w   #$000f,d2                                       ;by blitting one block to the left
    beq     .skip_compensate_for_x

    sub.w   #2,d1

.skip_compensate_for_x

    clr.l   d2
    move.w  v_scrolly_dest_offset_table(a0,d4.w),d2
    add.l   d2,d5

.check_past_end_of_source
    cmp.l   a5,d5
    blt     .check_left_blit_zero

    sub.l   v_map_bytes(a0),d5                              ;TODO: Maybe this isn't correct

.check_left_blit_zero
    cmp.b   #15,v_scroll_vector_x(a0)                       ;left? ; TASK (2)
    bne     .figure_out_num_blocks_to_blit                  ; TASK (2)
    tst     d4                                              ; TASK (2)
    bne     .figure_out_num_blocks_to_blit                  ; TASK (2)

    sub.w   #2,d5                                           ; TASK (2)
    cmp.l   a3,d5                                           ; TASK (2)
    bge     .figure_out_num_blocks_to_blit                  ; TASK (2)
    add.l   v_map_bytes(a0),d5                              ;TODO: Maybe this isn't correct ; TASK (2)

.figure_out_num_blocks_to_blit
    moveq   #0,d7
    asr.w   #1,d4

****************************************
***      DOWN SCROLL CHECKS          ***
****************************************


;    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?
;    bne     .check_up
;
;    tst.w   d4
;    beq     .none                                           ;If R, skip [A]
;
;****************************************
;***       UP SCROLL CHECKS           ***
;****************************************
;
;.check_up
;    cmp.b   #15,v_scroll_vector_y(a0)                       ;up?
;    bne     .check_position
;
;    tst.w   d4
;    beq     .none                                           ;If R, skip [C]

.check_position
    cmp.w   #4,d4                                           ;positions 4 & B have doubles
    beq     .double

    cmp.w   #$B,d4                                          ;odd positions > 3 (single block)
    beq     .double

    cmp.b   #1,v_scroll_vector_x(a0)                        ;right? ; TASK (2)
    bne     .check_left                                     ; TASK (2)

    tst.w   d4                                              ; TASK (2)
    bne     .single                                         ; TASK (2)

    cmp.b   #15,v_scroll_vector_y(a0)                       ;up? ; TASK (2)
    bne     .single                                         ; TASK (2)

    add.w   #screen_bpl_bytes_per_row-2,d5                  ; TASK (2)
	sub.w	#2,d1                                           ; TASK (2)
    bra     .single                                         ; TASK (2)

    ;cmp.w   #$F,d4                                          ;R + step F = double blit ; TASK (2)
    ;beq     .single                                         ; TASK (2)

.check_left                                                 ; TASK (2)
    cmp.b   #15,v_scroll_vector_x(a0)                       ;left? ; TASK (2)
    bne     .single                                         ; TASK (2)

    cmp.w   #$F,d4                                          ; TASK (2)
    bne     .single                                         ; TASK (2)
    sub.w   #2,d5                                           ; TASK (2)
    add.w   #screen_bpl_bytes_per_row-2,d1                  ; TASK (2)
    bra     .single                                         ; TASK (2)

;    tst     d4                                              ; TASK (2)
;    bne     .single                                         ; TASK (2)

.double
    addq    #1,d7

.single
    addq    #1,d7
    asl.w   #2,d7
.none
    rts

;-----------------------------------------------
;-------------------------------------
;-------------------------------------
;-------------------------------------
;-------------------------------------
;-------------------------------------
;-------------------------------------
;-------------------------------------
;-------------------------------------
    ;mcgeezer_special2


    bra     .double

    *********************************
    *****    CHECK DIAGONAL     *****
    *********************************

    *****  DOWN RIGHT  *****

;.check_diagonal
;    bra     .single
;
;    cmp.b   #1,v_scroll_vector_x(a0)                        ;right?
;    bne     .check_left
;
;    tst.w   d4
;    bne     .single
;
;    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?
;    bne     .adjust_right
;
;    move.w  v_map_x_position(a0),d2
;    and.w   #15,d2
;    sub.w   #1,d2
;    beq     .adjust_right
;
;    and.w   #15,d2
;    move.w  d2,d3
;    add.w   d2,d2
;
;    cmp.w   #4,d3
;    blt     .subtract
;
;    cmp.w   #14,d3
;    bge     .subtract
;
;    cmp.w   #11,d3
;    blt     .account_for_double_blocks_down_right
;    add.w   #2,d2
;
;.account_for_double_blocks_down_right
;    add.w   #2,d2
;
;.subtract
;    sub.w   d2,d5
;    sub.w   d2,d1
;
;.adjust_right
;    add.w   v_video_x_bitplane_offset(a0),d5
;    add.w   v_video_x_bitplane_offset(a0),d1
;    move.l  a3,d5
;    add.l   #$2d000,d5
;    bra     .single
;
;    *****   UP LEFT    *****
;
;.check_left
;    cmp.b   #15,v_scroll_vector_x(a0)                       ;left?
;    bne     .single
;
;    cmp.w   #$F,d4
;    bne     .single
;
;    cmp.b   #15,v_scroll_vector_y(a0)                        ;up?
;    bne     .adjust_left_down
;
;    move.w  v_map_x_position(a0),d2
;    add.w   #1,d2
;    and.w   #15,d2
;
;    add.w   d2,d2
;    move.w  v_scrolly_dest_offset_table(a0,d2.w),d2
;
;    sub.w   d2,d5
;    sub.w   d2,d1
;    move.l  a3,d5
;    add.l   #$2d000,d5
;    bra     .single
;
;.adjust_left_down
;    add.w   #2,d1
;    add.w   #2,d5
;    sub.w   v_video_x_bitplane_offset(a0),d5
;    sub.w   v_video_x_bitplane_offset(a0),d1
;    move.l  a3,d5
;    add.l   #$2d000,d5
;    bra     .single
;

;-------------------------------------------------------

    ;SPECIAL CASE #4: FINISHED BLITTING A ROW
    ;                 WHEN WE HIT THIS CASE, BLIT TO THE FIRST BLOCK OF THE FILL COLUMN BELOW
    ;                 THE SOURCE (THE END OF THE FILL ROW) BLOCK IS CORRECT
    ;RIGHT FILL BLOCK sometimes; LEFT FILL BLOCK SOMETIMES

.check_special_case_04

;TODO: THE SOURCE BLOCK SHOULD BE ONE ON THE RIGHT, OR LEFT in some cases.
;THIS WHOLE SECTION IS A MESS. CERTAIN BLOCKS GO TO THE LEFT; OTHERS GO TO THE RIGHT.
;FINDING THE BLOCK TO REPLACE (WHICH IS A RIGHT/NORMAL BLOCK) IS A PAIN!
;SITUATION: WE HAVE A SITUATION THAT ONCE IT HAS BLITTED (STEP-x) NUMBER OF REGULAR (R) BLOCKS,
;           WE NEED TO GO TO THE BOTTOM OF THE ROW AND START BLITTING LEFT (FILL) BLOCKS
;  SEEMS WE NEED TO COUNT THE NUMBER OF TIMES WE BLIT ON SPECIAL CASE #3 and then ONLY REPLACE (R) BLOCKS
;  THAT MANY TIMES. OTHERWISE, BLIT AS NORMAL (FILL). WE DON'T KEEP DOING IT IF WE KEEP SCROLLING UP.

    tst.w   d3
    bne     .continue_with_column_offset

    cmp.b   #15,v_scroll_vector_y(a0)                       ;up?
    bne     .continue_with_column_offset

    sub.l   #2,d1

    clr.l   d2
    move.l  v_scroll_screen(a0),d1                          ;D dest (frontbuffer)
    add.l   #screen_tile_bytes_per_row-2,d1
    move.w  v_scroll_y_block_step(a0),d2
    add.w   d4,d2
    and.w   #15,d2
    add.w   d2,d2
    move.w  v_scrollx_dest_offset_table(a0,d2.w),d2
    add.l   d2,d1

    clr.l   d6                                              ;TILE ROW BYTES
    move.w  v_map_bytes_per_tile_row(a0),d6
    sub.w   #1,d2
    move.w  #14,d2

.subbo
    sub.l   d6,d5
    dbf     d2,.subbo

    cmp.l   v_screen(a0),d1
    bge     .continue
    add.l   #screen_buffer_bytes,d1

.continue
    add.l   #screen_columns*2,d1
    cmp.w   #1,d4                           ;SEEMS TO HAPPEN ON THE BLITS STARTING FROM X POSITIONS ENDING IN F
    beq     .move_1

.skip_adjust_source_left
    add.l   #2,d5

.move_1

    add.l   #screen_columns*2-2,d5          ;TRYING TO GET TO THE RIGHT COLUMN SOURCE
    move.l  #$7ef08,d5

    move.w  #0,d7;DEBUG
    bra     .none;DEBUG

    move.w  #1,d7
    bra     .finish

    ;END: SPECIAL CASE #4
;--------------------------------------------------------
    ;SPECIAL CASE #1: FINISHED BLITTING A COLUMN; ON [U] ROW
    ;                 WHEN WE HIT THIS CASE, JUST BLIT THE BLOCK WITHOUT
    ;                 THE OFFSET
	;

    cmp.w   d6,d3
    bne     .add_bitplane_offset

;    move.w  v_video_y_position(a0),d7
;    and.w   #15,d7
;    beq     .add_bitplane_offset

;THIS MAY BE THE CULPRIT!

;DEBUG (COMMENTED OUT)
;    sub.w   #2,d1;DEBUG
;    sub.w   #2,d5;DEBUG
	
	;move.l	#$7EF08,d5
	
	;this should be an UP block


    ;END: SPECIAL CASE #1
;---------------------------------------------
