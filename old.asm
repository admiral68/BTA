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
