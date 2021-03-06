
*******************************************************************************
* TEST ROUTINES
*******************************************************************************
TESTLoadLevel1Map:
;lea Map,a2
;lea TESTMapSource,a1
    ;THIS DECODES THE TILES IN MAP LAYOUT (16x16 TILES). 128 tiles horizontally
    ;then wrap to the next row of tiles

    move.l  #0,d2
    move.l  #0,d3

    move.l  a1,a4                                           ;row
    move.l  #test_rows_to_decode-1,d0

.outer_loop
    move.l  #test_cols_to_decode-1,d1
    move.l  #0,d4

.inner_loop
    move.w  (a1)+,d2                                        ;load "scroll word" into d2 from a1
    ror     #8,d2                                           ;swap bytes; source is little endian
    move.w  d2,d3
    and.w   #tile_index_mask,d3

    btst    #15,d2
    beq     .finish_inner_loop

    or.w    #$8000,d3                                       ;set "flipped" tile index

.finish_inner_loop
    move.w  d3,(a2)+                                        ;poke this into the tile list

    ;Here we need a counter, because every 16 tiles, Black Tiger wraps down

    addi    #1,d4
    move.l  d4,d6
    divu    #16,d6                                          ;test_cols_to_decode/16 = 128
    swap    d6
    cmp.w   #0,d6
    bne     .end_inner_loop

    lea     $1E0(a1),a1                                     ;down to next 16x16 block of tile indexes

.end_inner_loop
    dbf     d1,.inner_loop

    lea     $20(a4),a4                                      ;down one row of 16 tile indexes in src
    movea.l a4,a1

    dbf     d0,.outer_loop

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
    btst    #0,d2
    beq     .byte_1_bit_1

    or.b    #$01,(a2)

.byte_1_bit_1
    btst    #1,d2
    beq     .byte_1_bit_2

    or.b    #$02,(a2)

.byte_1_bit_2
    btst    #2,d2
    beq     .byte_1_bit_3

    or.b    #$04,(a2)

.byte_1_bit_3
    btst    #3,d2
    beq     .byte_2_bit_0

    or.b    #$08,(a2)

.byte_2_bit_0
    btst    #8,d2
    beq     .byte_2_bit_1

    or.b    #$10,(a2)

.byte_2_bit_1
    btst    #9,d2
    beq     .byte_2_bit_2

    or.b    #$20,(a2)

.byte_2_bit_2
    btst    #10,d2
    beq     .byte_2_bit_3

    or.b    #$40,(a2)

.byte_2_bit_3
    btst    #11,d2
    beq     .byte_1_bit_4

    or.b    #$80,(a2)

    ;BPL 0

.byte_1_bit_4
    btst    #4,d2
    beq     .byte_1_bit_5

    or.b    #$01,1(a2)

.byte_1_bit_5
    btst    #5,d2
    beq     .byte_1_bit_6

    or.b    #$02,1(a2)

.byte_1_bit_6
    btst    #6,d2
    beq     .byte_1_bit_7

    or.b    #$04,1(a2)

.byte_1_bit_7
    btst    #7,d2
    beq     .byte_2_bit_4

    or.b    #$08,1(a2)

.byte_2_bit_4
    btst    #12,d2
    beq     .byte_2_bit_5

    or.b    #$10,1(a2)

.byte_2_bit_5
    btst    #13,d2
    beq     .byte_2_bit_6

    or.b    #$20,1(a2)

.byte_2_bit_6
    btst    #14,d2
    beq     .byte_2_bit_7

    or.b    #$40,1(a2)

.byte_2_bit_7
    btst    #15,d2
    beq     .end

    or.b    #$80,1(a2)

.end
    rts

;-----------------------------------------------
TESTDecodeRowOf16Pixels:
    ;INPUT: a1 - source bytes ptr
    ;USES:  d2,a4
    ;OUTPUT: a2 - bitplane data

    move.l  0,0(a2)                                         ;leftmost column destinations
    move.l  0,4(a2)                                         ;rightmost column destinations

    move.l  a1,d2
    add.l   #test_tilesrc_bp_offset,d2                      ;test_tilesrc_bp_offset = offset to bitplanes 0 and 1 in source
    move.l  d2,a4

                                                            ;leftmost columns of 8 pixels
    move.l  #0,d2
    move.w  (a4),d2

                                                            ;d2.w = zeroAndOneByte1 & zeroAndOneByte2
                                                            ;a2 => Bitplane 01 - lower byte; Bitplane 00 - upper byte
    bsr     TESTExtract8BitplaneBytesFromTwoSourceBytes     ;returns DecodedBitplaneBytes in a2

    lea     2(a2),a2
    move.l  #0,d2
    move.w  (a1),d2                                         ;d2.w = twoAndThreeByte1 & twoAndThreeByte2


                                                            ;a2 => Bitplane 03 - lower byte; Bitplane 02 - upper byte
    bsr     TESTExtract8BitplaneBytesFromTwoSourceBytes     ;returns DecodedBitplaneBytes in a2


                                                            ;rightmost columns of 8 pixels

    lea     2(a2),a2
    move.l  #0,d2
    move.w  test_tilesrc_upr_px_b_off(a4),d2                ;add $20 to get to the src of the rightmost 8 pixel columns


                                                            ;d2.w = zeroAndOneByte1 & zeroAndOneByte2
                                                            ;a2 => Bitplane 01 - lower byte; Bitplane 00 - upper byte

    bsr     TESTExtract8BitplaneBytesFromTwoSourceBytes

    lea     2(a2),a2
    move.l  #0,d2
    move.w  test_tilesrc_upr_px_b_off(a1),d2                ;d2.w = twoAndThreeByte1 & twoAndThreeByte2


                                                            ;a2 => Bitplane 03 - lower byte; Bitplane 02 - upper byte

    bsr     TESTExtract8BitplaneBytesFromTwoSourceBytes
    lea     -6(a2),a2
    rts

;-----------------------------------------------
TESTExtractTile:
;INPUT:  a1 - source bytes ptr
;        a3 - destination ptr
;        a5 - DecodedBitplaneBytes
;        d1 - flipped = 1.b, not flipped = 0.b
;USES:   a2
;OUTPUT: a3 - destination

    move    #$0F,d0                                         ;16 rasterlines at a time; rightmost byte done too

.extract_tile:

    move.l  a5,a2                                           ;DecodedBitplaneBytes;stores intermediate decoded bitplane bytes
    bsr     TESTDecodeRowOf16Pixels                         ;This can be changed for a version where we decode differently

    ;    0  1  2  3   4  5  6  7
    ;a2: 3, 2, 1, 0 | 3, 2, 1, 0

    ;To flip the tile horizontally, we need to reverse the 4 bit color indexes,
    ;which means reversing the NYBBLES--not the bits. Reversing the bits gives
    ;us swapping of the odd bitplanes, which messes up the colors

    cmp.b   #1,d1
    bne     .no_flip

    swap    d1

    move.b  3(a2),d6
    move.b  7(a2),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,3(a2)
    move.b  d1,7(a2)

    move.b  2(a2),d6
    move.b  6(a2),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,2(a2)
    move.b  d1,6(a2)

    move.b  1(a2),d6
    move.b  5(a2),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,1(a2)
    move.b  d1,5(a2)

    move.b  (a2),d6
    move.b  4(a2),d1
    REPT 8
    roxr.b  #1,d1
    addx.b  d6,d6
    ENDR
    roxr.b  #1,d1
    move.b  d6,(a2)
    move.b  d1,4(a2)

    swap    d1

.no_flip

    ;this interleaves the bytes. Maybe we can do something that doesn't
    ;interleave the bytes

    move.b  3(a2),test_bmp_bp_bytes_per_raster_line*0(a3)           ;bitplane 0
    move.b  2(a2),test_bmp_bp_bytes_per_raster_line*1(a3)           ;bitplane 1
    move.b  1(a2),test_bmp_bp_bytes_per_raster_line*2(a3)           ;bitplane 2
    move.b  (a2),test_bmp_bp_bytes_per_raster_line*3(a3)            ;bitplane 3
    move.b  7(a2),test_bmp_bp_bytes_per_raster_line*0+1(a3)         ;bitplane 0
    move.b  6(a2),test_bmp_bp_bytes_per_raster_line*1+1(a3)         ;bitplane 1
    move.b  5(a2),test_bmp_bp_bytes_per_raster_line*2+1(a3)         ;bitplane 2
    move.b  4(a2),test_bmp_bp_bytes_per_raster_line*3+1(a3)         ;bitplane 3

    lea     test_bmp_bp_bytes_per_raster_line*tile_bitplanes(a3),a3 ;move down one rasterline ($400 = $100 * 4 bitplanes; $100 = bytes in one rasterline for one bitplane)
    lea     $02(a1),a1                                              ;source bytes per rasterline are 2 bytes apart

    dbf     d0,.extract_tile
    rts

;-----------------------------------------------
TESTCopyScreenFromMapSourceBitmap:
    move.l  a3,d3
    move.l  a4,d4
    add.l   #2,d4

    clr.l   d5
    move.b  v_map_tile_width(a0),d5
    sub.b   #(screen_columns-1),d5
    add.w   d5,d5

    move.w  #$09F0,BLTCON0(a6)                                  ;use A and D. Op: D = A
    move.w  #$0000,BLTCON1(a6)
    move.w  #$FFFF,BLTAFWM(a6)
    move.w  #$FFFF,BLTALWM(a6)
    move.w  d5,BLTAMOD(a6)                                      ;skip 107 columns (copy 21)
    move.w  #2,BLTDMOD(a6)                                      ;skip 1 column (copy 21)
    move.l  d3,BLTAPTH(a6)
    move.l  d4,BLTDPTH(a6)

    move.w  #(screen_width-tile_width)/16,BLTSIZE(a6)           ;no "h" term needed since it's 1024. Thanks ross @eab!

    WAITBLIT

    move.l  a3,d3
    move.l  a4,d4

    ;add.l   #2+screen_bytes_per_row*(screen_height+tile_height),d4;TASK (1)
    add.l   #2+screen_bytes_per_row*(screen_height+tile_height+tile_height),d4;TASK (1)

    move.w  d5,BLTAMOD(a6)                                      ;skip 107 columns (copy 21)
    move.w  #2,BLTDMOD(a6)                                      ;skip 1 column (copy 21)
    move.l  d3,BLTAPTH(a6)
    move.l  d4,BLTDPTH(a6)

    move.w  #tile_plane_lines*64+(screen_width-tile_width)/16,BLTSIZE(a6)        ;no "h" term needed since it's 1024. Thanks ross @eab!
    rts

;-----------------------------------------------
TESTPreRenderDebugString6Chars:
;THIS IS HARDCODED TO debug_string_mapx_bytes_per_row (6 chars)
; a1=text
; a2=Rendered Text Buffer

.loop:
    moveq   #0,d0
    lea     v_debug_char_lut(a0),a3
    move.b  (a1)+,d0
    beq     .exit
    sub.b   #32,d0

    move.b  (a3,d0.w),d0
    divu    #10,d0                                          ;row
    move.l  d0,d1
    clr.w   d1
    swap    d1                                              ;remainder (column)

    mulu    #debug_font_row_height,d0
    add.l   d1,d0                                           ;offset into font bitmap
    add.l   #DebugFontBitmapSource,d0

.draw:
    move.l  d0,a3                                           ;index into character

    move.b  (a3),(a2)
    move.b  debug_font_bitmap_bytes_per_row*1(a3),debug_string_mapx_bytes_per_row*1(a2)
    move.b  debug_font_bitmap_bytes_per_row*2(a3),debug_string_mapx_bytes_per_row*2(a2)
    move.b  debug_font_bitmap_bytes_per_row*3(a3),debug_string_mapx_bytes_per_row*3(a2)
    move.b  debug_font_bitmap_bytes_per_row*4(a3),debug_string_mapx_bytes_per_row*4(a2)
    move.b  debug_font_bitmap_bytes_per_row*5(a3),debug_string_mapx_bytes_per_row*5(a2)
    move.b  debug_font_bitmap_bytes_per_row*6(a3),debug_string_mapx_bytes_per_row*6(a2)
    move.b  debug_font_bitmap_bytes_per_row*7(a3),debug_string_mapx_bytes_per_row*7(a2)

    move.b  debug_font_bitmap_bpl_bytes_per_row(a3),debug_string_bpl_bytes_per_row(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*1(a3),debug_string_bpl_bytes_per_row+debug_string_mapx_bytes_per_row*1(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*2(a3),debug_string_bpl_bytes_per_row+debug_string_mapx_bytes_per_row*2(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*3(a3),debug_string_bpl_bytes_per_row+debug_string_mapx_bytes_per_row*3(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*4(a3),debug_string_bpl_bytes_per_row+debug_string_mapx_bytes_per_row*4(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*5(a3),debug_string_bpl_bytes_per_row+debug_string_mapx_bytes_per_row*5(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*6(a3),debug_string_bpl_bytes_per_row+debug_string_mapx_bytes_per_row*6(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*7(a3),debug_string_bpl_bytes_per_row+debug_string_mapx_bytes_per_row*7(a2)

    addq.w  #1,a2
    bra     .loop

.exit
    rts
;-----------------------------------------------
TESTPreRenderDebugStringToSprite:
;THIS IS HARDCODED TO 2 bytes per row (2 chars)
; a1=text
; a2=Sprite Data

    moveq   #1,d3
.loop:
    moveq   #0,d0
    lea     v_debug_char_lut(a0),a3
    move.b  (a1)+,d0
    beq     .exit
    sub.b   #32,d0

    move.b  (a3,d0.w),d0
    divu    #10,d0                                          ;row
    move.l  d0,d1
    clr.w   d1
    swap    d1                                              ;remainder (column)

    mulu    #debug_font_row_height,d0
    add.l   d1,d0                                           ;offset into font bitmap
    add.l   #DebugFontBitmapSource,d0

.draw:
    move.l  d0,a3                                           ;index into character

    move.b  (a3),(a2)
    move.b  debug_font_bitmap_bytes_per_row*1(a3),4(a2)
    move.b  debug_font_bitmap_bytes_per_row*2(a3),8(a2)
    move.b  debug_font_bitmap_bytes_per_row*3(a3),12(a2)
    move.b  debug_font_bitmap_bytes_per_row*4(a3),16(a2)
    move.b  debug_font_bitmap_bytes_per_row*5(a3),20(a2)
    move.b  debug_font_bitmap_bytes_per_row*6(a3),24(a2)
    move.b  debug_font_bitmap_bytes_per_row*7(a3),28(a2)

    move.b  debug_font_bitmap_bpl_bytes_per_row(a3),2(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*1(a3),6(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*2(a3),10(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*3(a3),14(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*4(a3),18(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*5(a3),22(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*6(a3),26(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*7(a3),30(a2)

    addq.w  #1,a2
    dbf     d3,.loop

.exit
    rts
;-----------------------------------------------
TESTRenderDebugStringToScreen:
;THIS IS HARDCODED TO screen_bytes_per_row
; a1=text
; a2=Screen Buffer Location

.loop:
    moveq   #0,d0
    lea     v_debug_char_lut(a0),a3
    move.b  (a1)+,d0
    beq     .exit
    sub.b   #32,d0

    move.b  (a3,d0.w),d0
    divu    #10,d0                                          ;row
    move.l  d0,d1
    clr.w   d1
    swap    d1                                              ;remainder (column)

    mulu    #debug_font_row_height,d0
    add.l   d1,d0                                           ;offset into font bitmap
    add.l   #DebugFontBitmapSource,d0

.draw:
    move.l  d0,a3                                           ;index into character

    move.b  (a3),(a2)
    move.b  debug_font_bitmap_bytes_per_row*1(a3),screen_bytes_per_row*1(a2)
    move.b  debug_font_bitmap_bytes_per_row*2(a3),screen_bytes_per_row*2(a2)
    move.b  debug_font_bitmap_bytes_per_row*3(a3),screen_bytes_per_row*3(a2)
    move.b  debug_font_bitmap_bytes_per_row*4(a3),screen_bytes_per_row*4(a2)
    move.b  debug_font_bitmap_bytes_per_row*5(a3),screen_bytes_per_row*5(a2)
    move.b  debug_font_bitmap_bytes_per_row*6(a3),screen_bytes_per_row*6(a2)
    move.b  debug_font_bitmap_bytes_per_row*7(a3),screen_bytes_per_row*7(a2)

    move.b  debug_font_bitmap_bpl_bytes_per_row(a3),debug_string_bpl_bytes_per_row(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*1(a3),screen_bpl_bytes_per_row+screen_bytes_per_row*1(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*2(a3),screen_bpl_bytes_per_row+screen_bytes_per_row*2(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*3(a3),screen_bpl_bytes_per_row+screen_bytes_per_row*3(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*4(a3),screen_bpl_bytes_per_row+screen_bytes_per_row*4(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*5(a3),screen_bpl_bytes_per_row+screen_bytes_per_row*5(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*6(a3),screen_bpl_bytes_per_row+screen_bytes_per_row*6(a2)
    move.b  debug_font_bitmap_bpl_bytes_per_row+debug_font_bitmap_bytes_per_row*7(a3),screen_bpl_bytes_per_row+screen_bytes_per_row*7(a2)

    addq.w  #1,a2
    bra     .loop

.exit
    rts
;-----------------------------------------------
TESTConvertByteToString
;Converts two digit hex byte to hex string representation
;INPUT: d0.b src;a1:dest

    move.b  d0,d1

    lsr.b   #4,d0
    and.l   #$F,d0

    lea     v_debug_hexchar_lut(a0),a2
    adda    d0,a2
    move.b  (a2),(a1)

    move.b  d1,d0
    and.l   #$F,d0

    lea     v_debug_hexchar_lut(a0),a2
    adda    d0,a2
    move.b  (a2),1(a1)

    rts
;-----------------------------------------------
TESTUpdateMapXMapYDebugStrings
    move.w  v_map_x_position(a0),d0
    and.w   #$FF00,d0
    lsr.w   #8,d0
    lea     v_text_buffer(a0),a1
    bsr     TESTConvertByteToString
    lea     Sprite04+4,a2
    bsr     TESTPreRenderDebugStringToSprite

    move.w  v_map_x_position(a0),d0
    and.w   #$00FF,d0
    lea     v_text_buffer(a0),a1
    bsr     TESTConvertByteToString
    lea     Sprite05+4,a2
    bsr     TESTPreRenderDebugStringToSprite

    move.w  v_map_y_position(a0),d0
    and.w   #$FF00,d0
    lsr.w   #8,d0
    lea     v_text_buffer(a0),a1
    bsr     TESTConvertByteToString
    lea     Sprite04Line02,a2
    bsr     TESTPreRenderDebugStringToSprite

    move.w  v_map_y_position(a0),d0
    and.w   #$00FF,d0
    lea     v_text_buffer(a0),a1
    bsr     TESTConvertByteToString
    lea     Sprite05Line02,a2
    bsr     TESTPreRenderDebugStringToSprite

    rts
;-----------------------------------------------
