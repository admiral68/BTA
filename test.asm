
*******************************************************************************
* TEST ROUTINES
*******************************************************************************
TESTLoadLevel1Tiles:
    ;THIS DECODES THE TILES IN MAP LAYOUT (16x16 TILES). 128 tiles horizontally
    ;then wrap to the next row of tiles

    move.l #0,d2
    move.l #0,d3

    ;lea TilesToDecode,a2
    ;lea ScrollDataLev1,a1
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

