*******************************************************************************
* ROUTINES
*******************************************************************************
LoadLevelMap:
;lea Map,a2
;lea TESTMapSource,a1
    ;THIS SETS THE TILES IN MAP LAYOUT (16x16 TILES). 128 tiles horizontally
    ;then wraps to the next row of tiles

;    move.l  #0,d2
;    move.l  #0,d3
;
;    move.l  a1,a4                                           ;row
;    move.l  #test_rows_to_decode-1,d0
;
;.outer_loop
;    move.l  #test_cols_to_decode-1,d1
;    move.l  #0,d4
;
;.inner_loop
;    move.w  (a1)+,d2                                        ;load "scroll word" into d2 from a1
;    ror     #8,d2                                           ;swap bytes; source is little endian
;    move.w  d2,d3
;    and.w   #tile_index_mask,d3
;
;    btst    #15,d2
;    beq     .finish_inner_loop
;
;    or.w    #$8000,d3                                       ;set "flipped" tile index
;
;.finish_inner_loop
;    move.w  d3,(a2)+                                        ;poke this into the tile list
;
;    ;Here we need a counter, because every 16 tiles, Black Tiger wraps down
;
;    addi    #1,d4
;    move.l  d4,d6
;    divu    #16,d6                                          ;test_cols_to_decode/16 = 128
;    swap    d6
;    cmp.w   #0,d6
;    bne     .end_inner_loop
;
;    lea     $1E0(a1),a1                                     ;down to next 16x16 block of tile indexes
;
;.end_inner_loop
;    dbf     d1,.inner_loop
;
;    lea     $20(a4),a4                                      ;down one row of 16 tile indexes in src
;    movea.l a4,a1
;
;    dbf     d0,.outer_loop


    rts
;-----------------------------------------------
AssembleSourceTilesIntoMapSourceBitmap:
;SCREEN LO-RES
;W: 2048; H=768
;8 pixels/byte
;32 bytes per line/16 words per line
;
;    lea    DecodedGraphic,a0
;    lea    FastData,a1
;
;    move.l a0,v_tile_decode_row_dest(a1)
;    move.l (DecodedGraphicE-DecodedGraphic)/4,d0
;
;.l0:
;    clr.l  (a0)+
;    dbf    d0,.l0
;
;    clr.l  d0
;
;    lea    Map,a0                                    ;Starting tile
;    lea    FastData,a2
;    lea    v_tile_decode_row_dest(a2),a1
;
;    move.l (a1),v_tile_decode_dest(a2)
;    move.l #0,d5
;
;.loop_rows
;    move.l #0,d4
;
;.loop_columns
;    move.l v_tile_decode_dest(a2),a3
;
;    move.b #0,d1
;    move.l v_tile_source(a2),a1
;    move.w (a0)+,d0
;    btst   #15,d0
;    beq    .no_flip
;    move.b #1,d1
;.no_flip
;    andi.w #tile_index_mask,d0
;    asl.l  #$06,d0
;    lea    (a1,d0.l),a1
;
;    lea    FastData,a2
;    lea    v_decoded_bitplane_bytes(a2),a5                     ;stores intermediate decoded bitplane bytes
;    bsr    TESTExtractTile
;
;    lea    FastData,a2
;    lea    v_current_map_columns(a2),a4
;    add.l  #2,v_tile_decode_dest(a2)
;    addi.b #1,d4
;
;.check_loop
;    cmp.b (a4),d4
;    bne    .loop_columns
;
;    lea    FastData,a2
;    move.l v_tile_decode_row_dest(a2),a1
;    lea    v_dest_graphic_vtile_offset(a2),a3                  ;One tile height in destination bitmap
;
;    adda.l (a3),a1
;    move.l a1,v_tile_decode_dest(a2)
;    move.l a1,v_tile_decode_row_dest(a2)
;
;    lea    v_current_map_rows(a2),a4
;    addi.b #1,d5
;    cmp.b  (a4),d5
;    bne    .loop_rows

    rts
;-----------------------------------------------
CopyScreenFromMapSourceBitmap:
;    move.l  a3,d3
;    move.l  a4,d4
;    add.l   #2+screen_bytes_per_row*tile_height,d4
;
;    move.w  #$09F0,BLTCON0(a6)                                  ;use A and D. Op: D = A
;    move.w  #$0000,BLTCON1(a6)
;    move.w  #$FFFF,BLTAFWM(a6)
;    move.w  #$FFFF,BLTALWM(a6)
;    move.w  #2*(map_tile_width-(screen_columns-1)),BLTAMOD(a6)  ;skip 107 columns (copy 21)
;    move.w  #2,BLTDMOD(a6)                                      ;skip 1 column (copy 21)
;    move.l  d3,BLTAPTH(a6)
;    move.l  d4,BLTDPTH(a6)
;
;    move.w  #(screen_width-tile_width)/16,BLTSIZE(a6)           ;no "h" term needed since it's 1024. Thanks ross @eab!
;
;    WAITBLIT
;
;    move.l  a3,d3
;    move.l  a4,d4
;
;    add.l   #2+screen_bytes_per_row*(screen_height+tile_height),d4
;
;    move.w  #2*(map_tile_width-(screen_columns-1)),BLTAMOD(a6)  ;skip 107 columns (copy 21)
;    move.w  #2,BLTDMOD(a6)                                      ;skip 1 column (copy 21)
;    move.l  d3,BLTAPTH(a6)
;    move.l  d4,BLTDPTH(a6)
;
;    move.w  #tile_plane_lines*64+(screen_width-tile_width)/16,BLTSIZE(a6)        ;no "h" term needed since it's 1024. Thanks ross @eab!

    rts
;-----------------------------------------------
