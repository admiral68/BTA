*******************************************************************************
* ROUTINES
*******************************************************************************
LoadLevelMap:
;INPUTS: MapSource(a1), Map (dest)(a2), d0(rows), d1(columns)

    rts                                                     ;TODO: REMOVE

    move.l  #0,d2
    move    d1,d4                                           ;save it

.outer_loop
    move.l  d4,d1

.inner_loop
    move.w  (a1)+,d2                                        ;load "map word" into d2 from a1
    and.w   #tile_index_mask,d2
    move.w  d2,(a2)+                                        ;poke this into the map

    dbf     d1,.inner_loop
    dbf     d0,.outer_loop

    rts
;-----------------------------------------------
AssembleSourceTilesIntoMapSourceBitmap:
;SCREEN LO-RES
;W: 2048; H=768
;8 pixels/byte
;32 bytes per line/16 words per line

    rts                                                     ;TODO: REMOVE

;clear the map

    lea     MapSourceBitmap,a0
    lea     FastData,a1

    move.l  a0,v_tile_map_dest(a1)
    move.l  a0,v_tile_map_row_dest(a1)
    move.l  (MapSourceBitmapE-MapSourceBitmap)/4,d0

.l0:
    clr.l   (a0)+
    dbf     d0,.l0

    clr.l   d0

    lea     Map,a0                                          ;Starting tile
    lea     FastData,a2

    move.l  #0,d5                                           ;row count in d5

.loop_rows

    move.l  #0,d4                                           ;column count in d4

.loop_columns

    move.l  v_tile_map_dest(a2),a3                          ;DEST in a3

    move.b #0,d1                                            ;"FLIP" flag (off) in d1
    move.l v_tile_source(a2),a1                             ;SOURCE in a1
    move.w (a0)+,d0                                         ;TILE index/attr in d0
    btst   #15,d0
    beq    .no_flip
    move.b #1,d1                                            ;"FLIP" flag (on) in d1

.no_flip

    andi.w  #tile_index_mask,d0
    move    d0,d2
    and.w   #15,d0
    asl.w   #5,d0
    mulu    #map_source_tile_bytes_per_row,d2
    add.w   d2,d0

    lea    (a1,d0.l),a1

    lea     FastData,a2
    lea     v_decoded_bitplane_bytes(a2),a5                 ;stores intermediate decoded bitplane bytes
    bsr     TileExtractFromSourceIntoMapBitmap

    lea     FastData,a2
    lea     v_current_map_columns(a2),a4
    add.l   #2,v_tile_map_dest(a2)
    addi.b  #1,d4
    cmp.b   (a4),d4
    bne     .loop_columns

    lea     FastData,a2
    move.l  v_tile_map_row_dest(a2),a1
    lea     v_dest_graphic_vtile_offset(a2),a3              ;One tile height in destination bitmap

    adda.l  (a3),a1
    move.l  a1,v_tile_map_dest(a2)
    move.l  a1,v_tile_map_row_dest(a2)

    lea     v_current_map_rows(a2),a4
    addi.b  #1,d5
    cmp.b   (a4),d5
    bne     .loop_rows

    rts
;-----------------------------------------------
CopyScreenFromMapSourceBitmap:
;5 bitplanes
    rts                                                         ;TODO: REMOVE

    move.l  a3,d3
    move.l  a4,d4
    add.l   #2+screen_bytes_per_row*tile_height,d4              ;down one row,over one column into the buffer

    move.w  #$09F0,BLTCON0(a6)                                  ;use A and D. Op: D = A
    move.w  #$0000,BLTCON1(a6)
    move.w  #$FFFF,BLTAFWM(a6)
    move.w  #$FFFF,BLTALWM(a6)
    move.w  #2*(map_tile_width-(screen_columns-1)),BLTAMOD(a6)  ;skip 107 columns (copy 21)
    move.w  #2,BLTDMOD(a6)                                      ;skip 1 column (copy 21)
    move.l  d3,BLTAPTH(a6)
    move.l  d4,BLTDPTH(a6)

    move.w  #(screen_width-tile_width)/16,BLTSIZE(a6)           ;no "h" term needed since it's 1024. Thanks ross @eab!

    WAITBLIT

    add.l   #screen_bytes_per_row*tile_height,d4                ;two rows were unblitted

    move.w  #2*(map_tile_width-(screen_columns-1)),BLTAMOD(a6)  ;skip 107 columns (copy 21)
    move.w  #2,BLTDMOD(a6)                                      ;skip 1 column (copy 21)
    move.l  d3,BLTAPTH(a6)
    move.l  d4,BLTDPTH(a6)

    move.w  #2*tile_plane_lines*64+(screen_width-tile_width)/16,BLTSIZE(a6)

    rts
;-----------------------------------------------
