*******************************************************************************
* ROUTINES
*******************************************************************************
LoadLevelMap:
;INPUTS: MapSource(a1), Map (dest)(a2), d0(rows), d1(columns)

    move.l  #0,d2
    move    d1,d4                                           ;save it

.outer_loop
    move.l  d4,d1

.inner_loop
    move.w  (a1)+,(a2)+                                     ;poke this into the map

    dbf     d1,.inner_loop
    dbf     d0,.outer_loop

    rts
;-----------------------------------------------
AssembleSourceTilesIntoMapSourceBitmap:
;SCREEN LO-RES
;W: 2048; H=768
;8 pixels/byte
;32 bytes per line/16 words per line

;clear the map

    lea     MapSourceBitmap,a0
    lea     FastData,a1

    move.l  a0,v_tile_map_dest(a1)
    move.l  a0,v_tile_map_row_dest(a1)
    move.l  #(MapSourceBitmapE-MapSourceBitmap)/4,d0

.l0:
    clr.l   (a0)+
    dbf     d0,.l0

    swap    d0
    tst.w   d0
    beq     .continue
    sub.w   #1,d0
    swap    d0
    bra     .l0

.continue
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
    btst   #11,d0
    beq    .no_flip
    move.b #1,d1                                            ;"FLIP" flag (on) in d1

.no_flip

    andi.w  #tile_index_mask,d0
    move    d0,d2
    and.w   #15,d0
    add.w   d0,d0
    andi.w  #$7F0,d2
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

    adda.w  v_map_bytes_per_tile_row(a2),a1                 ;One tile height in destination bitmap
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
    move.l  a3,d3
    move.l  a4,d4

    ;5bp
    ;204x5 = 1020 lines (first blit)
    ;68x5  =  340 lines (second blit)
    ;      = 1360 lines / 5 = 272 total raster lines

    ;4bp
    ;256x4 = 1024 lines (first blit)
    ;16x4  =   64 lines (second blit)
    ;      = 1088 lines / 4 = 272 total raster lines

    add.l   #2+screen_bytes_per_row*tile_height,d4              ;down one row,over one column into the buffer

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

    move.w  #1020*64+(screen_width-tile_width)/16,BLTSIZE(a6)   ;TODO: LUT

    WAITBLIT

    mcgeezer_special

    clr.l   d6
    move.w  v_map_source_bpl_bytes_per_row(a0),d6
    mulu    #1020,d6                                            ;TODO: LUT

    add.l   #screen_bytes_per_row*204,d4                        ;TODO: LUT; 68 lines (of 272) were unblitted
    add.l   d6,d3                                               ;TODO: LUT; 68 lines (of 272) were unblitted

    move.w  d5,BLTAMOD(a6)                                      ;skip 107 columns (copy 21)
    move.w  #2,BLTDMOD(a6)                                      ;skip 1 column (copy 21)
    move.l  d3,BLTAPTH(a6)
    move.l  d4,BLTDPTH(a6)

    move.w  #340*64+(screen_width-tile_width)/16,BLTSIZE(a6)    ;TODO: LUT

    rts
;-----------------------------------------------
