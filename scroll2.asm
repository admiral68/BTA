ScrollGetMapXYForHorizontal2:
;INPUT: fast data (a0)
;returns mapx/y in d3

    move.w  v_map_y_position(a0),d3
    asr.w   #4,d3
    swap    d3

    move.w  v_map_x_position(a0),d3                         ;save for mapy
    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?
    bne     .and
    add.w   #1,d3                                           ;if also scrolling down, add one to the x-step
.and
    and.w   #15,d3                                          ;mapy = mapposx & (NUMSTEPS - 1);

    rts

;-----------------------------------------------
ScrollGetMapXYForVertical2:
    move.w  v_map_y_position(a0),d3                         ;save for mapy
    asr.w   #4,d3                                           ;mapy (block)
    swap    d3

    move.w  v_map_x_position(a0),d3                         ;mapposx
    asr.w   #4,d3                                           ;mapx (block)
    rts

;-----------------------------------------------
ScrollGetHTileOffsets2:
;INPUT: mapx/y in d3
;       x = in pixels
;       y = in "planelines" (1 realline = BLOCKSDEPTH planelines)
;       MapSourceBitmap=a3;MapSourceBitmapE=a5
;SOURCE => d5 (d3=offset)

****************************************
***           SOURCE                 ***
****************************************

    *********************************
    ***** SET TILE STARTING ROW *****
    *********************************

    clr.l   d1                                              ;SOURCE OFFSET
    clr.l   d2                                              ;COUNTER
    clr.l   d4                                              ;TILE ROW BYTES
    move.w  v_map_bytes_per_tile_row(a0),d4

    swap    d3                                              ;mapy (block)
    move.w  d3,d2                                           ;mapy (block) => COUNTER
    tst.w   d2                                              ;if zero, no need to add any to base offset
    beq     .skip_add_base

    sub.w   #1,d2

.add_base
    add.l   d4,d1                                           ;SOURCE OFFSET += TILE ROW BYTES
    dbf     d2,.add_base

.skip_add_base

    *********************************
    *****  ADD TILE STEP ROWS   *****
    *********************************

    swap    d3                                              ;x-step
    move.w  d3,d2
    tst.w   d2
    beq     .find_source_column

    sub.w   #1,d2

.addo                                                       ;mapy * mapwidth
    add.l   d4,d1
    dbf     d2,.addo

    *********************************
    *****   SET SOURCE COLUMN   *****
    *********************************

.find_source_column

    move.w  v_map_x_position(a0),d2
    asr.w   #4,d2
    cmp.b   #1,v_scroll_vector_x(a0)
    bne     .source_left

    add.w   #screen_columns,d2                              ;add right column offset

.source_left
    sub.w   #1,d2                                           ;back a column
    bpl     .asl
    clr.w   d2

.asl
    asl.w   #1,d2                                           ;mapx=col;*2=bp byte offset

    *********************************
    *****    SET SOURCE PTR     *****
    *********************************

    add.l   d2,d1                                           ;source offset = mapy * mapwidth + mapx

    WAITBLIT                                                ;HardWaitBlit();

    move.l  a3,d5                                           ;A source (blocksbuffer)
    add.l   d1,d5                                           ;blocksbuffer + mapy + mapx

    sub.l   d4,d5                                           ;We're starting one source row higher

    cmp.l   a3,d5
    bge     .check_past_end_of_source

    add.l   d4,d5
    add.l   v_map_bytes(a0),d5

.check_past_end_of_source
    cmp.l   a5,d5
    blt     .destination
    sub.l   v_map_bytes(a0),d5

****************************************
***         DESTINATION              ***
****************************************

.destination
    move.l  d2,d4
    clr.l   d2
    move.l  v_scroll_screen(a0),d1                          ;D dest (frontbuffer)

    *********************************
    *****    FIND BUFFER ROW    *****
    *********************************

    mcgeezer_special2
    move.w  d3,d2                                           ;x-step
    swap    d3                                              ;mapy (block)
    add.w   d3,d2                                           ;rolls buffer down d3 rows
    swap    d3                                              ;x-step
    and.w   #15,d2

    asl.w   #1,d2
    move.w  v_scrollx_dest_offset_table(a0,d2.w),d2

    **************************************
    ***** ADD RIGHT SCROLL BP OFFSET *****
    **************************************

    cmp.b   #1,v_scroll_vector_x(a0)                        ;moving right?
    bne     .go_back_one_destination_column

    add.w   v_video_x_bitplane_offset(a0),d2                ;VideoXBitplaneOffset: always either one bitplane pointer down (because of shift)
    bra     .set_destination_ptr

.go_back_one_destination_column
    sub.l   #2,d2                                           ;last column

.set_destination_ptr
    add.l   d2,d1                                           ;frontbuffer + y + x

    *********************************
    *****      CHECK SKIPS      *****
    *********************************

    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?
    bne     .check_up

    tst.w   d3                                              ;SKIP FIRST BLIT
    bne     .single

.none
    moveq   #0,d7
    rts

.check_up
    cmp.b   #15,v_scroll_vector_y(a0)                       ;up?
    bne     .single

    cmp.b   #15,d3                                          ;SKIP LAST BLIT
    beq     .none

.single
    moveq   #1,d7
    asl.w   #2,d7
    rts

;-----------------------------------------------
ScrollGetVTileOffsets2:
;INPUT: mapx/mapy(offset for dest) in d3
;       y step/actual mapy(source) in d4

    ;SOURCE => d5 (d3=offset)

****************************************
***           SOURCE (d5)            ***
****************************************

****************************************
***         DESTINATION (d1)         ***
****************************************

.double
    addq    #1,d7

.single
    addq    #1,d7
    asl.w   #2,d7
.none
    rts

;-----------------------------------------------
