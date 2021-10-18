ScrollGetXYPositionRight:
;INPUT: fast data (a0)
    ;returns mapx/y in d3

    ;get source ptrs
    move.w v_map_x_position(a0),d3
    and.w #15,d3                                            ;mapy = mapposx & (NUMSTEPS - 1);
    swap d3
    move.w v_map_x_position(a0),d3                          ;mapposx
    asr.w #4,d3                                             ;mapposx / BLOCKWIDTH

    add.w #screen_columns,d3                                ;mapx = mapposx / BLOCKWIDTH + BITMAPBLOCKSPERROW;
    rts

;-----------------------------------------------
ScrollGetXYPositionLeft:
;INPUT: fast data (a0)
;returns mapx/y in d3

    move.w v_map_x_position(a0),d4                          ;save for mapy
    sub.w #1,d4                                             ;NEW CODE
    move.w d4,d3                                            ;save for mapy
    and.w #15,d3                                            ;mapy = mapposx & (NUMSTEPS - 1);
    swap d3
    move.w d4,d3                                            ;mapposx
    asr.w #4,d3                                             ;mapx = mapposx / BLOCKWIDTH
    subi.w #1,d3                                            ;because we have one blank column to the left

    rts

;-----------------------------------------------
ScrollGetXYPositionDown:
;returns mapx/mapy (source blocks) in d3
;        scroll step/mapy (dest block) in d4

;at this point, the scroll bitplane pointer has already moved down
;and the vertical split has been calculated

    clr.l d4
    move.w v_map_y_position(a0),d3                          ;save for mapy
    move.w d3,d4                                            ;mapposy (if scrolling down, we're one behind)
    and.w #15,d4                                            ;scroll step y

    swap d3
    move.w v_map_x_position(a0),d3                          ;mapposx
    asr.w #4,d3                                             ;mapx (block)

    swap d3                                                 ;mapposy

    asr.w #4,d3                                             ;mapy (block)

    cmp.w #map_tile_height,d3                               ;This is because the
    ble .save_mapy                                          ;source bitmap is only map_tile_height blocks high

    sub.w #map_tile_height,d3                               ;special case: grab source blocks from top of test bitmap

.save_mapy

    swap d4
    move.w d3,d4                                            ;mapy (block) in d4
    swap d4                                                 ;scroll step y

;destination block in d3

.end
    swap d3                                                 ;mapx (block)
    rts

;-----------------------------------------------
ScrollGetXYPositionUp:
;returns mapx/mapy (source blocks) in d3
;        scroll step/mapy (dest block) in d4

;at this point, the scroll bitplane pointer has not moved down
;and the vertical split has not been calculated

    clr.l d4
    move.w v_map_y_position(a0),d3                          ;save for mapy
    move.w d3,d4                                            ;mapposy (if scrolling down, we're one behind)
    and.w #15,d4                                            ;scroll step y

    swap d3
    move.w v_map_x_position(a0),d3                          ;mapposx
    asr.w #4,d3                                             ;mapx (block)

    swap d3                                                 ;mapposy

    asr.w #4,d3                                             ;mapy (block)

    cmp.w #map_tile_height,d3                               ;This is because the
    ble .save_mapy                                          ;source bitmap is only map_tile_height blocks high

    sub.w #map_tile_height,d3                               ;special case: grab source blocks from top of test bitmap

.save_mapy

    swap d4
    move.w d3,d4                                            ;mapy (block) in d4
    swap d4                                                 ;scroll step y

;destination block in d3

    add.w #1,d3                                             ;mapy+1--row under visible screen

.end

    swap d3                                                 ;mapx (block)
    rts

;-----------------------------------------------
ScrollGetStepAndDelay:
;USES: d1,d2,a0 (fast data),d0
;OUTPUT:d0(delay),d1(scroll step),d2(map x/y)
    clr.l d1
    clr.l d2

    move.w v_map_y_position(a0),d2
    move.w v_map_y_position(a0),d1

    and.w #$000F,d1

    swap d1
    swap d2

    move.w v_map_x_position(a0),d1
    move.w v_map_x_position(a0),d2

    and.w #$000F,d1
    clr.l d0

    move.b v_scroll_positions(a0,d1.w),d0

    rts

;-----------------------------------------------
ScrollIncrementXPosition:
;INPUT: FastData(a0)

    addi.w #1,v_map_x_position(a0)                                           ;mapposx++;
    move.w v_map_x_position(a0),v_video_x_position(a0)                       ;videoposx = mapposx;

    cmp.w #(screen_columns*tile_width),v_video_x_position(a0)               ;352 not quite
    bne .update

    move.w #0,v_video_x_position(a0)                                        ;reset video x to zero

.update
    move.b #1,v_scroll_previous_direction(a0)                               ;previous_direction = DIRECTION_RIGHT;
    rts

;-----------------------------------------------
ScrollDecrementXPosition:
;INPUT: FastData(a0)
    subi.w #1,v_map_x_position(a0)                                           ;mapposx--;
    move.w v_map_x_position(a0),v_video_x_position(a0)                       ;videoposx = mapposx;

    cmp.w #-1,v_video_x_position(a0)                                         ;-1
    bne .end

    move.w #(screen_columns*tile_width-1),v_video_x_position(a0)             ;reset video x to 351

.end

    bsr ScrollGetStepAndDelay
    rts

;-----------------------------------------------
ScrollIncrementYPosition:
;FastData(a0)
    addi.w #1,v_map_y_position(a0)                          ;mapposy++;
    addi.w #1,v_video_y_position(a0)                        ;videoposy = mapposy;

    cmp.w #screen_buffer_height,v_video_y_position(a0)
    bne .update

    move.w #0,v_video_y_position(a0)                        ;reset video y to 0

.update
    rts

;-----------------------------------------------
ScrollDecrementYPosition:                                   ;INPUT: mapx/y in d3; x/y in d4
;INPUT: FastData(a0)
    cmp.w #0,v_map_y_position(a0)
    beq .end

    subi.w #1,v_map_y_position(a0)                          ;mapposy--;
    subi.w #1,v_video_y_position(a0)                        ;videoposy = mapposy;

    cmp.w #-1,v_video_y_position(a0)                        ;-1
    bne .end

    move.w #screen_buffer_height-1,v_video_y_position(a0)   ;reset video y to 287

.end
    rts

;-----------------------------------------------
ScrollUpdateBitplanePointers:
;INPUT:d4=(dx=lw;dy=hw);a0=FastData;a1=Copper
    movem.l d0/d3-d4,-(sp)                                  ;Save used registers

    move.l v_scroll_screen(a0),d3
    move.l v_scroll_screen_split(a0),d6

    clr.l d5
    move.w d4,d5

    beq .check_y                                            ;no x-scroll. Check y-scroll

;do delta-x

    btst #15,d5
    beq .positive_x

;TODO: SCROLL VELOCITY
    neg.w d5
    sub.l d5,d3
    sub.l d5,d3
    sub.l d5,d6
    sub.l d5,d6
    bra .check_y

;TODO: SCROLL VELOCITY
.positive_x
    add.l d5,d3
    add.l d5,d3
    add.l d5,d6
    add.l d5,d6

.check_y
    swap d4
    clr.l d5
    move.w d4,d5                                            ;dy

    beq .check_split_too_low                                ;no y-scroll

;do delta-y
    btst #15,d5                                             ;scrolling up?
    beq .positive_y

    neg.w d5
    subi #1,d5

;TODO: SCROLL VELOCITY
.loop_sub_y
    sub.l #screen_bytes_per_row,d6
    dbf.w d5,.loop_sub_y

    bra .check_split_too_low

.positive_y
    subi #1,d5

;TODO: SCROLL VELOCITY
.loop_add_y
    add.l #screen_bytes_per_row,d6
    dbf.w d5,.loop_add_y

;check to see if the bitplane pointers
;are outside of the buffer area

.check_split_too_low
    move.l v_screen(a0),d7
    cmp.l d7,d6
    bge .check_split_too_high
    add.l #screen_bytes_per_row*screen_buffer_height,d6
    bra .update_pointer

.check_split_too_high
    add.l #screen_bytes_per_row*screen_buffer_height,d7
    cmp.l d7,d6
    blt .update_pointer
    sub.l #screen_bytes_per_row*screen_buffer_height,d6

.update_pointer

    bsr ScrollCalculateVerticalSplit

    move.l #screen_bytes_per_row*tile_height,d1
    add.l d3,d1                                             ;v_scroll_screen+$B00
    move.l d3,v_scroll_screen(a0)
    move.l d6,v_scroll_screen_split(a0)
    move #4-1,d0

.loop
    move.w d6,4+c_bitplane_pointers_01(a1)                  ;lo word
    move.w d1,4+c_bitplane_pointers_02(a1)                  ;lo word
    swap d1
    swap d6
    move.w d6,c_bitplane_pointers_01(a1)                    ;hi word
    move.w d1,c_bitplane_pointers_02(a1)                    ;hi word
    swap d1
    swap d6
    add.l #screen_bpl_bytes_per_row,d1                      ;every 44 bytes we'll have new bitplane data
    add.l #screen_bpl_bytes_per_row,d6                      ;every 44 bytes we'll have new bitplane data
    addq #8,a1                                              ;point to next bpl to poke in copper
    dbf.w d0,.loop

    movem.l (sp)+,d0/d3-d4                                  ;restore
    rts

;-----------------------------------------------
ScrollCalculateVerticalSplit:
;INPUTS: d1,d6
;USES: d0,d2,d7
;OUTPUTS: d2,d6

    move.w #vert_display_start+screen_height,d0

    clr.l d2

    move.w v_video_y_position(a0),d2                        ;buffer coordinates
    divu #screen_buffer_height,d2                           ;bitplane pointers in screen buffer
    swap d2                                                 ;(ypos % screen_buffer_height)

    btst.b #1,v_scroll_command(a0)                          ;if downward scroll, continue
    bne .update_split

    btst.b #2,v_scroll_command(a0)                          ;if not upward scroll, skip the offset
    beq .end

.update_split
    sub.w d2,d0                                             ;d0 = d0 - (ypos % screen_buffer_height)
    cmp.w #$00ff,d0
    bhi .check_down
    sub.w #1,d0                                             ;compensates for vertical split glitch
.check_down
    btst.b #2,v_scroll_command(a0)                          ;if upward scroll, add one to the wait value
    beq .move
    add.w #1,d0
.move
    move.b d0,c_split(a1)                                   ;d0 is the second one
    and.w #$ff00,d0
    sne c_split_stop(a1)                                    ;set y=255 wait if position < $2C
.end
    rts
;-----------------------------------------------
ScrollGetHTileOffsets:
;INPUT: mapx/y in d3
;       x = in pixels
;       y = in "planelines" (1 realline = BLOCKSDEPTH planelines)
;       DecodedGraphic=a3;FastData=a5

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

    btst.b #0,v_scroll_command(a0)
    beq .left

    subi #1,d2                                              ;back a column

.left
    subi #1,d2                                              ;ADDED LINE: NEW (BREAKING?) CHANGE back a column
    bpl .asl
    clr.w d2
.asl
    asl.w #1,d2                                             ;mapx=col;*2=bp byte offset

    ;FOR DEBUGGING: COMMENT THE NEXT LINE OUT; it will always choose the same source tile
    add.l d2,d1                                             ;source offset = mapy * mapwidth + mapx
    move.l d1,d3                                            ;for debugging purposes

    WAITBLIT                                                ;HardWaitBlit();

    move.l a3,d5                                            ;A source (blocksbuffer)
    add.l d1,d5                                             ;blocksbuffer + mapy + mapx

    ;DESTINATION => d1
    move.l v_scroll_screen(a0),d1                           ;D dest (frontbuffer)

    clr.l d2
    move.w v_map_x_position(a0),d4

    btst.b #0,v_scroll_command(a0)
    beq .left2

    add.w v_video_x_bitplane_offset(a0),d2                  ;VideoXBitplaneOffset: always either one bitplane pointer down (because of shift)
                                                            ;or zero
    bra .get_step

.left2
    sub.w #2,d2                                             ;last column
    sub.w #1,d4                                             ;NEW CODE seems everything for left needs subtracted by 1

.get_step
    and.w #15,d4

    asl.w #1,d4
    add.w v_scrollx_dest_offset_table(a0,d4.w),d2

    move.l d2,d4                                            ;(for debugging)
    add.l d2,d1                                             ;frontbuffer + y + x
    rts

;-----------------------------------------------
ScrollGetVTileOffsets:
;INPUT: mapx/mapy(offset for dest) in d3
;       y step/actual mapy(source) in d4

    ;SOURCE => d5 (d3=offset)

    clr.l d1
    clr.l d2
    clr.l d5
    move.l d3,d6                                            ;for destination

    swap d4                                                 ;actual mapy(source)
    move.w d4,d2
    swap d4                                                 ;y step

    cmp.w #0,d2
    beq .skip_add
    sub.w #1,d2

.addo                                                       ;mapy * mapwidth
    add.l #$4000,d1
    dbf d2,.addo

.skip_add

    move.w d3,d2                                            ;mapx

    asl.w #1,d2                                             ;mapx=col;*2=bp byte offset

    ;FOR DEBUGGING: COMMENT THE NEXT LINE OUT; it will always choose the same source tile
    add.l d2,d1                                             ;source offset = mapy * mapwidth + mapx

    move.l d1,d3                                            ;for debugging purposes

    WAITBLIT                                                ;TODO: PUT BACK IN WHEN NOT DEBUGGING

    move.l a3,d5                                            ;A source (blocksbuffer)
    ;FOR DEBUGGING: COMMENT THE NEXT LINE OUT; it will always choose the same source tile
    add.l d1,d5                                             ;blocksbuffer + mapy + mapx

****************** DESTINATION **************************

    ;DESTINATION => d1 (d4)
    clr.l d3
    move.w d4,d3                                            ;y step;keep this for blit
    move.l v_scroll_screen(a0),d1                           ;D dest (frontbuffer)

    swap d6                                                 ;mapy(offset for dest)

    move.w d6,d2
    cmp.w #0,d2
    beq .add_column_offsets

************* CONVERT MAPY TO VIDEOY ********************

.convert_mapy_to_videoy
    cmp.w #screen_buffer_rows,d2
    ble .add_rows

    sub.w #screen_buffer_rows,d2
    bra .convert_mapy_to_videoy

.add_rows
    move.w d2,d6                                            ;debug
    sub.w #1,d2

.loop_add_tile_row                                          ;videoy * screenwidth
    add.l #screen_bytes_per_row*tile_height,d1
    dbf d2,.loop_add_tile_row

************** ADD COLUMN OFFSETS ***********************
.add_column_offsets
    clr.l d2
    move.w v_map_y_position(a0),d4
    swap d4
    move.w v_map_y_position(a0),d4
    and.l #$000F000F,d4

    asl.w #1,d4
    add.w v_scrolly_dest_offset_table(a0,d4.w),d2

    add.l d2,d1                                             ;destination offset = mapy * mapwidth + mapx

    clr.l d2
    move.w v_scrolly_dest_offset_table(a0,d4.w),d2
    sub.w #2,d2
    add.l d2,d5

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
