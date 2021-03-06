ScrollGetDirectionalVectors:
    clr.l   d6
    move.w  #0,v_scroll_vector_x(a0)
    move.w  v_map_y_position(a0),d5
    swap    d5
    move.w  v_map_x_position(a0),d5

    btst.b  #0,v_joystick_value(a0)                         ;right?
    beq     .check_left

    cmp.w   #test_right_scroll_extent,d5                    ;2048-352-tile_width*2; TODO: BETTER CHECK. SOMETIMES THE MAP IS HALF-WIDE
    bge     .check_up

    move.b  #1,v_scroll_vector_x(a0)
    bra     .check_up

.check_left
    btst.b #3,v_joystick_value(a0)                          ;left?
    beq     .check_up

    tst.w   v_map_x_position(a0)
    beq     .check_up

    move.b  #15,v_scroll_vector_x(a0)

.check_up
    swap    d5
    btst.b  #2,v_joystick_value(a0)                          ;up?
    beq     .check_down

    tst.w   d5
    beq     .end

    move.b  #15,v_scroll_vector_y(a0)
    rts

.check_down
    btst.b  #1,v_joystick_value(a0)                          ;down?
    beq     .end

    move.w  v_map_height(a0),d6
    sub.w   #screen_buffer_height,d6
    cmp.w   d6,d5                                           ;scroll through all pixels before changing direction
    bge     .end

    move.b  #1,v_scroll_vector_y(a0)

.end
    rts
;-----------------------------------------------
ScrollGetMapXYForHorizontal:
;INPUT: fast data (a0)
;returns mapx/y in d3

    move.w  v_map_x_position(a0),d3                         ;save for mapy
    and.w   #15,d3                                          ;mapy = mapposx & (NUMSTEPS - 1);
    swap    d3

    move.w  v_map_x_position(a0),d3                         ;mapposx
    asr.w   #4,d3                                           ;mapx = mapposx / BLOCKWIDTH

    rts

;-----------------------------------------------
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
    sub.w   #1,d4
    bpl     .save_mapy
    move.w  #16-1,d4; ;TASK (1) TEMPORARY!

    ;TODO: PUT THESE THREE LINES IN INSTEAD OF THE TEMPORARY LINE
    ;clr.w  d4 ;TASK (1)
    ;move.b v_map_tile_height(a0),d4 ;TASK (1)
    ;sub.w  #1,d4 ;TASK (1)

.save_mapy

    swap    d4                                              ;scroll step y

;destination block in d3

.end
    swap    d3                                              ;mapx (block)
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

    move.l a0,a4
    add.l #v_scroll_positions,a4
    move.b (a4,d1.w),d0

    rts

;-----------------------------------------------
ScrollIncrementXPosition:
;INPUT: FastData(a0)

    addi.w #1,v_map_x_position(a0)                                           ;mapposx++;
    move.w v_map_x_position(a0),v_video_x_position(a0)                       ;videoposx = mapposx;

    cmp.w #(screen_columns*tile_width),v_video_x_position(a0)               ;352 not quite
    bne .end

    move.w #0,v_video_x_position(a0)                                        ;reset video x to zero

.end
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

    add.l   #screen_buffer_bytes,d7
    cmp.l   d7,d6
    blt     .update_pointer
    sub.l   #screen_buffer_bytes,d6

.update_pointer

    bsr     ScrollCalculateVerticalSplit

    move.l  d3,d1                                           ;v_scroll_screen+$B40
    move.l  d3,v_scroll_screen(a0)
    move.l  d6,v_scroll_screen_split(a0)
    move    #screen_bitplanes-1,d0

.loop
    move.w  d6,4+c_bitplane_pointers_01(a1)                 ;lo word
    move.w  d1,4+c_bitplane_pointers_02(a1)                 ;lo word
    swap    d1
    swap    d6
    move.w  d6,c_bitplane_pointers_01(a1)                   ;hi word
    move.w  d1,c_bitplane_pointers_02(a1)                   ;hi word
    swap    d1
    swap    d6
    add.l   #screen_bpl_bytes_per_row,d1                    ;every 44 bytes we'll have new bitplane data
    add.l   #screen_bpl_bytes_per_row,d6                    ;every 44 bytes we'll have new bitplane data
    addq    #8,a1                                           ;point to next bpl to poke in copper
    dbf.w   d0,.loop

    movem.l (sp)+,d0/d3-d4                                  ;restore
    rts

;-----------------------------------------------
ScrollCalculateVerticalSplit:
;INPUTS: d1,d6
;USES: d0,d2,d7
;OUTPUTS: d2,d6

    move.w  #vert_display_start+screen_buffer_height,d0     ;Puts split at bottom of screen memory
                                                            ;Previously the split was at the start of the first hidden row
    clr.l   d2

    move.w  v_video_y_position(a0),d2                       ;buffer coordinates

    divu    #screen_buffer_height,d2                        ;bitplane pointers in screen buffer
    swap    d2                                              ;(ypos % screen_buffer_height)

    cmp.b   #1,v_scroll_vector_y(a0)                        ;if downward scroll, continue
    beq     .update_split

    cmp.b   #15,v_scroll_vector_y(a0)                       ;if not upward scroll, skip the offset
    bne     .end

.update_split
    sub.w   d2,d0                                           ;d0 = d0 - (ypos % screen_buffer_height)
    cmp.w   #$00ff,d0
    bhi     .check_down
    sub.w   #1,d0                                           ;compensates for vertical split glitch
.check_down
    cmp.b   #15,v_scroll_vector_y(a0)                       ;if upward scroll, add one to the wait value
    bne     .move
    add.w   #1,d0
.move
    move.b  d0,c_split(a1)                                  ;d0 is the second one
    and.w   #$ff00,d0
    sne     c_split_stop(a1)                                ;set y=255 wait if position < $2C
.end
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

    swap    d3                                              ;mapy

    move.w  d3,d2

    move.w  v_map_bytes_per_tile_row(a0),d5
    move.w  d5,d6

    move.w  d4,d7
    and.w   #15,d7                                          ;x-step

    tst.w   d7
    bne     .check_add

    sub.l   d5,d1
    bra     .find_source_column

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
    subi    #1,d2                                           ;ADDED LINE: NEW (BREAKING?) CHANGE back a column
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

    add.l   #$40000,d5 ;TASK (1) TEMPORARY! REPLACE WITH LINE BELOW
    ;add.l   v_map_bytes(a0),d5 ;TASK (1)

.check_past_end_of_source
    cmp.l   a5,d5
    blt     .destination

    sub.l   #$40000,d5 ;TASK (1) TEMPORARY! REPLACE WITH LINE BELOW
    ;sub.l   v_map_bytes(a0),d5 ;TASK (1)

****************************************
***         DESTINATION              ***
****************************************

.destination

    ;DESTINATION => d1
    move.l  v_scroll_screen(a0),d1                          ;D dest (frontbuffer)

    clr.l   d2

    and.w   #15,d4                                          ;x-step
    move.w  d4,d6

    asl.w   #1,d4
    add.w   v_scrollx_dest_offset_table(a0,d4.w),d2

    cmp.b   #1,v_scroll_vector_x(a0)                        ;moving right?
    bne     .left2

    add.w  v_video_x_bitplane_offset(a0),d2                 ;VideoXBitplaneOffset: always either one bitplane pointer down (because of shift)
                                                            ;or zero
    bra     .get_step

.left2
    tst.w   d7
    beq     .get_step
    sub.l   #2,d2                                           ;last column

.get_step

    move.l  d2,d4                                           ;(for debugging)
    add.l   d2,d1                                           ;frontbuffer + y + x

.figure_out_num_blocks_to_blit

    moveq   #0,d7

.single
    addq    #1,d7
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

    tst.w   d2                                              ;blitting into first column?
    bne     .destination

    tst.w   d4                                              ;If on the first step and blitting into column 0,
    beq     .none                                           ;skip the blit

****************** DESTINATION **************************

.destination
    ;DESTINATION => d1 (d4)
    clr.l   d3
    move.w  d4,d3                                           ;y step;keep this for blit
    move.l  v_scroll_screen(a0),d1                          ;D dest (frontbuffer)

    swap    d6                                              ;mapy(offset for dest)
    move.w  d6,d2

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
    blt     .figure_out_num_blocks_to_blit

    sub.l   v_map_bytes(a0),d5                              ;TODO: Maybe this isn't correct

.figure_out_num_blocks_to_blit
    moveq   #0,d7
    asr.w   #1,d4

****************************************
***      DOWN SCROLL CHECKS          ***
****************************************

    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?
    bne     .check_up

    tst.w   d4
    beq     .none                                           ;If R, skip [A]

****************************************
***       UP SCROLL CHECKS           ***
****************************************

.check_up
    cmp.b   #15,v_scroll_vector_y(a0)                       ;up?
    bne     .check_position

    tst.w   d4
    beq     .none                                           ;If R, skip [C]

.check_position
    cmp.w   #4,d4                                           ;positions 4 & B have doubles
    beq     .double

    cmp.w   #$B,d4                                          ;odd positions > 3 (single block)
    bne     .single

.double
    addq    #1,d7

.single
    addq    #1,d7
    asl.w   #2,d7
.none
    rts

;-----------------------------------------------
ScrollUpdateSaveWordRight:
    cmp.b #8,v_scroll_previous_x_direction(a0)              ;if (previous_direction == DIRECTION_LEFT)
    bne .update_saveword

    WAITBLIT

    move.l v_scroll_ptr_saveword(a0),a4
    cmp.l v_screen(a0),a4
    blt .update_saveword
    move.w v_scroll_saveword(a0),(a4)                       ;*savewordpointer = saveword;

.update_saveword

    ;savewordpointer = address of the last planeline that will be overblitted
    ;savewordpointer = (WORD *)(d2 + (y + BLOCKPLANELINES - 1) * BITMAPBYTESPERROW);

    clr.l d1

    move.l v_screen(a0),d2                                  ;frontbuffer
    move.w v_map_x_position(a0),d1                          ;videoposx
    and.l #$FFFFFFF0,d1                                     ;ROUND2BLOCKWIDTH(videoposx)
    add.l #screen_width,d1                                  ;x = BITMAPWIDTH + ROUND2BLOCKWIDTH(videoposx)

    asr.w #3,d1                                             ;(x / 8)
    add.l d1,d2                                             ;frontbuffer + (x / 8)

    clr.l d1

    move.w v_video_x_position(a0),d1
    add.w #1,d1
    and.w #$000F,d1                                         ;mapy = mapposx & (NUMSTEPS - 1);
    add.w d1,d1

    move.w v_scrollx_dest_offset_table(a0,d1.w),d1
    bne .subtract
    sub.l #screen_bytes_per_row,d2
    bra .add

.subtract
    sub.l #screen_bytes_per_row,d1

.add
    add.l d1,d2
    move.l d2,a4                                            ;(WORD *)(frontbuffer + (y + tile_plane_lines - 1) * bitmap_bytes_per_row + (x / 8));
    move.l a4,v_scroll_ptr_saveword(a0)                     ;savewordpointer = (WORD *)(frontbuffer + (y + tile_plane_lines - 1) * bitmap_bytes_per_row + (x / 8));
    move.w (a4),v_scroll_saveword(a0)                       ;saveword = *savewordpointer;

    rts

;-----------------------------------------------
ScrollUpdateSaveWordLeft:
    cmp.b #1,v_scroll_previous_x_direction(a0)              ;if (previous_direction == DIRECTION_RIGHT)
    bne .update_saveword

    WAITBLIT

    move.l v_scroll_ptr_saveword(a0),a4
    cmp.l v_screen(a0),a4
    blt .update_saveword
    move.w v_scroll_saveword(a0),(a4)                       ;*savewordpointer = saveword;

.update_saveword

    ;savewordpointer = address of the first planeline that will be overblitted
    ;savewordpointer = (WORD *)(frontbuffer + y * BITMAPBYTESPERROW + (x / 8));

    clr.l d1
    move.l v_screen(a0),d2                                  ;frontbuffer

    move.w v_video_x_position(a0),d1
    and.w #$000F,d1                                         ;mapy = mapposx & (NUMSTEPS - 1);
    add.w d1,d1

    move.w v_scrollx_dest_offset_table(a0,d1.w),d1
    add.l d1,d2                                             ;frontbuffer + y * Width of bitmap in bytes

    clr.l d1

    move.w v_map_x_position(a0),d1                          ;videoposx
    and.l #$FFFFFFF0,d1                                     ;x = ROUND2BLOCKWIDTH(videoposx)
    asr.w #3,d1                                             ;(x / 8)
    add.l d1,d2                                             ;(frontbuffer + y * Width of bitmap in bytes + (x / 8))

    move.l d2,a4                                            ;(WORD *)(frontbuffer + y * bitmap_bytes_per_row + (x / 8));
    move.l a4,v_scroll_ptr_saveword(a0)                     ;savewordpointer = (WORD *)(frontbuffer + y * bitmap_bytes_per_row + (x / 8));
    move.w (a4),v_scroll_saveword(a0)                       ;saveword = *savewordpointer;

    rts

;-----------------------------------------------
