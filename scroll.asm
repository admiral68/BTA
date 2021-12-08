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
ScrollGetMapXYForVertical:
    move.w  v_map_y_position(a0),d3                         ;save for mapy
    asr.w   #4,d3                                           ;mapy (block)

    cmp.b   #1,v_scroll_vector_y(a0)                        ;if downward scroll, continue
    bne     .do_upward

    add.w   #15,d3
    cmp.b   v_map_tile_height(a0),d3
    blt     .finish

    clr.l   d4
    move.b  v_map_tile_height(a0),d4
    sub.w   d4,d3
    bra     .finish

.do_upward
    sub.w   #1,d3
    bpl     .finish

    clr.w   d3
    move.b  v_map_tile_height(a0),d3
    sub.w  #1,d3

.finish
    swap    d3

    move.w  v_map_y_position(a0),d3                         ;mapposy
    and.w   #15,d3                                          ;y step
    rts

;-----------------------------------------------
ScrollHorizontalFixRow:
;(RIGHT) (0,3) => (16,3) ROW BLIT (FILL):                   Y-STEP BLOCK (FROM DOWN SOURCE AS A FILL (DOWN) BLOCK)
;THIS SEEMS TO SKIP AT LEAST ONE BLOCK

    move.w  v_map_x_position(a0),d3
    and.w   #15,d3
    bne     .end

    move.w  v_map_y_position(a0),d3
    and.w   #15,d3
    beq     .end

    move.w  #0,d7
    move.l  a3,d5

    clr.l   d1
    clr.l   d4
    clr.l   d2
    move.w  v_map_y_position(a0),d2
    asr.w   #4,d2
    move.w  d2,d3
    sub.w   #1,d2
    bpl     .one_more
    clr.l   d2
    move.b  v_map_tile_height(a0),d2

.one_more
    tst.w   d2
    beq     .find_dest

    sub.w   #1,d2
    move.w  v_map_bytes_per_tile_row(a0),d4

    cmp.b   #1,v_scroll_vector_x(a0)                        ;right?
    bne     .adjust_left

    add.l   v_map_bytes_per_tile_block(a0),d1
    bra     .addo

.adjust_left
    sub.w   #2,d5

.addo
    add.l   d4,d1
    dbf     d2,.addo


.find_dest
    add.l   d1,d5

    move.l  v_scroll_screen(a0),d1

    clr.l   d4

    swap    d3
    move.w  v_map_y_position(a0),d3
    and.w   #15,d3

    cmp.w   #4,d3
    bne     .check_11
    add.w   #1,d7

.check_11
    cmp.w   #11,d3
    bne     .get_offset
    add.w   #1,d7

.get_offset
    add.w   d3,d3
    move.w  v_scrolly_dest_offset_table(a0,d3.w),d2

    add.l   d2,d5
    add.l   d2,d1

    clr.l   d2
    swap    d3
    and.w   #15,d3
    add.w   d3,d3
    move.w  v_scrollx_dest_offset_table(a0,d3.w),d2
    add.l   d2,d1

    move.w  v_map_x_position(a0),d2
    asr.w   #4,d2
    add.w   d2,d2
    add.w   d2,d5

    sub.w   #2,d1
    sub.w   #2,d5

    WAITBLIT

    tst.w   d7
    bne     .double
    bsr     TileDraw
    bra     .end

.double
    bsr     TileDrawTwoHorizontal

.end
    rts
;-----------------------------------------------
ScrollVerticalFixColumn:
;(DOWN) (3,0) => (3,16) X-STEP BLOCK FROM RIGHT SOURCE (IF LAST X-DIRECTION WAS RIGHT) OR LEFT SOURCE (IF LAST X-DIRECTION WAS LEFT)
;LIKELY BUGS IN THIS
;DOWN SCROLL WORKING NICELY--EXCEPT FOR FINAL STEP ON HITTING BOTTOM OF MAP

    move.w  v_map_y_position(a0),d3
    and.w   #15,d3
    bne     .end

    move.w  v_map_x_position(a0),d3
    and.w   #15,d3
    beq     .end

    move.l  v_scroll_screen(a0),d1
    move.l  a3,d5

    move.w  v_map_x_position(a0),d3
    asr.w   #4,d3
    add.w   d3,d3

    swap    d3
    move.w  v_map_x_position(a0),d3
    and.w   #15,d3

    tst.w   d3
    beq     .end

;THIS WILL LOCATE THE BLOCKS TO THE RIGHT/LEFT OF THE SCREEN

;DOWN SCROLL:

;NORMALS ON 0,1,2,3,4,5
;REVERSE ON 6,7,8,9,A,B,C,D,E,F
;STEP = ADD THE Y BLOCK STEP TO THE X-STEP (& F)  ((X-STEP + Y BLOCK STEP) & F)
;
;GENERAL ALGORITHM: NORMALS ON 0-(STEP); REVERSE ON (STEP+1)-F
;WHERE STEP > 0 and STEP = X-STEP + Y BLOCK STEP. IF STEP = F, THEN ALL BLOCKS ARE NORMALS.
;BLIT TO THE UP FILL ROW (WHEN SCROLLING DOWN)
;WE ARE BLITTING TO THE UP-FILL ROW COLUMN

    clr.l   d7

    move.w  d3,d2
    swap    d2
    move.w  v_map_y_position(a0),d2
    asr.w   #4,d2
    sub.w   d2,d3
    and.w   #15,d3                                              ;x step - y block step

    cmp.b   #15,v_scroll_vector_y(a0)                           ;up?
    beq     .skip_going_to_right_source

    swap    d2                                                  ;DOWN SCROLL...
    cmp.w   d2,d3
    bge     .skip_going_to_right_source                         ;REVERSE BLOCK FROM LEFT

    move.w  #1,d7                                               ;FLAG: NORMAL BLOCK FROM RIGHT/DOWN SCROLL
    add.w   #screen_columns*2,d5                                ;NORMAL BLOCK FROM RIGHT

.skip_going_to_right_source
    sub.w   #2,d5

    move.w  v_map_y_position(a0),d2
    asr.w   #4,d2
    add.w   #13,d2                                              ;PUTS OUR BUFFER ON FILL ROW

    cmp.b   #15,v_scroll_vector_y(a0)                           ;up?
    bne     .skip_dest_offset
    add.w   #1,d2                                               ;PUTS OUR BUFFER ON FILL ROW

.skip_dest_offset
    move.w  d2,d6
    and.w   #15,d2
    clr.l   d4
    move.w  v_map_bytes_per_tile_row(a0),d4

.addo
    add.l   #screen_tile_bytes_per_row,d1
    dbf     d2,.addo

    clr.l   d2

    cmp.b   #15,v_scroll_vector_y(a0)                           ;up?
    bne     .addo2
    sub.w   #16,d6
    bpl     .try_add
    move.b  v_map_tile_height(a0),d2
    add.w   d2,d6

.try_add
    tst.w   d6
    beq     .skip_add_rows

.addo2
    add.l   d4,d5
    dbf     d6,.addo2

.skip_add_rows

;THIS LOCATES THE FILL COLUMN

    clr.l   d6
    move.w  #screen_bpl_bytes_per_row,d6

    cmp.b   #15,v_scroll_vector_y(a0)                           ;up?
    beq     .finish_offset
    tst.w   d7                                                  ;FLAG: NORMAL BLOCK FROM RIGHT/DOWN SCROLL?
    beq     .finish_offset                                      ;REVERSE BLOCK FROM LEFT
    add.l   d6,d1
.finish_offset
    sub.w   #2,d1

    move.w  #0,d3
    swap    d3
    add.l   d3,d5

    cmp.l   a5,d5
    blt     .check_screen
    sub.l   v_map_bytes(a0),d5

.check_screen
    move.l  v_screen(a0),d2
    add.l   #screen_buffer_bytes,d2

    cmp.l   d2,d1
    blt     .blit
    sub.l   #screen_buffer_bytes,d1

.blit
    WAITBLIT

    bsr     TileDraw

.end
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
    cmp.w   #$010c,d0                                       ;was 00ff
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

    cmp.b   #15,v_scroll_vector_y(a0)                       ;also scrolling up?
    bne     .add_tile_step_rows

    sub.w   #1,d2
    bpl     .add_tile_step_rows

    clr.w   d2
    move.b  v_map_tile_height(a0),d2
    sub.w  #1,d2

.add_tile_step_rows




    swap    d3                                              ;mapy (block)
    move.w  d3,d6
    swap    d3                                              ;x-step
    and.w   #15,d6
    sub.w   d6,d2
    and.w   #15,d2




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

;WHEN d6=d3, we're on the [U] fill row
;MAYBE THIS INFORMATION CAN BE SAVED SOMEHOW

    move.w  d3,d2                                           ;x-step
    cmp.b   #15,v_scroll_vector_y(a0)                       ;also scrolling up?
    bne     .find_buffer_row

    sub.w   #1,d2
    bpl     .find_buffer_row
    move.w  #15,d2

.find_buffer_row
    and.w   #15,d2

    asl.w   #1,d2
    move.w  v_scrollx_dest_offset_table(a0,d2.w),d2

    **************************************
    ***** ADD RIGHT SCROLL BP OFFSET *****
    **************************************

    cmp.b   #1,v_scroll_vector_x(a0)                        ;moving right?
    bne     .go_back_one_destination_column

    ;SPECIAL CASE #1: FINISHED BLITTING A COLUMN; ON [U] ROW
    ;                 WHEN WE HIT THIS CASE, JUST BLIT THE BLOCK WITHOUT
    ;                 THE OFFSET

    cmp.w   d6,d3
    bne     .add_bitplane_offset

    move.w  v_video_y_position(a0),d7
    and.w   #15,d7
    beq     .add_bitplane_offset

    sub.w   #2,d1
    sub.w   #2,d5

    ;END: SPECIAL CASE #1

.add_bitplane_offset
    add.w   v_video_x_bitplane_offset(a0),d2                ;VideoXBitplaneOffset: always either one bitplane pointer down (because of shift)
    bra     .set_destination_ptr

.go_back_one_destination_column

    ;SPECIAL CASE #2: FINISHED BLITTING A COLUMN; ON [U] ROW
    ;                 WHEN WE HIT THIS CASE, GRAB THE SOURCE FROM 16 TILES BELOW
    ; I THINK THIS BLOCK IS INCORRECT. THIS SHOULD GET DEBUGGED

    cmp.w   d6,d3
    bne     .left_test

    move.w  v_video_y_position(a0),d7
    and.w   #15,d7
    beq     .left_test

    add.l   v_map_bytes_per_tile_block(a0),d5

    ;END: SPECIAL CASE #2

.left_test
    tst.w   d3
    beq     .set_destination_ptr
    sub.l   #2,d2                                           ;last column

.set_destination_ptr
    add.l   d2,d1                                           ;frontbuffer + y + x

    *********************************
    *****    CHECK DIAGONAL     *****
    *********************************

    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?
    bne     .single

.double
;    moveq   #8,d7; TODO: THIS COULD BE AN ISSUE (MAYBE)
;    rts

.single
    moveq   #4,d7
    rts

;-----------------------------------------------
ScrollGetVTileOffsets:
;INPUT: mapx/mapy(offset for dest) in d3
;       y step/actual mapy(source) in d4

    ;SOURCE => d5 (d3=offset)

****************************************
***           SOURCE (d5)            ***
****************************************

    *********************************
    ***** SET TILE STARTING ROW *****
    *********************************

    moveq   #0,d7

.start_v_tiles
    clr.l   d1                                              ;SOURCE OFFSET
    clr.l   d2                                              ;COUNTER
    clr.l   d4                                              ;TILE ROW BYTES
    move.w  v_map_bytes_per_tile_row(a0),d4

    swap    d3                                              ;mapy (offset for dest)
    move.w  d3,d2
    cmp.w   #0,d2
    beq     .skip_add
    sub.w   #1,d2

.addo                                                       ;mapy * mapwidth
    add.l   d4,d1
    dbf     d2,.addo

.skip_add

    ***********************************
    ***** SET TILE STARTING BLOCK *****
    ***********************************

    clr.l   d2
    move.w  v_map_x_position(a0),d2
    asr.w   #4,d2
    add.w   d2,d2

    sub.l   #2,d1                                           ;takes care of column 0
    add.l   d2,d1                                           ;source offset = mapy * mapwidth + mapx

    WAITBLIT

    move.l  a3,d5                                           ;A source (blocksbuffer)
    add.l   d1,d5                                           ;blocksbuffer + mapy + mapx

****************************************
***         DESTINATION (d1)         ***
****************************************

    move.l  v_scroll_screen(a0),d1                          ;D dest (frontbuffer)

    move.w  d3,d2
    and.w   #15,d2

    tst.w   d2
    beq     .add_column_offsets
    sub.w   #1,d2

.loop_add_tile_row                                          ;videoy * screenwidth

    add.l   #screen_tile_bytes_per_row,d1
    dbf     d2,.loop_add_tile_row

.add_column_offsets

    clr.l   d2
    swap    d3                                              ;y-step

    move.w  v_video_x_position(a0),d4
    and.w   #15,d4
    beq     .continue_with_column_offset

    ;SPECIAL CASE #3: FINISHED BLITTING A ROW (AFFECTS DIAGONAL D+L)
    ;                 WHEN WE HIT THIS CASE, GRAB THE SOURCE FROM LEFT (NORMAL)
    ;THOUGHT THIS WORKED 100%

    cmp.b   #1,v_scroll_vector_y(a0)                        ;down?
    bne     .check_special_case_04

    tst.w   d3
    bne     .continue_with_column_offset

    move.w  v_video_y_position(a0),d2
    asr.w   #4,d2
    and.w   #15,d2
    move.w  d2,v_scroll_y_block_step(a0)

    clr.l   d2

    move.w  #1,d7                           ; at this point, the fill column has left (normal) blocks in it.
    bra     .continue_with_column_offset

    ;END: SPECIAL CASE #3

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

    move.w  #1,d7
    bra     .finish

    ;END: SPECIAL CASE #4












.continue_with_column_offset
    move.w  d3,d4
    asl.w   #1,d4
    add.w   v_scrolly_dest_offset_table(a0,d4.w),d2
    add.l   d2,d1                                           ;destination offset = mapy * mapwidth + mapx

    move.w  v_map_x_position(a0),d2                         ;when X is on an uneven tile boundary, compensate
    and.w   #$000f,d2                                       ;by blitting one block to the left
    beq     .skip_compensate_for_x

;HERE'S WHERE WE KNOW THAT WE'RE ON AN UNEVEN X BOUNDARY

    sub.w   #2,d1

.skip_compensate_for_x
    tst.w   d7;NEW
    bne     .check_d5;NEW

    clr.l   d2
    move.w  v_scrolly_dest_offset_table(a0,d4.w),d2
    add.l   d2,d5

.check_d5;NEW
    cmp.l   a5,d5
    blt     .figure_out_num_blocks_to_blit

    sub.l   v_map_bytes(a0),d5                              ;TODO: Maybe this isn't correct

.figure_out_num_blocks_to_blit
    tst.w   d7
    bne     .finish

.check_positions
    asr.w   #1,d4
    cmp.w   #4,d4                                           ;positions 4 & B have doubles
    beq     .double

    cmp.w   #$B,d4                                          ;odd positions > 3 (single block)
    beq     .double

    cmp.w   #$E,d4                                          ;odd positions > 3 (single block)
    bne     .single

.double
    addq    #1,d7
.single
    addq    #1,d7
.finish
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
