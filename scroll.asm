ScrollGetXYPositionRight:
;INPUT: fast data (a0)
    ;returns mapx/y in d3
    ;returns x/y in d4

    ;get source ptrs

    move.w v_map_x_position(a0),d3                          ;save for mapy
    swap d3
    move.w v_map_x_position(a0),d3                          ;mapposx
    asr.w #4,d3                                             ;mapposx / BLOCKWIDTH

    move.w #screen_columns,d4                        ;22

    add.w d4,d3                                             ;mapx = mapposx / BLOCKWIDTH + BITMAPBLOCKSPERROW;
    clr.l d4
    move.w d3,d4
    swap d3
    and.w #15,d3                                            ;mapy = mapposx & (NUMSTEPS - 1);

    ;get dest ptrs

    ;TODO: IF VERTICAL SCROLLING IS HAPPENING... NEED TO CALCULATE OFFSET FROM MAP (0,0)

                                                            ;VideoX for Right Scroll is always 0
                                                            ;always blitting to left column
    clr.l d5
    move.w d4,d5
    divu #screen_columns,d5                          ;bitplane pointers in screen buffer
    swap d5
    move.w d5,d4                                            ;x
    swap d4                                                 ;y

    move.w d3,d4                                            ;Map Position Y (which will need to be fixed)
    asl.w #4,d4                                             ;y = tile_height * y

    swap d3                                                 ;mapx
    swap d4                                                 ;x

    move.l d3,d6                                            ;preserve mapx/mapy

    rts

;-----------------------------------------------
ScrollGetXYPositionLeft:
;INPUT: fast data (a0)
;returns mapx/y in d3
;returns x/y in d4

    move.w v_map_x_position(a0),d3                          ;save for mapy
    swap d3
    move.w v_map_x_position(a0),d3                          ;mapposx
    asr.w #4,d3                                             ;mapx = mapposx / BLOCKWIDTH
    subi.w #1,d3                                            ;because we have one blank column to the left

    clr.l d4
    swap d3
    and.w #15,d3                                            ;mapy = mapposx & (NUMSTEPS - 1);

    move.b v_tile_y_blit_positions(a0,d3.w),d4              ;y
    swap d4                                                 ;x

    ;TODO: IF VERTICAL SCROLLING IS HAPPENING... NEED TO CALCULATE OFFSET FROM MAP (0,0)

    move.w #(screen_columns-1)*tile_width,d4         ;VideoX for Left Scroll is always 336
                                                            ;always blitting to right column


    swap d3                                                 ;mapx
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
ScrollGetXYPositionDown:
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

;destination block in d3

    ;add.w #screen_rows+1,d3                                 ;mapy+17--row under visible screen
    ;cmp.w #screen_buffer_rows,d3
    ;blt .end

    ;sub.w #screen_buffer_rows,d3

    ;sub.w #1,d3                                             ;-1--row above visible screen
    ;bpl .end

    ;add.w #map_tile_height,d3

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
ScrollGetXYPositionUp:
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

;destination block in d3

    add.w #screen_rows+1,d3                                 ;mapy+17--row under visible screen
    cmp.w #screen_buffer_rows,d3
    blt .end

    sub.w #screen_buffer_rows,d3

    ;sub.w #1,d3                                             ;-1--row above visible screen
    ;bpl .end

    ;add.w #map_tile_height,d3

.end

    swap d3
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

   cmp.w #(test_cols_to_decode*tile_width),v_video_x_position(a0)           ;352
   bne .update

   move.w #0,v_video_x_position(a0)                                         ;reset video x to zero

.update
   move.b #0,v_scroll_previous_direction(a0)                                ;previous_direction = DIRECTION_RIGHT;
   rts

;-----------------------------------------------
ScrollDecrementXPosition:
;INPUT: FastData(a0)
   subi.w #1,v_map_x_position(a0)                                           ;mapposx--;
   move.w v_map_x_position(a0),v_video_x_position(a0)                       ;videoposx = mapposx;

   cmp.w #-1,v_video_x_position(a0)                                         ;-1
   bne .end

   move.w #(test_cols_to_decode*tile_width-1),v_video_x_position(a0)        ;reset video x to 351

.end

   bsr ScrollGetStepAndDelay
   rts

;-----------------------------------------------
ScrollIncrementYPosition:
;FastData(a0)
   addi.w #1,v_map_y_position(a0)                           ;mapposy++;
   addi.w #1,v_video_y_position(a0)                         ;videoposy = mapposy;

   cmp.w #screen_buffer_height,v_video_y_position(a0)
   bne .check_map_height

   move.w #0,v_video_y_position(a0)                         ;reset video y to 0

.check_map_height
   cmp.w #map_height,v_map_y_position(a0)
   bne .update

   move.w #0,v_map_y_position(a0)                           ;reset map y to 0

.update
   move.b #2,v_scroll_previous_direction(a0)                ;previous_direction = DIRECTION_DOWN;
   rts

;-----------------------------------------------
ScrollDecrementYPosition:                                   ;INPUT: mapx/y in d3; x/y in d4
;INPUT: FastData(a0)
   cmp.w #0,v_map_y_position(a0)
   beq .end

   subi.w #1,v_map_y_position(a0)                           ;mapposy--;
   subi.w #1,v_video_y_position(a0)                         ;videoposy = mapposy;

   cmp.w #-1,v_video_y_position(a0)                         ;-1
   bne .end

   move.w #screen_buffer_height-1,v_video_y_position(a0)    ;reset video y to 287

.end
   bsr ScrollGetStepAndDelay
   rts

;-----------------------------------------------
ScrollUpdateBitplanePointers:
;INPUT:d4=(dx=lw;dy=hw);a0=FastData;a1=Copper

   mcgeezer_special

   move.l v_scroll_screen(a0),d3
   move.l v_scroll_screen_split(a0),d6

   clr.l d5
   move.w d4,d5

   beq .check_y                                             ;no x-scroll. Check y-scroll

;do delta-x

   btst #15,d5
   beq .positive_x

   neg.w d5
   sub.l d5,d3
   sub.l d5,d3
   sub.l d5,d6
   sub.l d5,d6
   bra .update_scroll_delay

.positive_x
   add.l d5,d3
   add.l d5,d3
   add.l d5,d6
   add.l d5,d6

.update_scroll_delay

   move.w d0,c_horizontal_scroll_pos_01(a1)                 ;update copper

.check_y
   swap d4
   clr.l d5
   move.w d4,d5                                             ;dy

   beq .check_top_too_low                                   ;no y-scroll

;do delta-y
   btst #15,d5                                              ;scrolling up?
   beq .positive_y

   neg.w d5
   subi #1,d5

.loop_sub_y
   sub.l #screen_bytes_per_row,d6
   dbf.w d5,.loop_sub_y

   bra .check_top_too_low

.positive_y
   subi #1,d5

.loop_add_y
   add.l #screen_bytes_per_row,d6
   dbf.w d5,.loop_add_y

;check to see if the bitplane pointers
;are outside of the buffer area

.check_top_too_low
   move.l v_screen(a0),d7
   cmp.l d7,d3
   bge .check_top_too_high

   ;mcgeezer_special
   add.l #screen_bytes_per_row*screen_buffer_height,d3
   bra .check_split_too_low

.check_top_too_high
   add.l #screen_bytes_per_row*screen_buffer_height,d7
   cmp.l d7,d3
   blt .check_split_too_low

   ;mcgeezer_special
   sub.l #screen_bytes_per_row*screen_buffer_height,d3

.check_split_too_low
   move.l v_screen(a0),d7
   cmp.l d7,d6
   bge .check_split_too_high

   ;mcgeezer_special
   add.l #screen_bytes_per_row*screen_buffer_height,d6
   bra .update_pointer

.check_split_too_high
   add.l #screen_bytes_per_row*screen_buffer_height,d7
   cmp.l d7,d6
   blt .update_pointer

   ;mcgeezer_special
   sub.l #screen_bytes_per_row*screen_buffer_height,d6

.update_pointer

   bsr ScrollCalculateVerticalSplit

   move.l d3,v_scroll_screen(a0)
   move.l d6,v_scroll_screen_split(a0)
   move #4-1,d1



.loop
   move.w d6,4+c_bitplane_pointers_01(a1)                   ;lo word
   move.w d3,4+c_bitplane_pointers_02(a1)                   ;lo word
   swap d3
   swap d6
   move.w d6,c_bitplane_pointers_01(a1)                     ;hi word
   move.w d3,c_bitplane_pointers_02(a1)                     ;hi word
   swap d3
   swap d6
   add.l #screen_bpl_bytes_per_row,d3                       ;every 44 bytes we'll have new bitplane data
   add.l #screen_bpl_bytes_per_row,d6                       ;every 44 bytes we'll have new bitplane data
   addq #8,a1                                               ;point to next bpl to poke in copper
   dbf.w d1,.loop
   rts

;-----------------------------------------------
ScrollCalculateVerticalSplit:
;INPUTS: d3,d6
;USES: d0,d1,d2
;OUTPUTS: d6
;calculate split

   ;mcgeezer_special2
   ;mcgeezer_special

   clr.l d2

   move.w v_video_y_position(a0),d2                         ;buffer coordinates
   divu #screen_buffer_height,d2                            ;bitplane pointers in screen buffer
   swap d2

; calculate raster line of display split

   move.w #v_display_start+screen_height,d0                 ;should this be screen buffer height?

   moveq #-2*tile_height,d1                                 ;DEBUG

   moveq #0,d1
   add.w d2,d1                                              ;d1 = d1 + (ypos % screen_buffer_height)

   bmi .no_change_in_split                                  ;DEBUG





.split_changes_here

;   swap d2
;   move.w d1,d2
;   and.w #(tile_height*2)-1,d2
;
;   cmp.w #1,d2
;   bne .check_transition
;
;   sub.w #tile_height,d1
;   bra .update_split
;
;.check_transition

   cmp.w #0,d1                                              ;first split--reset split pointer
   bne .update_split

   ;if there is no split, the bitplane pointers should be reset
   ;the code gets here first for scroll down
   move.l d3,d6





   move.l v_scroll_screen(a0),v_scroll_screen_split(a0)

   add.l #tile_height*2*screen_bytes_per_row,d6
   add.l #tile_height*2*screen_bytes_per_row,v_scroll_screen_split(a0)





;   sub.w #tile_height,d1; ???                               ;is this right? -- I think this needs to be subtracted again but only the first time...




.update_split
   sub.w d1,d0                                              ;d0 = d0 - (-2*16)








.no_change_in_split:
   ;rts

; write WAIT command for split line
   move.b d0,c_split(a1)                                    ;d0 is the second one
   and.w #$ff00,d0
   sne c_split_stop(a1)                                     ;set to $ffff, if (d0 & $ff00) != 0  --if y is past 256, add second wait

   rts
;-----------------------------------------------
ScrollGetHTileOffsets:
;INPUT: mapx/y in d3
;       x/y in d4
;       x = in pixels
;       y = in "planelines" (1 realline = BLOCKSDEPTH planelines)
;       DecodedGraphic=a3;FastData=a5

    ;SOURCE => d5 (d3=offset)

    clr.l d1
    clr.l d2
    clr.l d5

    swap d3                                                 ;mapy
    swap d4                                                 ;y

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

    subi #1,d2                                              ;back one column

.left
    asl.w #1,d2                                             ;mapx=col;*2=bp byte offset

    ;FOR DEBUGGING: COMMENT THE NEXT LINE OUT; it will always choose the same source tile
    add.l d2,d1                                             ;source offset = mapy * mapwidth + mapx
    move.l d1,d3                                            ;for debugging purposes

    WAITBLIT                                                ;HardWaitBlit();

    move.l a3,d5                                            ;A source (blocksbuffer)
    add.l d1,d5                                             ;blocksbuffer + mapy + mapx

    ;DESTINATION => d1 (d4)
    ;TODO: THIS MIGHT BE MESSED UP; ADD A TILE HEIGHT
    move.l v_scroll_screen(a0),d1                           ;D dest (frontbuffer)

    clr.l d2

    btst.b #0,v_scroll_command(a0)
    beq .left2

    add.w v_video_x_bitplane_offset(a0),d2                  ;VideoXBitplaneOffset: always either one bitplane pointer down (because of shift)
                                                            ;or zero
.left2
    move.w v_map_x_position(a0),d4
    and.w #15,d4

    asl.w #1,d4
    add.w v_scrollx_dest_offset_table(a0,d4.w),d2

    move.l d2,d4                                            ;(for debugging)
    add.l d2,d1                                             ;frontbuffer + y + x
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
ScrollGetVTileOffsets:
;INPUT: mapx/mapy(offset for dest) in d3
;       y step/actual mapy(source) in d4

    ;SOURCE => d5 (d3=offset)

    clr.l d1
    clr.l d2
    clr.l d5
    move.l d3,d6                                            ;for destination

    swap d3                                                 ;mapy(offset for dest)
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

    swap d3                                                 ;mapx
    move.w d3,d2

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
    move.l v_screen(a0),d1                                  ;D dest (frontbuffer)

    swap d6

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
