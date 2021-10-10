ScrollGetXYPositionRight:
;INPUT: fast data (a0)
    ;returns mapx/y in d3
    ;returns x/y in d4

    ;get source ptrs

    move.w v_map_x_position(a0),d3                          ;save for mapy
    swap d3
    move.w v_map_x_position(a0),d3                          ;mapposx
    asr.w #4,d3                                             ;mapposx / BLOCKWIDTH

    move.w #screen_buffer_columns,d4                        ;22

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
    divu #screen_buffer_columns,d5                          ;bitplane pointers in screen buffer
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

    move.w #(screen_buffer_columns-1)*tile_width,d4         ;VideoX for Left Scroll is always 336
                                                            ;always blitting to right column


    swap d3                                                 ;mapx
    rts

;-----------------------------------------------
ScrollGetXYPositionDown:
;INPUT: map pos ptr (a3); video pos ptr (a4)
;returns mapx/y in d3
;returns x/y in d4

    rts

;-----------------------------------------------
ScrollGetXYPositionUp:
;INPUT: map pos ptr (a3); TileXBlitPositions (a5)
;INPUT: map pos ptr (a3); video pos ptr (a4)
;returns mapx/y in d3
;returns x/y in d4

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
   move.w v_map_y_position(a0),v_video_y_position(a0)       ;videoposy = mapposy;

   cmp.w #screen_height,v_video_y_position(a0)
   bne .update

   move.w #screen_height,v_video_y_position(a0)             ;reset video y to 256

.update
   move.b #2,v_scroll_previous_direction(a0)                ;previous_direction = DIRECTION_DOWN;
   rts

;-----------------------------------------------
ScrollDecrementYPosition:                                   ;INPUT: mapx/y in d3; x/y in d4
;INPUT: FastData(a0)
   subi.w #1,v_map_y_position(a0)                           ;mapposy--;
   move.w v_map_y_position(a0),v_video_y_position(a0)       ;videoposy = mapposy;

   cmp.w #-1,v_video_y_position(a0)                         ;-1
   bne .end

   move.w #0,v_video_y_position(a0)                         ;reset video y to 0

.end

   bsr ScrollGetStepAndDelay
   rts

;-----------------------------------------------
ScrollUpdateBitplanePointers:
;INPUT:d4=(dx=lw;dy=hw);a0=FastData;a1=Copper

   move.l v_scroll_screen(a0),d3
   move.l v_scroll_screen_split(a0),d6

   clr.l d5
   move.w d4,d5

   beq .check_y

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
   move.w d4,d5

   beq .update_pointer

   clr.l d2
   move.w v_map_y_position(a0),d2
   divu #screen_buffer_height,d2                           ;bitplane pointers in screen buffer
   swap d2

; calculate raster line of display split
   move.w #v_display_start,d0
   moveq #-2*16,d1
   add.w d2,d1                             ;d1 = d1 + (ypos % screen_buffer_height)
   bmi .no_split
   sub.w d1,d0                             ;d0 = d0 - (-2*16)

; write WAIT command for split line
.no_split:
   move.b d0,c_split(a0)                   ;d0 is the second one
   and.w #$ff00,d0
   sne c_split_stop(a0)                    ;set to $ffff, if (d0 & $ff00) != 0  --if y is past 256, add second wait

;   ; write updated bitplane pointers for top and split section

;   moveq   #screen_bitplanes-1,d0
;.3:
;   swap    d6
;   move.w  d6,c_bitplane_pointers_01(a0)
;   swap    d6
;   move.w  d6,4+c_bitplane_pointers_01(a0)
;   swap    d7
;   move.w  d7,c_bitplane_pointers_02(a0)
;   swap    d7
;   move.w  d7,4+c_bitplane_pointers_02(a0)
;   addq.l  #8,a0
;   add.l   #screen_bpl_bytes_per_row,d6
;   add.l   #screen_bpl_bytes_per_row,d7
;   dbf d0,.3

   btst #15,d5
   beq .positive_y

   neg.w d5
   subi #1,d5

.loop_sub_y
   sub.l #screen_bytes_per_row,d6
   dbf.w d5,.loop_sub_y
   bra .update_pointer

.positive_y
   subi #1,d5

.loop_add_y
   add.l #screen_bytes_per_row,d6
   dbf.w d5,.loop_add_y

.update_pointer
   move.l d3,v_scroll_screen(a0)
   move.l d6,v_scroll_screen_split(a0)
   move #4-1,d1

.loop
   move.w d3,4+c_bitplane_pointers_01(a1)                    ;lo word
   move.w d6,4+c_bitplane_pointers_02(a1)                   ;lo word
   swap d3
   swap d6
   move.w d3,c_bitplane_pointers_01(a1)                      ;hi word
   move.w d6,c_bitplane_pointers_02(a1)                     ;hi word
   swap d3
   swap d6
   add.l #screen_bpl_bytes_per_row,d3                       ;every 44 bytes we'll have new bitplane data
   add.l #screen_bpl_bytes_per_row,d6                       ;every 44 bytes we'll have new bitplane data
   addq #8,a1                                               ;point to next bpl to poke in copper
   dbf.w d1,.loop

.end
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

    cmp.b #0,v_scroll_command(a0)
    bne .left

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
    move.l v_scroll_screen(a0),d1                           ;D dest (frontbuffer)

    clr.l d2

    cmp.b #0,v_scroll_command(a0)
    bne .left2

    add.w v_video_x_bitplane_offset(a0),d2                  ;VideoXBitplaneOffset: always either one bitplane pointer down (because of shift)
                                                            ;or zero
.left2
    move.w v_map_x_position(a0),d4
    and.w #15,d4

    asl.w #1,d4
    add.w v_scroll_dest_offset_table(a0,d4.w),d2

    move.l d2,d4                                            ;(for debugging)
    add.l d2,d1                                             ;frontbuffer + y + x
    rts

;-----------------------------------------------
