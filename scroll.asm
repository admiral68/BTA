ScrollGetXYPositionRight:
    ;returns mapx/y in d3
    ;returns x/y in d4

;BLOCKSWIDTH        = bitmapwidth
;BLOCKSBYTESPERROW  = screen_bytes_per_row
;BLOCKSPERROW       = tiles_per_row

    ;get source ptrs
    ;lea MapXYPosition,a3

    move.w 2(a3),d3                                         ;save for mapy
    swap d3
    move.w 2(a3),d3                                         ;mapposx
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
;INPUT: map pos ptr (a3); TileYBlitPositions (a5)
;returns mapx/y in d3
;returns x/y in d4

;BLOCKSWIDTH        = bitmapwidth
;BLOCKSBYTESPERROW  = tile_bytes_per_row
;BLOCKSPERROW       = tiles_per_row
    move.w 2(a3),d3                                         ;save for mapy
    swap d3
    move.w 2(a3),d3                                         ;mapposx
    asr.w #4,d3                                             ;mapx = mapposx / BLOCKWIDTH
    subi.w #1,d3                                            ;because we have one blank column to the left

    clr.l d4
    swap d3
    and.w #15,d3                                            ;mapy = mapposx & (NUMSTEPS - 1);

    ;lea TileYBlitPositions,a5
    move.b (a5,d3.w),d4                                     ;y
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
;USES: d1,d2,a3 (mapxy pos),a5 (scroll pos table),d0
;OUTPUT:d0(delay),d1(scroll step),d2(map x/y)
   clr.l d1
   clr.l d2

   move.w (a3),d2
   move.w (a3),d1

   and.w #$000F,d1

   swap d1
   swap d2

   move.w 2(a3),d1
   move.w 2(a3),d2

   and.w #$000F,d1
   clr.l d0

   move.b (a5,d1.w),d0

   rts

;-----------------------------------------------
ScrollIncrementXPosition:
;INPUT: MapXYPosition(a3),VideoXYPosition(a4),PreviousScrollDir(a5)

   lea MapXYPosition,a3
   lea VideoXYPosition,a4
   lea PreviousScrollDir,a5

   addi.w #1,2(a3)                                           ;mapposx++;
   move.w 2(a3),2(a4)                                         ;videoposx = mapposx;

   cmp.w #(test_cols_to_decode*tile_width),2(a4)             ;352
   bne .update

   move.w #0,2(a4)                                           ;reset video x to zero

.update
   move.b #0,(a5)                                           ;previous_direction = DIRECTION_RIGHT;
   rts

;-----------------------------------------------
ScrollDecrementXPosition:
;INPUT: MapXYPosition(a3),VideoXYPosition(a4),ScrollPositions(a5)
   subi.w #1,2(a3)                                          ;mapposx--;
   move.w 2(a3),2(a4)                                       ;videoposx = mapposx;

   cmp.w #-1,2(a4)                                          ;-1
   bne .end

   move.w #(test_cols_to_decode*tile_width-1),2(a4)         ;reset video x to 351

.end

   bsr ScrollGetStepAndDelay
   rts

;-----------------------------------------------
ScrollIncrementYPosition:
;MapXYPosition(a3),VideoXYPosition(a4),PreviousScrollDir(a5)
   addi.w #1,(a3)                                           ;mapposy++;
   move.w (a3),(a4)                                         ;videoposy = mapposy;

   cmp.w #screen_height,(a4)
   bne .update

   move.w #screen_height,(a4)                               ;reset video y to 256

.update
   move.b #2,(a5)                                           ;previous_direction = DIRECTION_DOWN;
   rts

;-----------------------------------------------
ScrollDecrementYPosition:                                   ;INPUT: mapx/y in d3; x/y in d4
;INPUT: MapXYPosition(a3),VideoXYPosition(a4),ScrollPositions(a5)
   subi.w #1,(a3)                                           ;mapposy--;
   move.w (a3),(a4)                                         ;videoposy = mapposy;

   cmp.w #-1,(a4)                                           ;-1
   bne .end

   move.w #0,(a4)                                           ;reset video y to 0

.end

   bsr ScrollGetStepAndDelay
   rts

;-----------------------------------------------
ScrollUpdateBitplanePointers:
;INPUT:d4=(dx=lw;dy=hw);d3=ScrollScreen;a1=CopHorzScrollPos;a2=CopBplP

   clr.l d5
   move.w d4,d5

   beq .check_y

   btst #15,d5
   beq .positive_x

   neg.w d5
   sub.l d5,d3
   sub.l d5,d3
   bra .update_scroll_delay

.positive_x
   add.l d5,d3
   add.l d5,d3

.update_scroll_delay

   move.w d0,2(a1)                                          ;update copper

.check_y
   swap d4
   clr.l d5
   move.w d4,d5

   beq .update_pointer

   btst #15,d5
   beq .positive_y

   neg.w d5
   subi #1,d5

.loop_sub_y
   sub.l #screen_bp_bytes_per_raster_line,d3
   dbf.w d5,.loop_sub_y
   bra .update_pointer

.positive_y
   subi #1,d5

.loop_add_y
   add.l #screen_bp_bytes_per_raster_line,d3
   dbf.w d5,.loop_add_y

.update_pointer
   move.l d3,ScrollScreen
   move #4-1,d1

.loop
   move.w d3,6(a2)                                          ;lo word
   swap d3
   move.w d3,2(a2)                                          ;hi word
   swap d3
   add.l #screen_bp_bytes_per_raster_line,d3                ;every 44 bytes we'll have new bitplane data
   addq #8,a2                                               ;point to next bpl to poke in copper
   dbf.w d1,.loop

.end
   rts

;-----------------------------------------------
ScrollUpdateSaveWordRight:
   cmp.b #1,(a5)                                            ;if (previous_direction == DIRECTION_LEFT)
   bne .rupdate_saveword

   WAITBLIT                                                 ;HardWaitBlit();
   move.w (a4),(a3)                                         ;*savewordpointer = saveword;

.rupdate_saveword


;TODO: THIS CODE CAUSES A GURU MEDITATION :---)

;   clr.l d1
;   clr.l d2
;
;   lea Screen,a2                                            ;frontbuffer
;   move.l a2,d2
;   move.w d4,d1                                             ;x
;   asr.w #3,d1                                              ;(x / 8)
;   add.l d1,d2                                              ;frontbuffer + (x / 8)
;
;   clr.l d1
;
;   swap d4                                                  ;y
;   move.w d4,d1
;   add.w #tile_plane_lines-1,d1                             ;(y + tile_plane_lines - 1)
;   mulu #screen_width/2,d1                                  ;* bitmap_bytes_per_row
;   add.l d1,d2
;   move.l d2,a3                                             ;savewordpointer = (WORD *)(frontbuffer + (y + tile_plane_lines - 1) * bitmap_bytes_per_row + (x / 8));
;   move.w (a3),(a4)                                         ;saveword = *savewordpointer;
;   swap d4                                                  ;x

    rts

;-----------------------------------------------
ScrollUpdateSaveWordLeft:                                   ;OUTPUT: mapx/y in d3; video x/y in d4
   cmp.b #0,(a5)                                            ;if (previous_direction == DIRECTION_RIGHT)
   bne .lupdate_saveword

   WAITBLIT                                                 ;HardWaitBlit();
   move.w (a4),(a3)                                         ;*savewordpointer = saveword;

.lupdate_saveword

;TODO: THIS CODE CAUSES A GURU MEDITATION :---)

;   clr.l d1
;   clr.l d2
;   lea Screen,a2
;   move.l a2,d2                                             ;frontbuffer
;   move.w d4,d1                                             ;x
;   asr.w #3,d1                                              ;(x / 8)
;   add.l d1,d2                                              ;frontbuffer + (x / 8)
;
;;TODO: CONVERT y (in d4) TO SCREEN BUFFER COORDS; RIGHT NOW IT IS IN BIG BITMAP COORDS
;   clr.l d1
;   swap d4                                                  ;y
;   move.w d1,d4
;   mulu #screen_width/2,d1                                  ;* bitmap_bytes_per_row
;   add.l d1,d2
;   move.l d2,a3                                             ;savewordpointer = (WORD *)(frontbuffer + y * bitmap_bytes_per_row + (x / 8));
;   move.w (a3),(a4)                                         ;saveword = *savewordpointer;
;   swap d4                                                  ;x

;   lea PreviousScrollDir,a5
   move.b #1,(a5)                                           ;previous_direction = DIRECTION_LEFT;

   rts

;-----------------------------------------------
