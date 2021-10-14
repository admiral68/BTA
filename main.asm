   clr.w $102.w
   lea DecodedGraphic,a3

    INCDIR ""
    INCLUDE "photon/PhotonsMiniWrapper1.04!.S"
    INCLUDE "photon/Blitter-Register-List.S"

    INCLUDE "common.s"
    INCLUDE "tile.s"
    INCLUDE "test.asm"
    INCLUDE "tile.asm"
    INCLUDE "scroll.asm"


*******************************************************************************
* GAME
*******************************************************************************
Init:
    movem.l d0-a6,-(sp)

    lea Screen,a1
    lea screen_bytes_per_row*tile_height(a1),a1
    bsr.w ClearScreen

    lea Screen,a0
    move.l #screen_bpl_bytes_per_row*screen_bitplanes*(screen_buffer_height+2),d0

.clr
    move.b #0,(a0)+
    dbf d0,.clr

; some test code

    bsr TESTCode

    bsr DecodeTileGraphicToLongBitmap

    lea DecodedGraphic,a3
    lea Screen,a4
    bsr TESTCopyScreenFromDecodedLongBitmap

    lea FastData,a1
    lea Screen,a0                                           ;ptr to first bitplane of image

    move.l a0,v_screen(a1)
    move.l a0,v_scroll_screen(a1)

    lea screen_bytes_per_row*tile_height(a0),a0             ;+2 because we're scrollin' (Skip first column)

    move.l a0,v_scroll_screen_split(a1)

    lea Copper,a1                                           ;where to poke the bitplane pointer words.
    move #4-1,d0

.bpl7:
    move.l a0,d1
    swap d1
    move.w d1,c_bitplane_pointers_01(a1)                    ;hi word
    move.w d1,c_bitplane_pointers_02(a1)                    ;hi word
    swap d1
    move.w d1,4+c_bitplane_pointers_01(a1)                  ;lo word
    move.w d1,4+c_bitplane_pointers_02(a1)                  ;lo word

    addq #8,a1                                              ;point to next bpl to poke in copper
    lea screen_bp_bytes_per_raster_line(a0),a0              ;every 44 bytes we'll have new bitplane data
    dbf d0,.bpl7

;test code ends

    movem.l (sp)+,d0-a6
    rts

*******************************************************************************
* START
*******************************************************************************

StartGame:
    bsr.w Init

    lea $DFF000,a6
    move.w #$87C0,DMACON(a6)                                ;SET+BLTPRI+DMAEN+BPLEN+COPEN+BLTEN

    move.w #0,$01fc(a6)                                     ;slow fetch mode, AGA compatibility
    move.w #$200,BPLCON0(a6)
    move.w #vert_display_start<<8+h_display_start,DIWSTRT(a6)
    move.w #vert_display_start<<8+h_display_stop,DIWSTOP(a6)
    move.w #DMA_fetch_start,DDFSTRT(a6)                     ;$28 for 22 columns; $38 for 20 columns (etc)
    move.w #$d0,DDFSTOP(a6)

    move.w #screen_modulo,BPL1MOD(a6)
    move.w #screen_modulo,BPL2MOD(a6)

    move.l #Copper,COP1LCH(a6)
    move.l #VBint,$6c(a4)                                   ;set vertb interrupt vector compatibly.
    move.w #$c020,INTENA(a6)                                ;enable interrupts generally
                                                            ;and vertb specifically.

    bsr.s Main

    rts

*******************************************************************************
* MAIN
*******************************************************************************

Main:
    movem.l d0-a6,-(sp)

.WaitMouse
    btst #6,$bfe001
    bne.s .WaitMouse

    movem.l (sp)+,d0-a6
    rts

*******************************************************************************
* TEST ROUTINES
*******************************************************************************
TESTVBCode:
    lea FastData,a0
    bsr TESTScroll
    rts

;-----------------------------------------------
TESTCode:
    lea FastData,a0

    move.b #test_cols_to_decode,v_tile_columns_to_decode(a0)
    move.b #test_rows_to_decode,v_tile_rows_to_decode(a0)
    move.l #test_bmp_vtile_offset,v_dest_graphic_vtile_offset(a0)

    lea EncodedTilesSource,a1
    move.l a1,v_tile_source(a0)

    lea TilesToDecode,a2
    lea ScrollDataLev1,a1
    bsr TESTLoadLevel1Tiles
    rts

;-----------------------------------------------
TESTUpdatePaletteDuringScroll:
    movem.l d0-a6,-(sp)

    btst.b #3,v_scroll_command(a0)                          ;left?
    bne .check_left

    btst.b #0,v_scroll_command(a0)                          ;right?
    beq .continue

    cmp.w #66,v_tile_x_position(a0)                         ;palette switch column
    blo .continue

    ;mcgeezer_special
    lea Copper,a2

    move.w #$0b87,c_palette_01(a2)
    move.w #$0754,c_palette_01+4(a2)
    move.w #$0975,c_palette_01+8(a2)
    move.w #$0ca8,c_palette_01+12(a2)
    move.w #$0ed8,c_palette_01+16(a2)
    move.w #$0fff,c_palette_01+20(a2)
    move.w #$0060,c_palette_01+24(a2)
    move.w #$0090,c_palette_01+28(a2)
    move.w #$00e0,c_palette_01+32(a2)
    move.w #$0777,c_palette_01+36(a2)
    move.w #$0aaa,c_palette_01+40(a2)
    move.w #$0747,c_palette_01+44(a2)
    move.w #$0868,c_palette_01+48(a2)
    move.w #$0a8a,c_palette_01+52(a2)
    move.w #$0cac,c_palette_01+56(a2)
    move.w #$0111,c_palette_01+60(a2)

    bra .continue

.check_left

    btst.b #7,v_scroll_command(a0)                          ;0=r;1=l;d=2;u=3;rd=4;ru=5;ld=6;lu=7
    beq .continue

    cmp.w #66,v_tile_x_position(a0)                         ;palette switch column
    bhi .continue

    lea Copper,a2

    move.w #$0111,c_palette_01(a2)
    move.w #$0FF9,c_palette_01+4(a2)
    move.w #$0EC7,c_palette_01+8(a2)
    move.w #$0DA6,c_palette_01+12(a2)
    move.w #$0C85,c_palette_01+16(a2)
    move.w #$0A74,c_palette_01+20(a2)
    move.w #$0864,c_palette_01+24(a2)
    move.w #$0753,c_palette_01+28(a2)
    move.w #$0641,c_palette_01+32(a2)
    move.w #$0533,c_palette_01+36(a2)
    move.w #$0431,c_palette_01+40(a2)
    move.w #$0111,c_palette_01+44(a2)
    move.w #$0111,c_palette_01+48(a2)
    move.w #$0111,c_palette_01+52(a2)
    move.w #$0111,c_palette_01+56(a2)
    move.w #$0110,c_palette_01+60(a2)

.continue
    movem.l (sp)+,d0-a6
    rts

;-----------------------------------------------
TESTScrollRight:
;INPUT:a0(FAST DATA)
   cmp.w #0,d1
   bne .update_horz_scroll_position

   ;tile is completely scrolled through; time to move the pointers

   addi.w #1,v_tile_x_position(a0)
   cmp.w #1,v_tile_x_position(a0)                           ;If we're just starting, skip to the end
   beq .update_horz_scroll_position

   cmp.w #128,v_tile_x_position(a0)
   beq .no_update

   move.l #1,d4
   lea Copper,a1
   bsr ScrollUpdateBitplanePointers
   rts

.no_update
   move.w #127,v_tile_x_position(a0)                        ;Tile position (current level max x)

.update_horz_scroll_position

   lea Copper,a1                                  ;Copper Horizontal Scroll pos
   move.w d0,c_horizontal_scroll_pos_01(a1)       ;update copper

   rts

;-----------------------------------------------
TESTScrollLeft:
;INPUT:a0
   cmp.w #15,d1
   bne .update_horz_scroll_position

   ;tile is completely scrolled through; time to move the pointers

   subi.w #1,v_tile_x_position(a0)
   cmp.w #-1,v_tile_x_position(a0)
   beq .no_update

.update
   move.l #$0000FFFF,d4
   lea Copper,a1
   bsr ScrollUpdateBitplanePointers
   bra .update_horz_scroll_position

.no_update
   move.w #0,v_tile_x_position(a0)

.update_horz_scroll_position

   lea Copper,a1                                  ;Copper Horizontal Scroll pos
   move.w d0,c_horizontal_scroll_pos_01(a1)       ;update copper

   rts

;-----------------------------------------------
TESTScroll:

   bsr TESTUpdatePaletteDuringScroll
   bsr ScrollGetStepAndDelay

   cmp.b #16,v_scroll_command(a0)                           ;16=kill code
   bne .continue
   rts

.continue
   cmp.b #1,v_scroll_command(a0)
   beq .right

   cmp.b #8,v_scroll_command(a0)
   beq .left

   cmp.b #2,v_scroll_command(a0)
   beq .down

   cmp.b #4,v_scroll_command(a0)
   beq .up

   cmp.b #3,v_scroll_command(a0)
   beq .rightdown

   cmp.b #5,v_scroll_command(a0)
   beq .rightup

   cmp.b #10,v_scroll_command(a0)
   beq .leftdown

   cmp.b #12,v_scroll_command(a0)
   beq .leftup

   rts

.right
    ;if (mapposx >= (mapwidth * BLOCKWIDTH - SCREENWIDTH - BLOCKWIDTH)) return;
    move.b #2,d3
    cmp.w #(test_cols_to_decode*tile_width-screen_width-tile_width*2),d2              ;2048-352-tile_width*2
    blo .scroll_right
    bra .switch_direction

.scroll_right

   bsr TESTScrollRight                                      ;INPUT:d2,a0 (d1)
   bsr ScrollGetXYPositionRight

   lea DecodedGraphic,a3
   bsr ScrollGetHTileOffsets
   bsr TileDraw
   bsr ScrollIncrementXPosition                             ;INPUT: mapx/y in d3; x/y in d4
   rts

.left
    move.b #4,d3
    cmp.w #tile_width,d2
    bhi .scroll_left
    bra .switch_direction

.scroll_left

   bsr ScrollDecrementXPosition
   bsr TESTScrollLeft
   bsr ScrollGetXYPositionLeft

   lea DecodedGraphic,a3
   bsr ScrollGetHTileOffsets
   bsr TileDraw                                             ;DrawBlock(x,y,mapx,mapy);
   rts

.up
   bsr ScrollDecrementYPosition
   
   move.l #$FFFF0000,d4
   lea Copper,a1
   bsr ScrollUpdateBitplanePointers                         ;INPUT:d4=(dx=lw;dy=hw);a0=FastData;a1=Copper

   bsr ScrollGetXYPositionUp

   lea DecodedGraphic,a3
   bsr ScrollGetVTileOffsets
   beq .blit_up_single
.blit_up_double
   bsr TileDrawTwoHorizontal
   rts
.blit_up_single
   bsr TileDraw
.end_up
   cmp.w #0,v_map_y_position(a0)
   bne .end_scroll
   move.b #2,d3
   bra .switch_direction

.down
   move.l #$10000,d4
   lea Copper,a1
   bsr ScrollUpdateBitplanePointers                         ;INPUT:d4=(dx=lw;dy=hw);a0=FastData;a1=Copper

   bsr ScrollGetXYPositionDown

   lea DecodedGraphic,a3
   bsr ScrollGetVTileOffsets
   beq .blit_down_single
.blit_down_double
   bsr TileDrawTwoHorizontal
   bra .end_down
.blit_down_single
   bsr TileDraw
.end_down
   bsr ScrollIncrementYPosition                             ;INPUT: mapx/y in d3; x/y in d4
   cmp.w #screen_height+1,v_map_y_position(a0)              ;scroll through all pixels before changing direction
   bne .end_scroll
   move.b #4,d3
   bra .switch_direction

.rightup
   rts

.rightdown
   rts

.leftup
   rts

.leftdown
   rts

.switch_direction
   move.b d3,v_scroll_command(a0)
.end_scroll
   rts

*******************************************************************************
* ROUTINES
*******************************************************************************
VBCode:

    rts

;-----------------------------------------------
DecodeTileGraphicToLongBitmap:
    ;SCREEN LO-RES
    ;W: 2048; H=256
    ;8 pixels/byte
    ;32 bytes per line/16 words per line
    ;done to show how to extract Black Tiger-encoded image data
    ;32*32*4

    lea DecodedGraphic,a0
    lea FastData,a1

    move.l a0,v_tile_decode_row_dest(a1)
    move.l (DecodedGraphicE-DecodedGraphic)/4,d0

.l0:
    clr.l (a0)+
    dbf d0,.l0

    clr.l d0

    lea TilesToDecode,a0                                    ;Starting tile
    lea FastData,a2
    lea v_tile_decode_row_dest(a2),a1

    move.l (a1),v_tile_decode_dest(a2)
    move.l #0,d5

.loop_rows
    move.l #0,d4

.loop_columns
    move.l v_tile_decode_dest(a2),a3

    move.b #0,d1
    move.l v_tile_source(a2),a1
    move.w (a0)+,d0
    btst #15,d0
    beq .no_flip
    move.b #1,d1
.no_flip
    andi.w #tile_index_mask,d0
    asl.l #$06,d0
    lea (a1,d0.l),a1

    lea FastData,a2
    lea v_decoded_bitplane_bytes(a2),a5                     ;stores intermediate decoded bitplane bytes
    bsr TESTExtractTile

    lea FastData,a2
    lea v_tile_columns_to_decode(a2),a4
    add.l #2,v_tile_decode_dest(a2)
    addi.b #1,d4

.check_loop
    cmp.b (a4),d4
    bne .loop_columns

    lea FastData,a2
    move.l v_tile_decode_row_dest(a2),a1
    lea v_dest_graphic_vtile_offset(a2),a3                  ;One tile height in destination bitmap

    adda.l (a3),a1
    move.l a1,v_tile_decode_dest(a2)
    move.l a1,v_tile_decode_row_dest(a2)

    lea v_tile_rows_to_decode(a2),a4
    addi.b #1,d5
    cmp.b (a4),d5
    bne .loop_rows

    rts

;-----------------------------------------------
ClearScreen:                                                                            ;a1=screen destination address to clear
    WAITBLIT
    clr.w $66(a6)                                                                       ;destination modulo
    move.l #$01000000,$40(a6)                                                           ;set operation type in BLTCON0/1
    move.l a1,$54(a6)                                                                   ;destination address
    move.w #screen_height*bpls*64+screen_bp_bytes_per_raster_line/2,$58(a6)             ;blitter operation size
    rts
;-----------------------------------------------
VBint:                                                      ;Blank template VERTB interrupt
    movem.l d0-a6,-(sp)                                     ;Save used registers
    lea $DFF000,a6
    btst #5,INTREQR(a6)                                     ;INTREQR check if it's our vertb int.
    beq.s .notvb

    moveq #$20,d0                                           ;poll irq bit
    move.w d0,INTREQ(a6)
    move.w d0,INTREQ(a6)

    move.w #vert_display_start<<8+h_display_start,DIWSTRT(a6)
    move.w #vert_display_start<<8+h_display_stop,DIWSTOP(a6)
    move.w #DMA_fetch_start,DDFSTRT(a6)                     ;$28 for 22 columns; $38 for 20 columns (etc)
    move.w #$d0,DDFSTOP(a6)

    move.w #screen_modulo,BPL1MOD(a6)
    move.w #screen_modulo,BPL2MOD(a6)

    bsr TESTVBCode
    bsr VBCode

.notvb:
    movem.l (sp)+,d0-a6                                     ;restore
    rte
;-----------------------------------------------

    even

*******************************************************************************
* DATA (FASTMEM)
*******************************************************************************

EncodedTilesSource: INCBIN "gfx/gfx2.bin"
    EVEN

ScrollDataLev1: INCBIN "data/lev_1_scroll_data.bin"
    EVEN

TilesToDecode:
    ds.w (test_cols_to_decode+1)*(test_rows_to_decode+1)*tile_height

FastData:
;v_tile_y_position
;v_tile_x_position
    dc.l 0

;v_tile_decode_dest
    dc.l 0

;v_tile_decode_row_dest
    dc.l 0

;v_screen
    dc.l 0

;v_tile_source
    dc.l 0

;v_map_y_position
;v_map_x_position
    dc.l 0

;v_video_y_position
;v_video_x_position
    dc.l 0

;v_dest_graphic_vtile_offset
    dc.l 0

;v_scroll_screen
    dc.l 0

;v_scroll_screen_split
    dc.l 0

;v_scrollx_dest_offset_table
    dc.w $0000,$0B00,$1600,$2100,$2C00,$3700,$4200,$4D00
    dc.w $5800,$6300,$6E00,$7900,$8400,$8F00,$9A00,$A500

;v_scrolly_dest_offset_table
    dc.w $0002,$0004,$0006,$0008,$000A,$000C,$000E,$0012
    dc.w $0014,$0018,$001A,$001E,$0020,$0024,$0026,$002A

;v_scroll_saveword
    dc.w 0

;v_video_x_bitplane_offset
    dc.w screen_bpl_bytes_per_row-2

;v_scroll_positions
    dc.b $FF,$EE,$DD,$CC,$BB,$AA,$99,$88
    dc.b $77,$66,$55,$44,$33,$22,$11,$00

;v_tile_y_blit_positions
    dc.b $F0,$E0,$D0,$C0,$B0,$A0,$90,$80
    dc.b $70,$60,$50,$40,$30,$20,$10,$00

;v_decoded_bitplane_bytes
    dc.b 0,0,0,0,0,0,0,0

;v_tile_columns_to_decode
    dc.b 0

;v_tile_rows_to_decode
    dc.b 0

;v_scroll_command
    dc.b 2

;v_scroll_previous_direction
    dc.b 2



    EVEN

*******************************************************************************
* CHIPMEM
*******************************************************************************

    SECTION AllData,DATA_C

    ;move.w #$01a0,DMACON(a6)
    ;move.w #$0200,BPLCON0(a6)   ;disables it

    ;bsr    waitvertb
    ;move.w #PLANES<<12|$200,BPLCON0(a6)  ;shows it

Copper:

;c_horizontal_scroll_pos_01
    dc.w BPLCON1,$00

;c_sprites_enable_01
    dc.w BPLCON2,0                                          ;move.w #$24,BPLCON2(a6)

;c_palette_01
    tile_pal_0f

;c_display_enable_01
    dc.w BPLCON0,$4200

;c_bitplane_pointers_01
    dc.w $00e0,0                                            ;1
    dc.w $00e2,0
    dc.w $00e4,0                                            ;2
    dc.w $00e6,0
    dc.w $00e8,0                                            ;3
    dc.w $00ea,0
    dc.w $00ec,0                                            ;4
    dc.w $00ee,0
;   dc.w $00f0,0                                            ;5
;   dc.w $00f2,0
;   dc.w $00f4,0                                            ;6
;   dc.w $00f6,0

;c_split_stop
    dc.w $ffdf,$fffe

;c_split
    dc.w $2c01,$fffe

;c_bitplane_pointers_02
    dc.w $00e0,0                                            ;1
    dc.w $00e2,0
    dc.w $00e4,0                                            ;2
    dc.w $00e6,0
    dc.w $00e8,0                                            ;3
    dc.w $00ea,0
    dc.w $00ec,0                                            ;4
    dc.w $00ee,0
;   dc.w $00f0,0                                            ;5
;   dc.w $00f2,0
;   dc.w $00f4,0                                            ;6
;   dc.w $00f6,0

    dc.w $0180,$0FF9

    dc.w $ffff,$fffe

CopperE:

    EVEN

*******************************************************************************
* BUFFERS
*******************************************************************************

    SECTION AllBuffers,BSS_C

Screen:
    ds.b screen_bpl_bytes_per_row*screen_bitplanes*(screen_buffer_height+2)
ScreenE:

Screen2:
    ds.b screen_bpl_bytes_per_row*screen_bitplanes*(screen_buffer_height+2)
Screen2E:

    EVEN

DecodedGraphic:
    ds.b $80000                                             ;bitmapwidth/16*tile_bitplanes*vlines_per_graphic
    ;REMEMBER, the test bitmap is only 16 tiles high (256)
DecodedGraphicE:


