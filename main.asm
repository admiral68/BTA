    INCDIR ""
    INCLUDE "photon/PhotonsMiniWrapper1.04!.S"
    INCLUDE "photon/Blitter-Register-List.S"

    INCLUDE "common.s"
    INCLUDE "input.s"
    INCLUDE "tile.s"
    INCLUDE "test.asm"
    INCLUDE "tile.asm"
    INCLUDE "map.asm"
    INCLUDE "scroll.asm"
    INCLUDE "input.asm"
    INCLUDE "debug.asm"

*******************************************************************************
* GAME
*******************************************************************************
Init:
    movem.l d0-a6,-(sp)

    lea     Screen,a1
    lea     screen_bytes_per_row*tile_height(a1),a1
    bsr.w   ClearScreen

    lea     Screen,a0
    move.l  #screen_bpl_bytes_per_row*screen_bitplanes*(screen_buffer_height+2),d0

.clr
    move.b  #0,(a0)+
    dbf     d0,.clr

    lea     FastData,a0

    move.b  #level_01_main_map_rows,v_map_tile_height(a0)
    move.b  #level_01_main_map_cols,v_map_tile_width(a0)
    move.w  #level_01_main_map_cols*16,v_map_width(a0)
    move.w  #level_01_main_map_rows*16,v_map_height(a0)
    move.w  #eight_by_four_map_bytes_per_tile_row,v_map_bytes_per_tile_row(a0)
    move.w  #eight_by_four_map_bpl_bytes_per_row,v_map_source_bpl_bytes_per_row(a0)
    move.w  #eight_by_four_map_bytes_per_row,v_map_source_bytes_per_row(a0)
    move.b  #level_01_main_map_cols,v_current_map_columns(a0)
    move.b  #level_01_main_map_rows,v_current_map_rows(a0)
    move.l  #level_01_main_map_cols*2*map_bitplanes*tile_height*level_01_main_map_rows,v_map_bytes(a0)                        =

;level_01_dungeon_map_cols           = 64
;level_01_dungeon_map_rows           = 24

    lea     TilesSourceLevel01,a1
    move.l  a1,v_tile_source(a0)

    lea     Map,a2
    lea     MapSourceLevel01,a1
    move.l  #level_01_main_map_rows-1,d0
    move.l  #level_01_main_map_cols-1,d1

    bsr     LoadLevelMap
    bsr     AssembleSourceTilesIntoMapSourceBitmap


    lea     FastData,a0
    lea     MapSourceBitmap,a3
    lea     Screen,a4
    bsr     CopyScreenFromMapSourceBitmap

;SetScreenBufferPointersInFastMem


    lea     FastData,a1
    lea     Screen,a0                                       ;ptr to first bitplane of image

    move.l  a0,v_screen(a1)
    move.l  a0,v_scroll_screen(a1)

    move.l  a0,d2

    move.l  a0,v_scroll_screen_split(a1)

    move.l  d2,d3
    add.l   #screen_buffer_bytes,d3
    move.l  d3,v_screen_end(a1)

;SetCopperScreenBitplanePointers

    lea     Copper,a1                                       ;where to poke the bitplane pointer words.
    move    #screen_bitplanes-1,d0

.bpl7:
    move.l  a0,d1
    swap    d1
    swap    d2
    move.w  d1,c_bitplane_pointers_01(a1)                   ;hi word
    move.w  d2,c_bitplane_pointers_02(a1)                   ;hi word
    swap    d1
    swap    d2
    move.w  d1,4+c_bitplane_pointers_01(a1)                 ;lo word
    move.w  d2,4+c_bitplane_pointers_02(a1)                 ;lo word

    addq    #8,a1                                           ;point to next bpl to poke in copper
    lea     screen_bp_bytes_per_raster_line(a0),a0          ;every 44 bytes we'll have new bitplane data
    add.l   #screen_bp_bytes_per_raster_line,d2
    dbf     d0,.bpl7

    bsr     SetupDebugStrings
    bsr     InputInitControllers

    movem.l (sp)+,d0-a6
    rts

*******************************************************************************
* START
*******************************************************************************

StartGame:
    bsr.w   Init

    lea     $DFF000,a6
    move.w  #$7FFF,DMACON(a6)                               ;disable all bits in DMACON
    move.w  #$87E0,DMACON(a6)                               ;SET+BLTPRI+DMAEN+BPLEN+COPEN+BLTEN+SPREN

    move.w  #0,$01fc(a6)                                    ;slow fetch mode, AGA compatibility
    move.w  #$24,BPLCON2(a6)
    move.w  #$200,BPLCON0(a6)
    move.w  #vert_display_start<<8+h_display_start,DIWSTRT(a6)
    move.w  #vert_display_stop<<8+h_display_stop,DIWSTOP(a6)
    move.w  #DMA_fetch_start,DDFSTRT(a6)                    ;$28 for 22 columns; $38 for 20 columns (etc)
    move.w  #DMA_fetch_stop,DDFSTOP(a6)                     ;$d0 for 22 columns; $c0 for 20 columns

    move.w  #screen_modulo,BPL1MOD(a6)
    move.w  #screen_modulo,BPL2MOD(a6)

    move.l  #Copper,COP1LCH(a6)
    move.l  #VBint,$6c(a4)                                  ;set vertb interrupt vector compatibly.
    move.w  #$c020,INTENA(a6)                               ;enable interrupts generally
                                                            ;and vertb specifically.

    bsr.s   Main

    rts

*******************************************************************************
* MAIN
*******************************************************************************

Main:
    movem.l d0-a6,-(sp)

.WaitMouse

    moveq #1,d0
    lea FastData,a0
    bsr InputReadController

    btst.b #4,v_joystick_value(a0)                          ;16=kill code (fire button)
    bne .quit

    btst #6,$bfe001
    bne.s .WaitMouse

.quit
    movem.l (sp)+,d0-a6
    rts

*******************************************************************************
* TEST ROUTINES
*******************************************************************************
TESTVBCode:
    lea FastData,a0
    bsr TESTScroll
    bsr TESTUpdateMapXMapYDebugStrings
    rts

;-----------------------------------------------
TESTScroll:

  *-------------*
  *  ALGORITHM  *
  *-------------*

  *****************************************************
  ** BLIT ORDER: R->D->U->L (BLIT IN ASCENDING MODE) **
  *****************************************************

.continue
    bsr ScrollGetStepAndDelay

    btst.b #0,v_joystick_value(a0)                          ;right?
    beq .check_down

    bsr .right

.check_down
    btst.b #1,v_joystick_value(a0)                          ;down?
    beq .check_up
    bsr .down
    bra .check_left

.check_up
    btst.b #2,v_joystick_value(a0)                          ;up?
    beq .check_left
    bsr .up

.check_left
    btst.b #3,v_joystick_value(a0)                          ;left?
    beq .update_joystick

    bsr .left

.update_joystick
    move.b v_joystick_value(a0),v_previous_joystick_value(a0)
    rts

***********************************************
*** R: All blocks fill column (left column) ***
***                                         ***
***********************************************

.right
    cmp.w #test_right_scroll_extent,d2                      ;2048-352-tile_width*2; TODO: BETTER CHECK. SOMETIMES THE MAP IS HALF-WIDE
    bge .end_scroll

.scroll_right
    cmp.w #0,d1                                             ;INPUT:d2,a0 (d1)
    bne .get_xy_position_right

    ;tile is completely scrolled through; time to move the pointers
    addi.w #1,v_tile_x_position(a0)

.increment_x
    move.l #1,d4
    lea Copper,a1
    bsr ScrollUpdateBitplanePointers

.get_xy_position_right
    bsr ScrollGetMapXYForHorizontal
    add.w #screen_columns+1,d3                              ;mapx = mapposx / BLOCKWIDTH + BITMAPBLOCKSPERROW;

    bsr ScrollUpdateSaveWordRight

    lea MapSourceBitmap,a3
    lea MapSourceBitmapE,a5
    bsr ScrollGetHTileOffsets

    lea TileDrawVerticalJumpTable,a4
    move.l (a4,d7.w),a4
    jsr (a4)

    bsr ScrollIncrementXPosition                            ;INPUT: mapx/y in d3; x/y in d4
    bsr ScrollGetStepAndDelay
    lea Copper,a1                                           ;Copper Horizontal Scroll pos
    move.w d0,c_horizontal_scroll_pos_01(a1)                ;update copper
    move.b #1,v_scroll_previous_x_direction(a0)             ;previous_direction = DIRECTION_RIGHT
    rts

************************************************
*** L: All blocks fill column (right column) ***
***    If D, skip [B]. If U, skip [D]        ***
************************************************

.left
    tst.w v_map_x_position(a0)
    beq .end_scroll

    bsr ScrollDecrementXPosition
    bsr ScrollGetStepAndDelay

    cmp.w #0,d1
    bne .get_map_xy_left

    ;tile is completely scrolled through; time to move the pointers

    subi.w #1,v_tile_x_position(a0)

.decrement_x
    move.l #$0000FFFF,d4
    lea Copper,a1
    bsr ScrollUpdateBitplanePointers

.get_map_xy_left
    bsr ScrollGetMapXYForHorizontal
    bsr ScrollUpdateSaveWordLeft

    lea MapSourceBitmap,a3
    lea MapSourceBitmapE,a5
    bsr ScrollGetHTileOffsets

    lea TileDrawVerticalJumpTable,a4
    move.l (a4,d7.w),a4
    jsr (a4)

    ;needed for change of direction
    bsr ScrollGetStepAndDelay

.update_left_scroll_pos

    lea Copper,a1                                           ;Copper Horizontal Scroll pos
    move.w d0,c_horizontal_scroll_pos_01(a1)                ;update copper
    move.b #8,v_scroll_previous_x_direction(a0)             ;previous_direction = DIRECTION_LEFT
    rts

*******************************************
*** U: All blocks fill row (bottom row) ***
***    If R, skip [C]                   ***
*******************************************

.up
    cmp.w #0,v_map_y_position(a0)
    ble .end_scroll

    bsr ScrollDecrementYPosition

    move.l #$FFFF0000,d4
    lea Copper,a1
    bsr ScrollUpdateBitplanePointers                        ;INPUT:d4=(dx=lw;dy=hw);a0=FastData;a1=Copper

.get_xy_position_up
    bsr ScrollGetMapXYForVertical
    swap d3
    add.w #1,d3                                             ;mapy+1--row under visible screen
    swap d3

    lea MapSourceBitmap,a3
    lea MapSourceBitmapE,a5
    bsr ScrollGetVTileOffsets

    ;mcgeezer_special

    lea TileDrawHorizontalJumpTable,a4
    move.l (a4,d7.w),a4
    jsr (a4)
    rts

****************************************
*** D: All blocks fill row (top row) ***
***    If R, skip [A]                ***
****************************************

.down
    move.w  v_map_y_position(a0),d4
    clr.l   d5
    move.w  v_map_height(a0),d5
    sub.w   #screen_buffer_height,d5
    cmp.w   d5,d4                                           ;scroll through all pixels before changing direction
    bge     .end_scroll

    move.l #$10000,d4
    lea Copper,a1
    bsr ScrollUpdateBitplanePointers                        ;INPUT:d4=(dx=lw;dy=hw);a0=FastData;a1=Copper

.get_xy_position_down
    bsr ScrollGetMapXYForVertical

    lea MapSourceBitmap,a3
    lea MapSourceBitmapE,a5
    bsr ScrollGetVTileOffsets

    mcgeezer_special

    lea TileDrawHorizontalJumpTable,a4
    move.l (a4,d7.w),a4
    jsr (a4)

.end_down
    bsr ScrollIncrementYPosition                            ;INPUT: mapx/y in d3; x/y in d4

.end_scroll
    rts

*******************************************************************************
* ROUTINES
*******************************************************************************
VBCode:

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

TilesSourceLevel01:             INCBIN "gfx/Level01/tiles/all_tiles_256x336x5"
TilesLevel1SourceE:
    EVEN

MapSourceLevel01:               INCBIN "gfx/Level01/map/main_128x48"
MapSourceLevel1E:
    EVEN

MapTestSource:
    dc.w $029,$02a,$02b,$02c,$02d,$000,$029,$02a,$02b,$02c,$02d,$000
MapTestSourceE:
    EVEN

MapSourceDungeonLevel1:         INCBIN "gfx/Level01/map/dungeon_64x24"
MapSourceDungeonLevel1E:
    EVEN

MapSourceWiseManLevel1:         INCBIN "gfx/Level01/map/wiseman_16x7"
MapSourceWiseManLevel1E:
    EVEN

DebugFontBitmapSource:          INCBIN "gfx/debug_alpha80x32x2.raw"
DebugFontBitmapSourceE:
    EVEN

Map:    ;TODO: Dynamic alloc?
    ds.w (level_01_main_map_cols+1)*(level_01_main_map_rows+1)*tile_height

FastData:
;v_tile_y_position
;v_tile_x_position
    dc.l 0

;v_tile_map_dest
    dc.l 0

;v_tile_map_row_dest
    dc.l 0

;v_screen
    dc.l 0

;v_tile_source
    dc.l 0

;v_map_y_position
;v_map_x_position
    dc.l $00000000

;v_video_y_position
;v_video_x_position
    dc.l $00000000

;v_map_source_bytes_per_row
    dc.w 0
    dc.w 0

;v_scroll_screen
    dc.l 0

;v_scroll_screen_split
    dc.l 0

;v_scroll_ptr_saveword
    dc.l 0

;v_map_width
    dc.w 0

;v_map_bytes_per_tile_row
    dc.w 0

;v_map_bytes
    dc.l 0

;v_map_height
    dc.w 0

;v_map_source_bpl_bytes_per_row
    dc.w 0

;v_scrollx_dest_offset_table
    dc.w $0000,$0B40,$1680,$21C0,$2D00,$3840,$4380,$4EC0
    dc.w $5A00,$6540,$7080,$7BC0,$8700,$9240,$9D80,$A8C0

;v_scrolly_dest_offset_table
    dc.w $0000,$0002,$0004,$0006,$0008,$000C,$000E,$0010
    dc.w $0012,$0014,$0016,$0018,$001C,$001E,$0020,$0022

;v_scroll_saveword
    dc.w 0

;v_video_x_bitplane_offset
    dc.w screen_bpl_bytes_per_row-2

;v_scroll_positions
    dc.b $00,$FF,$EE,$DD,$CC,$BB,$AA,$99
    dc.b $88,$77,$66,$55,$44,$33,$22,$11

;v_screen_end
    dc.l 0

;v_unused_04
    dc.b $B0,$A0,$90,$80
    dc.b $70,$60,$50,$40,$30,$20,$10,$00

;v_decoded_bitplane_bytes
    dc.b 0,0,0,0,0,0,0,0,0,0

;v_current_map_columns
    dc.b 0

;v_current_map_rows
    dc.b 0

;v_joystick_value
    dc.b 0

;v_scroll_previous_x_direction
    dc.b 1

;v_y_scroll_velocity
    dc.b 1

;v_x_scroll_velocity
    dc.b 1

;v_previous_joystick_value
    dc.b 0

;v_previous_x_step_value
    dc.b 0

;v_previous_y_step_value
    dc.b 0

;v_map_tile_width
    dc.b 0

;v_map_tile_height
    dc.b 0

;v_debug_char_lut
    ;blk.b 16,9
    dc.b $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09
    dc.b $1C,$1E,$1F,$20,$21,$22,$23,$24,$25,$26,$09,$09,$09,$09,$09,$09
    dc.b $09,$00,$01,$02,$03,$04,$05,$06,$07,$08,$0A,$0B,$0C,$0D,$0E,$0F
    dc.b $10,$11,$12,$14,$15,$16,$17,$18,$19,$1A,$1B,$09,$09,$09,$09,$09

;v_debug_hexchar_lut
    dc.b "0123456789ABCDEF"

;v_text_buffer
    ds.b 4
    EVEN

DebugStringMapX:
    dc.b "MAPX ",0

DebugStringMapY:
    dc.b "MAPY ",0

DebugStringMa:
    dc.b "MA",0

DebugStringPx:
    dc.b "PX",0

DebugStringPy:
    dc.b "PY",0

*******************************************************************************
* CHIPMEM
*******************************************************************************

    SECTION AllData,DATA_C

Copper:

    dc.w $0180,$0111
    dc.w $1401,$fffe                                        ;dc.w $2b01,$fffe

    dc.w BPLCON0,$0200
    dc.b 0,DIWSTRT,vert_display_start,h_display_start
    dc.b 0,DIWSTOP,vert_display_stop,h_display_stop
    dc.w DDFSTRT,DMA_fetch_start                            ;$28 for 22 columns; $38 for 20 columns (etc)
    dc.w DDFSTOP,DMA_fetch_stop                             ;$d0 for 22 columns; $c0 for 20 columns

    dc.w BPL1MOD,screen_modulo
    dc.w BPL2MOD,screen_modulo

;c_horizontal_scroll_pos_01
    dc.w BPLCON1,$0000

;c_sprites_enable_01
    dc.w $01fe,$0000                                        ;NOP

;c_palette_01
    level_1_main_pal

;c_bitplane_pointers_01
    dc.w $00e0,0                                            ;1
    dc.w $00e2,0
    dc.w $00e4,0                                            ;2
    dc.w $00e6,0
    dc.w $00e8,0                                            ;3
    dc.w $00ea,0
    dc.w $00ec,0                                            ;4
    dc.w $00ee,0
    dc.w $00f0,0                                            ;5
    dc.w $00f2,0
;   dc.w $00f4,0                                            ;6
;   dc.w $00f6,0

;;c_sprites01_cols
;    dc.w $1a2,0
;    dc.w $1a4,0
;    dc.w $1a6,0
;
;;c_sprites23_cols
;    dc.w $1a8,$0000
;    dc.w $1aa,$0111
;    dc.w $1ac,$0ddd
;
;;c_sprites45_cols
;    dc.w $1ae,$0000
;    dc.w $1b0,$0d00
;    dc.w $1b2,$0ddd
;
;;c_sprites67_cols
;    dc.w $1b4,$0d00
;    dc.w $1b6,$00f0
;    dc.w $1b8,$0ddd


;c_sprites01_cols
    dc.w $01fe,0
    dc.w $01fe,0
    dc.w $01fe,0

;c_sprites23_cols
    dc.w $01fe,$0000
    dc.w $01fe,$0000
    dc.w $01fe,$0000

;c_sprites45_cols
    dc.w $01fe,$0000
    dc.w $01fe,$0000
    dc.w $01fe,$0000

;c_sprites67_cols
    dc.w $01fe,$0000
    dc.w $01fe,$0000
    dc.w $01fe,$0000



;c_sprite00
    dc.w $120,0                                             ;SPR0PTH
    dc.w $122,0                                             ;SPR0PTL

;c_sprite01
    dc.w $124,0                                             ;SPR1PTH
    dc.w $126,0                                             ;SPR1PTL

;c_sprite02
    dc.w $128,0
    dc.w $12a,0

;c_sprite03
    dc.w $12c,0
    dc.w $12e,0

;c_sprite04
    dc.w $130,0
    dc.w $132,0

;c_sprite05
    dc.w $134,0
    dc.w $136,0

;c_null_sprites
    dc.w $138,0
    dc.w $13a,0
    dc.w $13c,0
    dc.w $13e,0

;c_display_enable_01
    dc.w BPLCON0,$5200

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
    dc.w $00f0,0                                            ;5
    dc.w $00f2,0
;   dc.w $00f4,0                                            ;6
;   dc.w $00f6,0

    dc.w $0180,$0FF9

    dc.w $ffff,$fffe

CopperE:

    EVEN

DebugStringMapXBitmap:
    ds.b debug_string_mapx_size

DebugStringMapYBitmap:
    ds.b debug_string_mapy_size

Sprite00:
    dc.w $304C,$4200    ;Vstart.b,Hstart/2.b,Vstop.b,%A0000SEH
    ds.w 20
Sprite00Line02:
    ds.w 18

Sprite01:
    dc.w $3054,$4200    ;Vstart.b,Hstart/2.b,Vstop.b,%A0000SEH
    ds.w 20
Sprite01Line02:
    ds.w 18

Sprite02:
    dc.w $2C40,$0000    ;Vstart.b,Hstart/2.b,Vstop.b,%A0000SEH
    REPT 256
    dc.w $FFFF,$0000
    ENDR
    dc.w 0,0

Sprite03:
    dc.w $3066,$4200    ;Vstart.b,Hstart/2.b,Vstop.b,%A0000SEH
    ds.w 20
Sprite03Line02:
    ds.w 18

Sprite04:
    dc.w $3060,$4200    ;Vstart.b,Hstart/2.b,Vstop.b,%A0000SEH
    ds.w 20
Sprite04Line02:
    ds.w 18

Sprite05:
    dc.w $3068,$4200    ;Vstart.b,Hstart/2.b,Vstop.b,%A0000SEH
    ds.w 20
Sprite05Line02:
    ds.w 18


NullSpr:
    dc.w $2a20,$2b00
    dc.w 0,0
    dc.w 0,0

*******************************************************************************
* BUFFERS
*******************************************************************************

    SECTION AllBuffers,BSS_C

Screen:
    ds.b screen_bpl_bytes_per_row*screen_bitplanes*(screen_buffer_height+2+16)
ScreenE:

Screen2:
    ds.b screen_bpl_bytes_per_row*screen_bitplanes*(screen_buffer_height+2+16)
Screen2E:

    EVEN

MapSourceBitmap:
    ds.b map_bytes
MapSourceBitmapE:

;MapSourceDungeonBitmap:
;    ds.b map_dungeon_bytes
;MapSourceDungeonBitmapE:
;
;MapSourceWisemanShopBitmap:
;    ds.b map_wiseman_bytes
;MapSourceWisemanShopBitmapE:
