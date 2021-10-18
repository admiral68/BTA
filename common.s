*******************************************************************************
* DEFINES
*******************************************************************************
    *-----------------*
    * pointer offsets *
    *-----------------*

v_tile_y_position           = 0
v_tile_x_position           = 2
v_tile_decode_dest          = 4
v_tile_decode_row_dest      = 8
v_screen                    = 12
v_tile_source               = 16
v_map_y_position            = 20
v_map_x_position            = 22
v_video_y_position          = 24
v_video_x_position          = 26
v_dest_graphic_vtile_offset = 28
v_scroll_screen             = 32
v_scroll_screen_split       = 36

v_scrollx_dest_offset_table = 40
v_scrolly_dest_offset_table = 72
v_scroll_saveword           = 104
v_video_x_bitplane_offset   = 106
v_scroll_positions          = 108
v_tile_y_blit_positions     = 124
v_decoded_bitplane_bytes    = 140
v_tile_columns_to_decode    = 148
v_tile_rows_to_decode       = 149
v_scroll_command            = 150
v_scroll_previous_direction = 151
v_y_scroll_velocity         = 152
v_x_scroll_velocity         = 153
v_test_started              = 154

c_horizontal_scroll_pos_01  = 34
c_sprites_enable_01         = 38
c_palette_01                = 42
c_display_enable_01         = 106
c_bitplane_pointers_01      = 110
c_split_stop                = 140
c_split                     = 144
c_bitplane_pointers_02      = 150

    *-----------------*
    * constants:video *
    *-----------------*

screen_width                        = 352                                               ;CONVERT
screen_height                       = 256                                               ;CONVERT
screen_buffer_height                = 288                                               ;CONVERT
screen_columns                      = screen_width/tile_width
screen_rows                         = screen_height/tile_height
screen_buffer_rows                  = screen_buffer_height/tile_height
screen_bitplanes                    = 4
screen_bpl_bytes_per_row            = screen_width/8
screen_bytes_per_row                = screen_bpl_bytes_per_row*screen_bitplanes
screen_modulo                       = (screen_width/8)*(screen_bitplanes-1)             ;offset by 3 bitplanes
screen_horz_disp_words              = screen_width/16
screen_bp_bytes_per_raster_line     = screen_horz_disp_words*2
screen_bp_tile_offset               = screen_bpl_bytes_per_row*screen_bitplanes

map_width                           = map_tile_width*tile_width
map_height                          = map_tile_height*tile_height
map_tile_width                      = test_cols_to_decode
map_tile_height                     = test_rows_to_decode
map_bpl_bytes_per_row               = map_width/8
map_bitplanes                       = 4
map_bytes_per_row                   = map_bpl_bytes_per_row*map_bitplanes

DMA_fetch_start                     = $28                                   ;CONVERT $28 for 22 columns;$38 for 20 columns
DMA_fetch_stop                      = $d0                                   ;CONVERT $d0 for 22 columns;$c0 for 20 columns
vert_display_start                  = $2c                                   ;CONVERT
vert_display_stop                   = $2c                                   ;CONVERT $12c
h_display_start                     = $71                                   ;CONVERT $81 for non-scrolling display; $91 otherwise
h_display_stop                      = $c1                                   ;CONVERT $c1 for non-scrolling display

bpls                                = 3                                     ;handy values:

vlines_per_graphic                  = 48                                    ;32

    *-----------------*
    * palettes        *
    *-----------------*


    *-----------------*
    * registers       *
    *-----------------*


INTREQR         = $1F
COP1LCH         = $80
DIWSTRT         = $8E                                                       ;Start of the screen window
DIWSTOP         = $90                                                       ;End of the screen window
DDFSTRT         = $92                                                       ;Bit=plane DMA Start
DDFSTOP         = $94                                                       ;Bit-Plane DMA Stop
INTENA          = $9A
INTREQ          = $9C

BPLCON0         = $100                                                      ;Bitplane control register 0
BPLCON1         = $102                                                      ;1 (Scroll value)
BPLCON2         = $104                                                      ;2 (Sprite <> Playfield priority)
BPL1MOD         = $108                                                      ;Modulo-Value for odd bit-planes
BPL2MOD         = $10A                                                      ;Modulo-Value for even bit-planes

*******************************************************************************
* MACROS
*******************************************************************************
mcgeezer_special:macro
    clr.w $100.w
    endm

mcgeezer_special2:macro
    clr.w $102.w
    endm

; write a copper wait instruction
; arguments: vpos,hpos,<ea>
COPWAIT:macro
    move.l  #((\1)&$ff)<<24|(((\2)&$fe)|1)<<16|$fffe,\3
    endm

tile_pal_00:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0000
    dc.w $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0110
    endm

tile_pal_01:macro
    dc.w $0180,$0b87,$0182,$0433,$0184,$0842,$0186,$0a53
    dc.w $0188,$0c64,$018a,$0db8,$018c,$0974,$018e,$0754
    dc.w $0190,$0644,$0192,$0c95,$0194,$0f85,$0196,$0ffa
    dc.w $0198,$0cca,$019a,$0998,$019c,$0666,$019e,$0111
    endm

tile_pal_02:macro
    dc.w $0180,$0b87,$0182,$0544,$0184,$0754,$0186,$0975
    dc.w $0188,$0ca8,$018a,$0eea,$018c,$0fc4,$018e,$0653
    dc.w $0190,$0974,$0192,$0c84,$0194,$0eef,$0196,$0aaa
    dc.w $0198,$0889,$019a,$0778,$019c,$0556,$019e,$0111
    endm

tile_pal_03:macro
    dc.w $0180,$0b87,$0182,$0754,$0184,$0975,$0186,$0ca8
    dc.w $0188,$0ed8,$018a,$0fff,$018c,$0060,$018e,$0090
    dc.w $0190,$00e0,$0192,$0777,$0194,$0aaa,$0196,$0747
    dc.w $0198,$0868,$019a,$0a8a,$019c,$0cac,$019e,$0111
    endm

tile_pal_04:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0000
    dc.w $0190,$0520,$0192,$0730,$0194,$0940,$0196,$0b50
    dc.w $0198,$0d60,$019a,$0f80,$019c,$0000,$019e,$0111
    endm

tile_pal_05:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0455
    dc.w $0190,$0600,$0192,$0900,$0194,$0986,$0196,$0a97
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
    endm

tile_pal_06:macro
    dc.w $0180,$0046,$0182,$089c,$0184,$0789,$0186,$0678
    dc.w $0188,$0567,$018a,$0456,$018c,$0345,$018e,$0540
    dc.w $0190,$0753,$0192,$0864,$0194,$0a75,$0196,$0c86
    dc.w $0198,$0ea7,$019a,$0fc8,$019c,$0ffa,$019e,$0000
    endm

tile_pal_07:macro
    dc.w $0180,$0000,$0182,$0afd,$0184,$07ec,$0186,$00c9
    dc.w $0188,$00a7,$018a,$0086,$018c,$0064,$018e,$0050
    dc.w $0190,$0040,$0192,$0000,$0194,$0000,$0196,$0000
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
    endm

tile_pal_08:macro
    dc.w $0180,$0000,$0182,$0fb9,$0184,$0e98,$0186,$0d86
    dc.w $0188,$0c75,$018a,$0a64,$018c,$0853,$018e,$0640
    dc.w $0190,$0435,$0192,$0857,$0194,$0b75,$0196,$0b5a
    dc.w $0198,$089c,$019a,$0789,$019c,$0046,$019e,$0000
    endm

tile_pal_09:macro
    dc.w $0180,$0000,$0182,$0cb8,$0184,$0ba7,$0186,$0a96
    dc.w $0188,$0985,$018a,$0874,$018c,$0763,$018e,$0650
    dc.w $0190,$0540,$0192,$0430,$0194,$0dd0,$0196,$0d90
    dc.w $0198,$0c70,$019a,$0900,$019c,$0700,$019e,$0000
    endm

tile_pal_0a:macro
    dc.w $0180,$0000,$0182,$0540,$0184,$0750,$0186,$0940
    dc.w $0188,$0e70,$018a,$0340,$018c,$0450,$018e,$0560
    dc.w $0190,$0670,$0192,$0780,$0194,$0990,$0196,$0aa0
    dc.w $0198,$0cc0,$019a,$0de0,$019c,$0ef0,$019e,$0000
    endm

tile_pal_0b:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0455
    dc.w $0190,$0566,$0192,$0776,$0194,$0986,$0196,$0a97
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
    endm

tile_pal_0c:macro
    dc.w $0180,$0000,$0182,$0899,$0184,$0aaa,$0186,$0998
    dc.w $0188,$0887,$018a,$0776,$018c,$0665,$018e,$0554
    dc.w $0190,$0440,$0192,$0000,$0194,$0000,$0196,$0000
    dc.w $0198,$089c,$019a,$0789,$019c,$0046,$019e,$0000
    endm

tile_pal_0d:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0000
    dc.w $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
    dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
    endm

tile_pal_0e:macro
    dc.w $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
    dc.w $0188,$0000,$018a,$0000,$018c,$0000,$018e,$0000
    dc.w $0190,$0035,$0192,$0146,$0194,$0257,$0196,$0368
    dc.w $0198,$0479,$019a,$058a,$019c,$0abc,$019e,$0000
    endm

tile_pal_0f:macro
    dc.w $0180,$0111,$0182,$0FF9,$0184,$0EC7,$0186,$0DA6
    dc.w $0188,$0C85,$018a,$0A74,$018c,$0864,$018e,$0753
    dc.w $0190,$0641,$0192,$0533,$0194,$0431,$0196,$0111
    dc.w $0198,$0111,$019a,$0111,$019c,$0111,$019e,$0110
    endm

obj_pal_00:macro
    dc.w $0180,$0000,$0182,$0343,$0184,$0565,$0186,$0797
    dc.w $0188,$0aca,$018a,$0fff,$018c,$0950,$018e,$0b74
    dc.w $0190,$0da6,$0192,$0fc8,$0194,$0555,$0196,$0777
    dc.w $0198,$0aaa,$019a,$0ccc,$019c,$0000,$019e,$0111
    endm

obj_pal_01:macro
    dc.w $0180,$0000,$0182,$0005,$0184,$0007,$0186,$000a
    dc.w $0188,$006c,$018a,$008d,$018c,$00dd,$018e,$0800
    dc.w $0190,$0555,$0192,$0777,$0194,$0999,$0196,$0bbb
    dc.w $0198,$0600,$019a,$0b00,$019c,$0fff,$019e,$0111
    endm

obj_pal_02:macro
    dc.w $0180,$0000,$0182,$0600,$0184,$0800,$0186,$0a40
    dc.w $0188,$0c60,$018a,$0d90,$018c,$0555,$018e,$0777
    dc.w $0190,$0999,$0192,$0bbb,$0194,$0eee,$0196,$0960
    dc.w $0198,$0b84,$019a,$0da6,$019c,$0fc8,$019e,$0111
    endm

obj_pal_03:macro
    dc.w $0180,$0000,$0182,$0700,$0184,$0a00,$0186,$0c60
    dc.w $0188,$0d90,$018a,$0dd0,$018c,$0666,$018e,$0888
    dc.w $0190,$09aa,$0192,$0bdd,$0194,$0eff,$0196,$0960
    dc.w $0198,$0b84,$019a,$0da6,$019c,$0fc8,$019e,$0111
    endm

obj_pal_04:macro
    dc.w $0180,$0000,$0182,$0700,$0184,$0946,$0186,$0b59
    dc.w $0188,$0e7d,$018a,$0faf,$018c,$0666,$018e,$0888
    dc.w $0190,$09aa,$0192,$0bdd,$0194,$0eff,$0196,$0960
    dc.w $0198,$0b84,$019a,$0da6,$019c,$0fc8,$019e,$0111
    endm

obj_pal_05:macro
    dc.w $0180,$0000,$0182,$0630,$0184,$0950,$0186,$0c85
    dc.w $0188,$0da7,$018a,$0fc9,$018c,$0333,$018e,$0666
    dc.w $0190,$0999,$0192,$0ccc,$0194,$0fff,$0196,$0800
    dc.w $0198,$0b40,$019a,$0e70,$019c,$0fa0,$019e,$0111
    endm

obj_pal_06:macro
    dc.w $0180,$0000,$0182,$0700,$0184,$0a00,$0186,$0c40
    dc.w $0188,$0d60,$018a,$0e90,$018c,$0fe0,$018e,$0555
    dc.w $0190,$0777,$0192,$0999,$0194,$0bbb,$0196,$0060
    dc.w $0198,$0080,$019a,$00b0,$019c,$0eee,$019e,$0111
    endm

obj_pal_07:macro
    dc.w $0180,$0000,$0182,$0610,$0184,$0820,$0186,$0b40
    dc.w $0188,$0e80,$018a,$0fc0,$018c,$0555,$018e,$0777
    dc.w $0190,$0999,$0192,$0bbb,$0194,$0fff,$0196,$0760
    dc.w $0198,$0974,$019a,$0c96,$019c,$0fc8,$019e,$0110
    endm

obj_pal_08:macro
    dc.w $0180,$0000,$0182,$0f00,$0184,$00f0,$0186,$0ff0
    dc.w $0188,$000f,$018a,$0f0f,$018c,$00ff,$018e,$0fff
    dc.w $0190,$0000,$0192,$0f00,$0194,$00f0,$0196,$0ff0
    dc.w $0198,$000f,$019a,$0f0f,$019c,$00ff,$019e,$0fff
    endm

hud_pal_00:macro
    dc.w $0180,$007d,$0182,$0ddd,$0184,$0d00,$0186,$0000
    dc.w $0188,$0000,$018a,$0dd0,$018c,$007f,$018e,$0000
    dc.w $0190,$0111,$0192,$0880,$0194,$0dd0,$0196,$0000
    dc.w $0198,$0111,$019a,$0049,$019c,$009d,$019e,$0000
    endm

hud_pal_01:macro
    dc.w $0180,$0111,$0182,$0a50,$0184,$0e80,$0186,$0000
    dc.w $0188,$0111,$018a,$0880,$018c,$0cc6,$018e,$0000
    dc.w $0190,$0111,$0192,$000c,$0194,$00ce,$0196,$0000
    dc.w $0198,$009e,$019a,$0ddd,$019c,$005d,$019e,$0000
    endm

hud_pal_02:macro
    dc.w $0180,$0ff9,$0182,$0880,$0184,$0cc5,$0186,$0000
    dc.w $0188,$0ddd,$018a,$0d00,$018c,$0009,$018e,$0000
    dc.w $0190,$0ddd,$0192,$0d70,$0194,$004b,$0196,$0000
    dc.w $0198,$0ddd,$019a,$0dd0,$019c,$007f,$019e,$0000
    endm

hud_pal_03:macro
    dc.w $0180,$0111,$0182,$0ddd,$0184,$0d00,$0186,$0d00
    dc.w $0188,$0111,$018a,$0ddd,$018c,$000d,$018e,$000d
    dc.w $0190,$0111,$0192,$0dd6,$0194,$0880,$0196,$0000
    dc.w $0198,$0111,$019a,$0ccc,$019c,$007c,$019e,$0000
    endm

hud_pal_04:macro
    dc.w $0180,$0000,$0182,$0ee0,$0184,$0770,$0186,$0000
    dc.w $0188,$0550,$018a,$0dd0,$018c,$0880,$018e,$0000
    dc.w $0190,$008b,$0192,$00be,$0194,$0008,$0196,$0000
    dc.w $0198,$0111,$019a,$0ddd,$019c,$0000,$019e,$0000
    endm

hud_pal_05:macro
    dc.w $0180,$0555,$0182,$0999,$0184,$0eee,$0186,$0000
    dc.w $0188,$000d,$018a,$0ddd,$018c,$0008,$018e,$0000
    dc.w $0190,$0111,$0192,$0fd4,$0194,$000b,$0196,$0000
    dc.w $0198,$0111,$019a,$0fd4,$019c,$0b00,$019e,$0000
    endm

hud_pal_06:macro
    dc.w $0180,$0fd0,$0182,$0fff,$0184,$0ff8,$0186,$0000
    dc.w $0188,$0111,$018a,$0008,$018c,$000d,$018e,$0000
    dc.w $0190,$0111,$0192,$0808,$0194,$0d0d,$0196,$0000
    dc.w $0198,$0111,$019a,$0884,$019c,$0dd8,$019e,$0000
    endm

WAITBLIT:macro
    tst DMACONR(a6)                                         ;for compatibility
    btst #6,DMACONR(a6)
    bne.s *-6
    endm

    *-----------------*
    * test stuff      *
    *-----------------*

test_tilesrc_bp_offset              = $20000
test_tilesrc_upr_px_b_off           = $20

test_cols_to_decode                 = 128
test_rows_to_decode                 = 16                                                ;TODO: Make it possible to decode more than 16 rows

test_bmp_width_pixels               = 2048
test_bmp_horz_disp_words            = test_bmp_width_pixels/tile_width
test_bmp_bp_bytes_per_raster_line   = test_bmp_horz_disp_words*2
test_bmp_bytes_per_raster_line      = test_bmp_bp_bytes_per_raster_line*tile_bitplanes
test_bmp_vtile_offset               = test_bmp_bytes_per_raster_line*tile_height

test_right_scroll_extent            =(test_cols_to_decode+1)*tile_width-screen_width

