*******************************************************************************
* DEFINES
*******************************************************************************

;"tile" here means 16x16 pixels

tile_bitplanes                      = screen_bitplanes
tile_height                         = 16
tile_width                          = 16
tile_plane_lines                    = tile_bitplanes*tile_height
tile_blit_size                      = tile_plane_lines*64+(tile_width/8)

tile_bytes_per_row                  = test_cols_to_decode*2
tiles_per_row                       = test_cols_to_decode

tile_index_mask                     = $07ff

