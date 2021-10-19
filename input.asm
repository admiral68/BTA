InputInitControllers:
    move.w  #$ff00,POTGO(a6)
    rts

;-----------------------------------------------
InputReadController:
; Read joystick button and directions for port d0.
    tst.b d0
    bne .port_1

.port_0
    btst #10,POTGOR(a6)
	bne .clear_port_0_button_2
    mcgeezer_special
	bset.b #5,v_scroll_command(a0)                          ;button 2 down
	bra .check_port_0_button_1
.clear_port_0_button_2
    bclr.b #5,v_scroll_command(a0)                          ;button 2 up

.check_port_0_button_1
    btst #6,CIAA+CIAPRA
    bne .clear_port_0_button_1
	bset.b #4,v_scroll_command(a0)                          ;button 1 down
	bra .set_port_0
.clear_port_0_button_1
    bclr.b #4,v_scroll_command(a0)                          ;button 1 up

.set_port_0:
    move.w JOY0DAT(a6),d1
    bra .set_directions

.port_1
;check button
    btst #14,POTGOR(a6)
	bne .clear_port_1_button_2
	bset.b #5,v_scroll_command(a0)                          ;button 2 down
	bra .check_port_1_button_1
.clear_port_1_button_2
    bclr.b #5,v_scroll_command(a0)                          ;button 2 up

.check_port_1_button_1
    btst #7,CIAA+CIAPRA
    bne .clear_port_1_button_1
	bset.b #4,v_scroll_command(a0)                          ;button 1 down
	bra .set_port_0
.clear_port_1_button_1
    bclr.b #4,v_scroll_command(a0)                          ;button 1 up

.set_port_1:
    move.w JOY1DAT(a6),d1

.set_directions:
    move.w d1,d2
    lsr.w #1,d2
    eor.w d1,d2

.check_down
    and.b   #$01,d2
    beq .check_up
    bset.b #1,v_scroll_command(a0)                          ;down
    bra .check_right

.check_up:    
    and.w   #$0100,d2
    beq .check_right
    bset.b #2,v_scroll_command(a0)							;up

.check_right
    and.b   #$02,d1
    beq .check_left

    bset.b #0,v_scroll_command(a0)							; right
    bra .end
	
.check_left:
    and.w   #$0200,d1
    beq .end
    bset.b #3,v_scroll_command(a0)							; left

.end:
    rts

;-----------------------------------------------
