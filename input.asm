InputInitControllers:
    move.w  #$ff00,POTGO(a6)
    rts

;-----------------------------------------------
InputUnlatchButton:
;d0=0=button1;ne=button2
    tst d0
    bne .button_2
    bclr.b #4,v_joystick_value(a0)                          ;button 1 up
    rts
.button_2
    bclr.b #5,v_joystick_value(a0)                          ;button 2 up
    rts

;-----------------------------------------------
InputReadController:
; Read joystick button and directions for port d0.

    tst.b d0
    bne .port_1

.port_0
    btst #10,POTGOR(a6)
    bne .check_port_0_button_1
    bset.b #5,v_joystick_value(a0)                          ;button 2 down

.check_port_0_button_1
    btst #6,CIAA+CIAPRA
    bne .set_port_0
    bset.b #4,v_joystick_value(a0)                          ;button 1 down

.set_port_0:
    move.w JOY0DAT(a6),d1
    bra .set_directions

.port_1
;check button
    btst #14,POTGOR(a6)
    bne .check_port_1_button_1
    bset.b #5,v_joystick_value(a0)                          ;button 2 down

.check_port_1_button_1
    btst #7,CIAA+CIAPRA
    bne .set_port_1
    bset.b #4,v_joystick_value(a0)                          ;button 1 down

.set_port_1:
    move.w JOY1DAT(a6),d1

.set_directions:

    move.w d1,d2
    lsr.w #1,d2
    eor.w d1,d2

.check_down
    and.b #$01,d2
    beq .clear_down

    bset.b #1,v_joystick_value(a0)                          ;down
    bclr.b #2,v_joystick_value(a0)                          ;no up
    bra .check_right

.clear_down
    bclr.b #1,v_joystick_value(a0)                          ;no down

.check_up:
    and.w #$0100,d2
    beq .clear_up

    bset.b #2,v_joystick_value(a0)                          ;up
    bclr.b #1,v_joystick_value(a0)                          ;no down
    bra .check_right

.clear_up
    bclr.b #2,v_joystick_value(a0)                          ;no up

.check_right
    and.b #$02,d1
    beq .clear_right

    bset.b #0,v_joystick_value(a0)                          ;right
    bclr.b #3,v_joystick_value(a0)                          ;no left
    rts

.clear_right
    bclr.b #0,v_joystick_value(a0)                          ;no right

.check_left:
    and.w   #$0200,d1
    beq .clear_left

    bset.b #3,v_joystick_value(a0)                          ;left
    bclr.b #0,v_joystick_value(a0)                          ;no right
    rts

.clear_left:
    bclr.b #3,v_joystick_value(a0)                          ;no left
    rts

;-----------------------------------------------
