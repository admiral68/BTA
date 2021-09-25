    INCDIR ""
    INCLUDE "photon/PhotonsMiniWrapper1.04!.S"
    INCLUDE "photon/Blitter-Register-List.S"

*******************************************************************************
* DEFINES
*******************************************************************************

w               =320
h               =256
bplsize         =w*h/8

bpls            =3                      ;handy values:
bpl             =w/16*2                 ;byte-width of 1 bitplane line
bwid            =bpls*bpl               ;byte-width of 1 pixel line (all bpls)

    *-----------------*
	* logo dimensions *
    *-----------------*

logow           =320
logomargin      =(320-logow)/2
logobpl         =logow/8
logobwid        =logobpl*3

    *-----------------*
	* palettes        *
    *-----------------*

logobgcol       =$44f



    *-----------------*
	* registers       *
    *-----------------*


INTREQR         = $1F
COP1LCH         = $80
DIWSTRT         = $8E
DIWSTOP         = $90
INTENA          = $9A
INTREQ          = $9C


*******************************************************************************
* MACROS
*******************************************************************************

logocolors:macro
    dc.w $068e,$0adf,$0dff
    dc.w $09bf,$056d,$044b,$033a
    endm
	
copper_pal_03:macro
    dc.w $0180,$0b87,$0182,$0754,$0184,$0975,$0186,$0ca8
    dc.w $0188,$0ed8,$018a,$0fff,$018c,$0060,$018e,$0090
    dc.w $0190,$00e0,$0192,$0777,$0194,$0aaa,$0196,$0747
    dc.w $0198,$0868,$019a,$0a8a,$019c,$0cac,$019e,$0111
	endm

WAITBLIT:macro
    tst DMACONR(a6)                         ;for compatibility
    btst #6,DMACONR(a6)
    bne.s *-6
    endm

*******************************************************************************
* GAME
*******************************************************************************

Init:
    movem.l d0-a6,-(sp)



    lea Screen,a1
    bsr.w ClearScreen

    lea Logo,a0                             ;ptr to first bitplane of logo
    lea CopBplP,a1                          ;where to poke the bitplane pointer words.
    move #3-1,d0
.bpll:
    move.l a0,d1
    swap d1
    move.w d1,2(a1)                         ;hi word
    swap d1
    move.w d1,6(a1)                         ;lo word

    addq #8,a1                              ;point to next bpl to poke in copper
    lea logobpl(a0),a0
    dbf d0,.bpll



    movem.l (sp)+,d0-a6
    rts

*******************************************************************************
* START
*******************************************************************************

StartGame:
    bsr.w Init

    lea $DFF000,a6
    move.w #$87C0,DMACON(a6)                ;SET+BLTPRI+DMAEN+BPLEN+COPEN+BLTEN

    move.l #Copper,COP1LCH(a6)
    move.l #VBint,$6c(a4)                   ;set vertb interrupt vector compatibly.
    move.w #$c020,INTENA(a6)                ;enable interrupts generally
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
* ROUTINES
*******************************************************************************

ClearScreen:                    			;a1=screen destination address to clear
    WAITBLIT
    clr.w $66(a6)               			;destination modulo
    move.l #$01000000,$40(a6)   			;set operation type in BLTCON0/1
    move.l a1,$54(a6)           			;destination address
    move.w #h*bpls*64+bpl/2,$58(a6) 		;blitter operation size
    rts

VBint:                      				;Blank template VERTB interrupt
    movem.l d0-a6,-(sp)     				;Save used registers
    lea $DFF000,a6
    btst #5,INTREQR(a6)     				;INTREQR check if it's our vertb int.
    beq.s .notvb

    moveq #$20,d0            				;poll irq bit
    move.w d0,INTREQ(a6)
    move.w d0,INTREQ(a6)

.notvb:
    movem.l (sp)+,d0-a6      				;restore
    rte

    even

*******************************************************************************
* DATA (FASTMEM)
*******************************************************************************

SkyBufferL:
    dc.l 0
    dc.l 0
SkyBufferLE:

*******************************************************************************
* CHIPMEM
*******************************************************************************

    SECTION AllData,DATA_C

Copper:
    dc.w $01fc,0         					;slow fetch mode, AGA compatibility
    dc.w $0100,$0200
    dc.b 0,$8e,$2c,$81
    dc.b 0,$90,$2c,$c1
    dc.w $0092,$38
    dc.w $0094,$d0

    dc.w $0108,logobwid-logobpl
    dc.w $010a,logobwid-logobpl

    dc.w $0102,$20
    dc.w $0104,0

    dc.w $0180,$044f,$0182,$068e,$0184,$0adf,$0186,$0dff
    dc.w $0188,$09bf,$018a,$056d,$018c,$044b,$018e,$033a

    dc.w $19df,$fffe



CopBplP:
    dc.w $00e0,0          					;1
    dc.w $00e2,0
    dc.w $00e4,0          					;2
    dc.w $00e6,0
    dc.w $00e8,0          					;3
    dc.w $00ea,0
;   dc.w $00ec,0          					;4
;   dc.w $00ee,0
;   dc.w $00f0,0          					;5
;   dc.w $00f2,0
;   dc.w $00f4,0          					;6
;   dc.w $00f6,0


    ;dc.w $0180,logobgcol
	
	
    dc.w $0100,$3200
    dc.w $4a07,$fffe

    dc.w $0180,$0b87,$0182,$0754,$0184,$0975,$0186,$0ca8
    dc.w $0188,$0ed8,$018a,$0fff,$018c,$0060,$018e,$0090
    dc.w $0190,$00e0,$0192,$0777,$0194,$0aaa,$0196,$0747
    dc.w $0198,$0868,$019a,$0a8a,$019c,$0cac,$019e,$0111

	dc.w $9207,$fffe

    dc.w $0180,$044f,$0182,$068e,$0184,$0adf,$0186,$0dff
    dc.w $0188,$09bf,$018a,$056d,$018c,$044b,$018e,$033a

    dc.w $ffff,$fffe
CopperE:

Oldguy: INCBIN "gfx/testgrfx.bin"
	dcb.b logobwid*6,0

Logo:   INCBIN "gfx/sky3centered.raw"
LogoE:
	dcb.b logobwid*6,0

EVEN

*******************************************************************************
* BUFFERS
*******************************************************************************

    SECTION AllBuffers,BSS_C
	
Screen:
    ds.b bplsize*3
ScreenE:


