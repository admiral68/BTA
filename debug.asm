*******************************************************************************
* ROUTINES
*******************************************************************************

SetupDebugStrings:
    lea     FastData,a0

    lea     DebugStringMapX,a1
    lea     DebugStringMapXBitmap,a2
    bsr     TESTPreRenderDebugString6Chars

    lea     DebugStringMapY,a1
    lea     DebugStringMapYBitmap,a2
    bsr     TESTPreRenderDebugString6Chars

    lea     DebugStringMa,a1
    lea     Sprite00+4,a2
    bsr     TESTPreRenderDebugStringToSprite

    lea     DebugStringMa,a1
    lea     Sprite00Line02,a2
    bsr     TESTPreRenderDebugStringToSprite

    lea     DebugStringPx,a1
    lea     Sprite01+4,a2
    bsr     TESTPreRenderDebugStringToSprite

    lea     DebugStringPy,a1
    lea     Sprite01Line02,a2
    bsr     TESTPreRenderDebugStringToSprite
	
    lea     Copper,a3

    lea     DebugFontBitmapSourceE-4*2+2,a2
    lea     c_sprites01_cols+2(a3),a1
    moveq   #3-1,d0
.coll:
    move.w  (a2)+,(a1)+
    addq.w  #2,a1
    dbf     d0,.coll

    WRITEBPP a3,c_sprite00,Sprite00
    WRITEBPP a3,c_sprite01,Sprite01
    WRITEBPP a3,c_sprite02,Sprite02
;    WRITEBPP a3,c_sprite03,Sprite03
    WRITEBPP a3,c_sprite04,Sprite04
    WRITEBPP a3,c_sprite05,Sprite05

    lea     NullSpr,a2
    move.l  a2,d1
    moveq   #2-1,d0

.sprpl:
    addq.w  #8,a1
    swap    d1
    move.w  d1,2(a1)
    swap    d1
    move.w  d1,6(a1)
    dbf     d0,.sprpl

    rts
;-----------------------------------------------
