	INCDIR ""
	INCLUDE "photon/PhotonsMiniWrapper1.04!.S"
	INCLUDE "photon/Blitter-Register-List.S"
	
StartGame:

.lmb 
          btst #6,$bfe001
          bne.s .lmb

          moveq #0,d0
          rts
