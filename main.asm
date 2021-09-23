
START:

.lmb 
          btst #6,$bfe001
          bne.s .lmb

          moveq #0,d0
          rts
