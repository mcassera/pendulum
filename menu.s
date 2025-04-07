menu:
;        0123456789012345678901234567890123456789


            .word $c060,mTitle
            .byte $70
            .word $c141,mSpeed
            .byte $f0
            .word $c191,gap
            .byte $f0
            .word $c1e1,inst
            .byte $f0
            .word $c3a1,prStart
            .byte $f0
            .word $c2a9,prTab
            .byte $f0 
            .word $c2d1,quitP
            .byte $50
            .word $c49f,dfs
            .byte $50
            .byte $ff
mTitle:     .text   "PENDULUM^"
mSpeed:     .text   "Use LEFT/RIGHT to set speed.   $^"
gap:        .text   "Use UP/DOWN to set the gap.    $^"
inst:       .text   "Use I/SHIFT I to change instrument.^"
prStart:    .text   "PRESS RETURN TO START.^"
prTab:      .text   "TAB will return to this menu.^"
quitP       .text   "Press 'q' to quit^"
dfs:        .text   "DefianceStudios^"
.byte $ff