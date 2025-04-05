menu:
;        0123456789012345678901234567890123456789


            .word $c060,mTitle,$c141,mSpeed,$c191,gap,$c1e1,inst,$c3a1,prStart,$c2a9,prTab,$c49f,dfs
            .byte $ff
mTitle:     .text   "PENDULUM^"
mSpeed:     .text   "Use LEFT/RIGHT to set speed.   $^"
gap:        .text   "Use UP/DOWN to set the gap.    $^"
inst:       .text   "Use I/SHIFT I to change instrument.^"
prStart:    .text   "PRESS RETURN TO START.^"
prTab:      .text   "TAB will return to this menu.^"
dfs:        .text   "DefianceStudios^"
.byte $ff