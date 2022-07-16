SECTION "SPRITES", ROM0

WipeSprites:
    ld a, 0
    ld hl, $FE00
    ld b, 160 
.Loop
    ld [hli], a
    dec b
    cp b
    jr nz, .Loop
    ret
