SECTION "Tiles", ROM0

LoadTiles:
    call LoadBGTiles
    call LoadOBJTiles
    ret

LoadBGTiles:
    ld de, Tiles
    ld hl, $9000
    ld bc, TilesEnd - Tiles
    call CopyTiles
    ret

LoadOBJTiles:
    ld de, Sprites
    ld hl, $8000
    ld bc, SpritesEnd - Sprites
    call CopyTiles
    ret

CopyTiles:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, CopyTiles
    ret

SECTION "Tile data", ROM0

Sprites:
    DB $03,$03,$0c,$0f,$10,$1f,$10,$1f,$21,$3e,$2f,$30,$2f,$30,$27,$38 ; player top-left
    DB $c0,$c0,$30,$f0,$08,$f8,$f8,$08,$fc,$04,$fc,$64,$fc,$64,$fc,$04 ; player top-right
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $23,$3c,$43,$7c,$4f,$7c,$73,$7f,$10,$1f,$11,$1f,$12,$1f,$12,$1f ; player middle-left
    DB $f8,$78,$f8,$08,$f0,$30,$e0,$e0,$20,$e0,$90,$f0,$50,$f0,$30,$f0 ; player middle-right
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $12,$1f,$3f,$21,$3f,$20,$1f,$10,$1f,$10,$3f,$22,$7d,$45,$7d,$7d ; player bottom-left
    DB $38,$c8,$38,$c8,$f0,$f0,$e0,$20,$f0,$10,$f8,$08,$f8,$08,$f0,$f0 ; player bottom-right
SpritesEnd:

Tiles:
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; std bg
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$C0,$3F
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FC,$03,$38,$C7
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$1F,$E0,$0F,$F0
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$00,$FF
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$01,$FE
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FC,$03,$F8,$07
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$00,$FF,$00,$FF
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$3F,$C0,$0F,$F0
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$7F,$80
    DB $FF,$00,$FF,$00,$FE,$01,$FE,$01,$FE,$01,$FE,$01,$FE,$01,$FE,$01
    DB $80,$7F,$1F,$FF,$3F,$FF,$71,$FF,$66,$FF,$66,$FF,$66,$FF,$7E,$FF
    DB $19,$E7,$81,$FF,$C3,$FF,$E3,$FF,$77,$FF,$77,$FF,$7F,$FF,$3F,$FF
    DB $CF,$F0,$C7,$F8,$E7,$F8,$E3,$FC,$F1,$FE,$F9,$FE,$F8,$FF,$FC,$FF
    DB $FE,$01,$FE,$01,$FE,$01,$FE,$01,$FE,$01,$FE,$01,$FE,$01,$FE,$01
    DB $00,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF
    DB $00,$FF,$FC,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    DB $3F,$C0,$1F,$E0,$8F,$F0,$C7,$F8,$E3,$FC,$F0,$FF,$F8,$FF,$FC,$FF
    DB $F1,$0F,$C3,$3F,$87,$7F,$9F,$7F,$1F,$FF,$3F,$FF,$7F,$FF,$7F,$FF
    DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$C1,$FF
    DB $86,$F9,$E2,$FD,$F0,$FF,$F8,$FF,$FC,$FF,$FE,$FF,$FE,$FF,$FF,$FF
    DB $3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1
    DB $3F,$C0,$3F,$C0,$3F,$C0,$3F,$C0,$3F,$C0,$3E,$C1,$3E,$C1,$3E,$C1
    DB $87,$F8,$E3,$FC,$F1,$FE,$F8,$FF,$FC,$FF,$FE,$FF,$FE,$FF,$FF,$FF
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$7F,$80,$7F,$80,$3F,$C0,$3F,$C0
    DB $FE,$01,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $3E,$FF,$00,$FF,$80,$7F,$FE,$01,$FC,$03,$FC,$03,$F8,$07,$F1,$0F
    DB $3F,$FF,$3F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    DB $FC,$FF,$FE,$FF,$FE,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF,$FF,$FF
    DB $7E,$81,$7E,$81,$3E,$C1,$1E,$E1,$9E,$E1,$8E,$F1,$CE,$F1,$C6,$F9
    DB $7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF
    DB $FF,$FF,$03,$FF,$01,$FF,$21,$DF,$03,$FF,$0F,$FF,$FF,$FF,$FF,$FF
    DB $FC,$FF,$FC,$FF,$FC,$FF,$FC,$FF,$FC,$FF,$FC,$FF,$F8,$FF,$F8,$FF
    DB $7F,$FF,$FF,$FF,$FF,$FF,$FE,$FF,$FF,$FF,$FF,$FF,$7F,$FF,$7F,$FF
    DB $00,$FF,$1C,$E3,$3E,$C1,$3E,$C1,$3E,$C1,$3C,$C3,$00,$FF,$81,$FF
    DB $FF,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$7F,$FF,$FF,$FF,$FF,$FF
    DB $3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1,$3E,$C1,$02,$FD
    DB $3C,$C3,$3C,$C3,$3C,$C3,$3C,$C3,$3C,$C3,$3C,$C3,$3C,$C3,$02,$FD
    DB $3F,$C0,$3F,$C0,$3F,$C0,$3F,$C0,$3F,$C0,$3F,$C0,$3F,$C0,$3F,$C0
    DB $F3,$0F,$E3,$1F,$E7,$1F,$C7,$3F,$8F,$7F,$9F,$7F,$1F,$FF,$3F,$FF
    DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$E0,$FF,$E0,$FF
    DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$03,$FF,$03,$FF
    DB $E6,$F9,$E2,$FD,$F0,$FF,$F8,$FF,$F8,$FF,$FC,$FF,$FC,$FF,$FE,$FF
    DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$FF,$00,$FF,$00,$FF,$3F,$C0
    DB $F0,$FF,$F2,$FD,$C3,$FC,$87,$F8,$1F,$E0,$3F,$C0,$FF,$00,$FF,$00
    DB $7F,$FF,$3F,$FF,$3F,$FF,$1F,$FF,$8F,$7F,$C7,$3F,$E1,$1F,$F0,$0F
    DB $F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7E,$FF
    DB $FF,$FF,$FE,$FF,$FC,$FF,$F8,$FF,$F8,$FF,$E0,$FF,$C2,$FD,$0E,$F1
    DB $00,$FF,$F8,$FF,$F8,$FF,$F8,$FF,$F8,$FF,$F8,$FF,$F8,$FF,$F8,$FF
    DB $00,$FF,$F8,$FF,$F9,$FE,$F9,$FE,$F9,$FE,$F9,$FE,$F9,$FE,$F9,$FE
    DB $FF,$FF,$FE,$FF,$FC,$FF,$F8,$FF,$F9,$FE,$E1,$FE,$C3,$FC,$0F,$F0
    DB $3F,$C0,$3F,$C0,$7F,$80,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $00,$FF,$80,$7F,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $07,$F8,$0F,$F0,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $F0,$0F,$F8,$07,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $00,$FF,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $3F,$C0,$7F,$80,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $FC,$03,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $1E,$E1,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $00,$FF,$03,$FC,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $01,$FE,$03,$FC,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $1F,$E0,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
    DB $ff,$ff,$ff,$ff,$aa,$ff,$55,$ff,$22,$ff,$88,$77,$55,$aa,$aa,$55 ; std floor
    DB $77,$88,$dd,$00,$aa,$00,$55,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; std floor 2
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$02,$00 ; column top-left
    DB $00,$00,$00,$00,$0a,$04,$0a,$04,$0b,$05,$8b,$05,$5b,$25,$5b,$25 ; column top-right
    DB $ff,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$00 ; solid wall bg
    DB $80,$00,$e0,$00,$f0,$00,$f0,$00,$f8,$00,$fe,$00,$fe,$00,$ff,$00 ; edge of wall bg
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$04,$00,$04,$08,$04,$10,$04,$10,$04,$10,$04,$40,$24,$00,$a4 ; column middle-left
    DB $00,$a4,$00,$a4,$00,$a4,$00,$a4,$00,$a4,$00,$a4,$00,$a4,$00,$a4 ; column left
    DB $5b,$25,$5b,$25,$5b,$25,$5b,$25,$5b,$25,$5b,$25,$5b,$25,$5b,$25 ; column middle-right

TilesEnd:
