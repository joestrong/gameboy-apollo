DEF screenX      EQU $C000
DEF playerX      EQU $C001
DEF playerY      EQU $C002
DEF playerXSpeed EQU $C003
DEF playerYSpeed EQU $C004

DEF maxScreenX   EQU 1

SECTION "Game", ROM0

CreateGame:
    ; Reset scroll
    xor a
    ld [screenX], a
    ld [rSCX], a
    ld [playerXSpeed], a
    ld [playerYSpeed], a

    call LoadTileMap
    call LoadSprites

    ld a, 32
    ld [playerX], a

    ld a, 120
    ld [playerY], a

    call PlaceSprites
    ret

UpdateGame:
    call PlaceSprites ; Update visuals with positions

    ; Check for dpad input
    ld hl, rP1
    ld a, P1F_4 ; using dpad mode
    cpl
    ld [hl], a

    ld a, [hl] ; delay few cycles
    ld a, [hl] ; delay few cycles
    cpl ; invert
    and $0F
    ld b, a
    swap b

    ; Check for button input
    ld hl, rP1
    ld a, P1F_5 ; using button mode
    cpl
    ld [hl], a

    ld a, [hl] ; delay few cycles
    ld a, [hl] ; delay few cycles
    ld a, [hl] ; delay few cycles
    cpl ; invert
    and $0F
    or b

    bit 4, a
    call nz, PlayerMoveRight

    bit 5, a
    call nz, PlayerMoveLeft

    bit 0, a
    call nz, JumpPressed

    ld a, [playerY]
    cp 120
    call c, AddGravity
    call nc, CancelGravity

    ld a, [playerYSpeed]
    cp 128
    call c, UpdateFalling
    call nc, UpdateJumping

    ret

PlayerMoveRight:
    push af
    push bc

    ld a, [playerX]
    inc a
    cp (160 - 16) ; screen - player width
    jr z, .playerHitEdge
    ld [playerX], a
    cp 90
    jr z, .pastMidPoint
    jr .end
.pastMidPoint
    ld a, [screenX]
    cp maxScreenX
    jr nz, .resetPlayerPos
.onLastScreen
    ld a, [rSCX]
    cp 95
    jr z, .end
.resetPlayerPos
    ld a, [playerX]
    dec a
    ld [playerX], a
.incScroll
    ld a, [screenX]
    ld b, a
    ld a, [rSCX]
    ld c, a
    inc bc
    ld a, b
    ld [screenX], a
    ld a, c
    ld [rSCX], a
    call LoadTileMapEdgeRight
    jr .end
.playerHitEdge
.end

    pop bc
    pop af
    ret

PlayerMoveLeft:
    push af
    push bc

    ld a, [playerX]
    dec a
    cp 8
    jr z, .playerHitEdge
    ld [playerX], a
    cp 70
    jr z, .pastMidPoint
    jr .end
.pastMidPoint
    ld a, [screenX]
    and a
    jr nz, .resetPlayerPos
.onFirstScreen
    ld a, [rSCX]
    and a
    jr z, .end
.resetPlayerPos
    ld a, [playerX]
    inc a
    ld [playerX], a
.incScroll
    ld a, [screenX]
    ld b, a
    ld a, [rSCX]
    ld c, a
    dec bc
    ld a, b
    ld [screenX], a
    ld a, c
    ld [rSCX], a
    call LoadTileMapEdgeLeft
    jr .end
.playerHitEdge

.end

    pop bc
    pop af
    ret

JumpPressed:
    push af
    ; skip if not still
    ld a, [playerYSpeed]
    cp 0
    jr nz, .skip
    ; skip if not on ground
    ld a, [playerY]
    cp 120
    jr nz, .skip

    ld a, -7
    ld [playerYSpeed], a
    call JumpSound
.skip
    pop af
    ret

AddGravity:
    push af
    ; check for max velocity
    ld a, [playerYSpeed]
    cp 10
    jr z, .skip

    inc a
    ld [playerYSpeed], a
.skip
    pop af
    ret

CancelGravity:
    push af
    ; check that we're not jumping
    ld a, [playerYSpeed]
    cp 129
    jr nc, .skip

    ; cancel any falling
    ld a, 0
    ld [playerYSpeed], a
.skip
    pop af
    ret

UpdateFalling:
    push af
    push bc
    ld a, [playerYSpeed]
    ; skip if speed is 0
    cp 0
    jr z, .skip

    ld b, a
    ld a, [playerY]
    ld c, a
.fallLoop
    ; skip if collision
    ld a, c
    cp 120
    jr z, .afterLoop

    inc c
    dec b
    ld a, b
    cp 0
    jr nz, .fallLoop
.afterLoop
    ld a, c
    ld [playerY], a
.skip
    pop bc
    pop af
    ret

UpdateJumping:
    push af
    push bc
    ld a, [playerYSpeed]
    ld b, a
    ld a, [playerY]
    add b
    ld [playerY], a
    pop bc
    pop af
    ret

LoadSprites:
    ; Loads tiles for player sprites
    ld a, $00
    ld [$FE02], a
    ld a, $01
    ld [$FE06], a
    ld a, $10
    ld [$FE0A], a
    ld a, $11
    ld [$FE0E], a
    ld a, $20
    ld [$FE12], a
    ld a, $21
    ld [$FE16], a

    ret

PlaceSprites:
    ; Positions player sprites X & Y
    ld a, [playerY] ; Y
    ld [$FE00], a
    ld [$FE04], a
    add 8
    ld [$FE08], a
    ld [$FE0C], a
    add 8
    ld [$FE10], a
    ld [$FE14], a

    ld a, [playerX]
    ld [$FE01], a
    ld [$FE09], a
    ld [$FE11], a
    add 8
    ld [$FE05], a
    ld [$FE0D], a
    ld [$FE15], a
    ret

LoadTileMap:
    ; Load map
    ld hl, $9800
    ld de, GameTileMap
    ld bc, (32*32) ; How many tiles for 1 'screen'
.CopyTileMap
    ld a, [de]
    ld [hli], a
    inc de
    dec bc

    ld a, c
    and $1F
    jr nz, .endSkip
.lineSkip
    ld a, e
    add (32 * maxScreenX)
    ld e, a
    ld a, d
    adc 0
    ld d, a
.endSkip

    ld a, b
    or a, c
    jr nz, .CopyTileMap

    ret

LoadTileMapEdgeLeft:
    push af
    push bc
    push de

    ld a, [rSCX]
    sub 8 ; Refresh tiles slightly outside viewport
    ld c, a
    and $07
    jr nz, .exitEarly ; Don't do anything unless divisible by 8

    ; BC number of tiles to offset
    ld a, [screenX]
    ld b, a
    ; bc is currently 16-bit scrollX
    ; divide by 8 (bc becomes number of tiles)
    srl b
    rr c
    srl b
    rr c
    srl b
    rr c
    ; BC now has offset in number of tiles

    ; DE: Where to get tiles (add offset)
    ld de, GameTileMap
    ld a, e
    add c
    ld e, a
    ld a, d
    adc b
    ld d, a

    ; Recalc C as number of scroll blocks (but wraps back to 0)
    ld a, [rSCX]
    sub 8 ; Refresh tiles slightly outside viewport
    ld c, a
    srl c
    srl c
    srl c

    ; HL: Where to place tiles (add offset, that wraps to 0)
    ld hl, $9800
    ld a, l
    add c
    ld l, a

    ld c, 32 ; number of lines
.copyLoop
    ; copy tilemap data
    ld a, [de]
    ld [hl], a

    ; new target location
    ld a, l
    add 32 ; tiles per line
    ld l, a
    ld a, h
    adc 0
    ld h, a

    ; new source location
    ld a, e
    add (32 * (maxScreenX + 1)) ; tiles per line
    ld e, a
    ld a, d
    adc 0
    ld d, a

    dec c
    ld a, c
    and a
    jr nz, .copyLoop
.endloop

.exitEarly
    pop de
    pop bc
    pop af
    ret

LoadTileMapEdgeRight:
    push af
    push bc
    push de

    ld a, [rSCX]
    ld c, a
    and $07
    jr nz, .exitEarly ; Don't do anything unless divisible by 8

    ; BC number of tiles to offset
    ld a, c
    add 160; width of screen (to calc right edge, allow overflow)
    ld c, a
    ld a, [screenX]
    adc 0
    ld b, a ; now has how many screens over
    ; bc is currently 16-bit scrollX
    ; divide by 8 (bc becomes number of tiles)
    srl b
    rr c
    srl b
    rr c
    srl b
    rr c
    ; BC now has offset in number of tiles

    ; DE: Where to get tiles (add offset)
    ld de, GameTileMap
    ld a, e
    add c
    ld e, a
    ld a, d
    adc b
    ld d, a

    ; Recalc C as number of scroll blocks (but wraps back to 0)
    ld a, [rSCX]
    add 160
    ld c, a
    srl c
    srl c
    srl c

    ; HL: Where to place tiles (add offset, that wraps to 0)
    ld hl, $9800
    ld a, l
    add c
    ld l, a

    ld c, 32 ; number of lines
.copyLoop
    ; copy tilemap data
    ld a, [de]
    ld [hl], a

    ; new target location
    ld a, l
    add 32 ; tiles per line
    ld l, a
    ld a, h
    adc 0
    ld h, a

    ; new source location
    ld a, e
    add (32 * (maxScreenX + 1)) ; tiles per line
    ld e, a
    ld a, d
    adc 0
    ld d, a

    dec c
    ld a, c
    and a
    jr nz, .copyLoop
.endloop

.exitEarly
    pop de
    pop bc
    pop af
    ret

SECTION "GameTileMap", ROM0

GameTileMap:
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$41,$42,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$44,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$44,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$44,$00,$41,$42,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$44,$50,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$51,$52,$44,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$51,$52,$43,$44,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$51,$52,$00,$41,$42,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
    DB $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
GameTileMapEnd:
