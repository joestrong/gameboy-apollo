DEF playerX      EQU $C000
DEF playerY      EQU $C001
DEF playerXSpeed EQU $C002
DEF playerYSpeed EQU $C003

SECTION "Game", ROM0

CreateGame:
    ; Reset scroll
    ld a, 0
    ld [rSCX], a

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
    ld a, [playerX]
    inc a
    ld [playerX], a
    pop af
    ret

PlayerMoveLeft:
    push af
    ld a, [playerX]
    dec a
    ld [playerX], a
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

    ld a, [playerX] ; X
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
    ld bc, GameTileMapEnd - GameTileMap
.CopyTileMap
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jr nz, .CopyTileMap

    ret

SECTION "GameTileMap", ROM0

GameTileMap:
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
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$44,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$44,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$44,$00,$41,$42
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$44,$50,$52
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$51,$52
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$51,$52
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$51,$52
    DB $44,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$51,$52
    DB $43,$44,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
    DB $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
    DB $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
    DB $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
GameTileMapEnd:
