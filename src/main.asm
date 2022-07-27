INCLUDE "inc/hardware.inc"
INCLUDE "src/lcd.asm"
INCLUDE "src/tiles.asm"
INCLUDE "src/sprites.asm"
INCLUDE "src/menu.asm"
INCLUDE "src/game.asm"

SECTION "Header", ROM0[$100]

	jp Init

	ds $150 - @, 0 ; Make room for the header

Init:
	; Shut down audio circuitry
	ld a, 0
	ld [rNR52], a

    call WaitVBlank
    call LCDOff
    call LoadTiles
    call WipeSprites

MenuScreen:
    call CreateMenu
    call LCDOn

	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ld [rBGP], a

	ld a, %11010000
	ld [rOBP0], a

.MenuLoop
    call WaitVBlank
    call UpdateMenu
    jp .MenuLoop

GameScreen:
    call WaitVBlank
    call LCDOff
    call CreateGame
    call LCDOn
.GameLoop
    call WaitVBlank
    call UpdateGame
    jp .GameLoop
