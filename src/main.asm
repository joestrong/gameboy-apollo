INCLUDE "inc/hardware.inc"
INCLUDE "src/lcd.asm"
INCLUDE "src/menu.asm"

SECTION "Header", ROM0[$100]

	jp MenuScreen

	ds $150 - @, 0 ; Make room for the header

MenuScreen:
	; Shut down audio circuitry
	ld a, 0
	ld [rNR52], a

    call WaitVBlank
    call LCDOff
    call CreateMenu
    call LCDOn

	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ld [rBGP], a

MenuLoop:
    call WaitVBlank
    call UpdateMenu
    jp MenuLoop
