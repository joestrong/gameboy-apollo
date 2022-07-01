INCLUDE "inc/hardware.inc"

SECTION "TOOLS", ROM0

WaitVBlank:
	ld a, [rLY]
	cp 144
	jp nz, WaitVBlank
    ret

LCDOff:
	ld a, 0
	ld [rLCDC], a
    ret

LCDOn:
	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON
	ld [rLCDC], a
    ret
