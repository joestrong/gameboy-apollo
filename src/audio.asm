SECTION "audio", ROM0

InitAudio:
    ; Master volume
    ld a, %01110111
    ld [$FF24], a
    ; L/R Mixer
    ld a, %11111111
    ld [$FF25], a
    ret

JumpSound:
    ; Sound channel 2
    ld a, %01001000 ; %WWLLLLLL Wave pattern / Length
    ld [$FF16], a

    ld a, %11110001 ; %VVVVDNNN Volume / Direction 0=Down / Envelope
    ld [$FF17], a

    ld a, $FF ; pitch L
    ld [$FF18], a

    ld a, %11000011 ; $IC---HHH Initial / Counter 1=Stop / pitch H
    ld [$FF19], a
    ret
