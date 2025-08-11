	rem
	rem Bank-switching example
	rem by Oscar Toledo G. http://nanochess.org
	rem Apr/27/2024
	rem

	BANK ROM 128	' Select a 128KB target ROM size.
			' Now it is in bank 0.

	PLAY FULL
	BANK SELECT 1	' Select bank 1 to access.
	PLAY tune_2

	BANK SELECT 2	' Select bank 2 to access.

	' Notice bank 2 is selected, but the
	' music is being played from bank 1.

	MODE 1
	SCREEN DISABLE
	DEFINE VRAM PLETTER $0000,$1800,image_bitmap
	DEFINE VRAM PLETTER $2000,$1800,image_color
	SCREEN ENABLE

	WHILE 1: WEND

	BANK 1		' Start of bank 1 data

	' Mecha-8 level 5: alone
	' Fragment
tune_2: DATA BYTE 5
	MUSIC G5#Y,C3#,-,M1
	MUSIC S,S,-,M2
	MUSIC F5#,G3#,-,M2
	MUSIC S,S,-,M2
	MUSIC E5,C3#,-,M1
	MUSIC S,S,-,M2
	MUSIC D5#,G3#,-,M2
	MUSIC S,S,-,M2
	MUSIC E5,C3#,-,M1
	MUSIC S,S,-,M2
	MUSIC F5#,G3#,-,M2
	MUSIC S,S,-,M2
	MUSIC G5#,C3#,-,M1
	MUSIC S,S,-,M2
	MUSIC S,G3#,-,M2
	MUSIC S,S,-,M2
	MUSIC S,C3#,-,M1
	MUSIC S,S,-,M2
	MUSIC C5#,G3#,-,M2
	MUSIC -,S,-,M2
	MUSIC G5#,C3#,-,M1
	MUSIC S,S,-,M2
	MUSIC E5,G3#,-,M2
	MUSIC -,S,-,M2
	MUSIC F5#,B2,-,M1
	MUSIC S,S,-,M2
	MUSIC S,F3#,-,M2
	MUSIC S,S,-,M2
	MUSIC S,B2,-,M1
	MUSIC S,S,-,M2
	MUSIC -,F3#,-,M2
	MUSIC -,S,-,M2
	MUSIC -,B2,-,M1
	MUSIC -,S,-,M2
	MUSIC -,F3#,-,M2
	MUSIC -,S,-,M2
	MUSIC -,B2,-,M1
	MUSIC -,S,-,M2
	MUSIC -,F3#,-,M1
	MUSIC -,S,-,M2
	MUSIC C5#,B2,-,M1
	MUSIC S,S,-,M2
	MUSIC F5#,F3#,-,M1
	MUSIC S,S,-,M2
	MUSIC C5#,B2,-,M1
	MUSIC S,S,-,M2
	MUSIC E5,A2,-,M1
	MUSIC S,S,-,M2
	MUSIC S,E3,-,M1
	MUSIC S,S,-,M2
	MUSIC S,A2,-,M1
	MUSIC S,S,-,M2
	MUSIC S,E3,-,M1
	MUSIC S,S,-,M2
	MUSIC S,A2,-,M1
	MUSIC S,S,-,M2
	MUSIC S,E3,-,M1
	MUSIC S,S,-,M2
	MUSIC S,A2,-,M1
	MUSIC S,S,-,M2
	MUSIC F5#,E3,-,M1
	MUSIC S,S,-,M2
	MUSIC E5,A2,-,M2
	MUSIC S,S,-,M2
	MUSIC D5#,E3,-,M1
	MUSIC S,S,-,M2
	MUSIC S,A2,-,M2
	MUSIC S,S,-,M2
	MUSIC C5,G2#,-,M1
	MUSIC S,S,-,M2
	MUSIC S,D3#,-,M2
	MUSIC S,S,-,M2
	MUSIC S,G2#,-,M1
	MUSIC S,S,-,M2
	MUSIC S,D3#,-,M2
	MUSIC S,S,-,M2
	MUSIC S,G2#,-,M1
	MUSIC S,S,-,M2
	MUSIC -,D3#,-,M2
	MUSIC -,S,-,M2
	MUSIC -,G2#,-,M1
	MUSIC -,S,-,M2
	MUSIC -,D3#,-,M2
	MUSIC -,S,-,M2
	MUSIC -,G2#,-,M1
	MUSIC -,S,-,M2
	MUSIC -,D3#,-,M1
	MUSIC -,S,-,M3
	MUSIC -,G2#,-,M1
	MUSIC -,S,-,M2
	MUSIC -,D3#,-,M1
	MUSIC -,S,-,M3
	MUSIC -,G2#,-,M1
	MUSIC -,S,-,M1
	MUSIC -,D3#,-,M1
	MUSIC -,S,-,M1
	MUSIC REPEAT

	BANK 2		' Start of bank 2 data.

image_bitmap:
	DATA BYTE $61,$bf,$5f,$ff,$df,$ff,$80,$00
	DATA BYTE $70,$10,$3e,$0e,$87,$00,$c0,$e0
	DATA BYTE $fc,$7f,$1f,$07,$03,$00,$18,$00
	DATA BYTE $02,$40,$12,$55,$0e,$aa,$55,$7f
	DATA BYTE $7f,$1b,$a3,$00,$09,$ea,$fd,$68
	DATA BYTE $09,$22,$e0,$c4,$07,$c0,$d5,$aa
	DATA BYTE $d4,$0f,$01,$00,$1d,$0f,$7f,$8e
	DATA BYTE $07,$8f,$90,$01,$3a,$99,$f9,$fc
	DATA BYTE $0d,$fe,$9e,$fe,$fe,$20,$33,$03
	DATA BYTE $ed,$67,$c8,$cc,$00,$88,$0c,$df
	DATA BYTE $cf,$e7,$e7,$08,$44,$d8,$01,$8a
	DATA BYTE $df,$01,$8a,$ff,$8a,$10,$fb,$40
	DATA BYTE $03,$30,$00,$05,$01,$45,$ff,$df
	DATA BYTE $c3,$00,$fc,$f8,$05,$00,$f0,$f9
	DATA BYTE $d0,$5b,$e0,$01,$45,$cc,$55,$01
	DATA BYTE $9f,$1f,$9f,$55,$3f,$5e,$38,$c3
	DATA BYTE $18,$f0,$af,$0e,$00,$8f,$1f,$1f
	DATA BYTE $01,$18,$28,$51,$a0,$41,$82,$07
	DATA BYTE $5b,$09,$27,$b1,$01,$a8,$50,$90
	DATA BYTE $1f,$8f,$11,$50,$08,$07,$40,$00
	DATA BYTE $90,$00,$59,$bf,$7f,$ea,$03,$d5
	DATA BYTE $bf,$fe,$7e,$d5,$ea,$80,$01,$55
	DATA BYTE $a2,$c0,$a2,$46,$e0,$0f,$e6,$e4
	DATA BYTE $e4,$aa,$03,$c9,$01,$01,$8f,$83
	DATA BYTE $81,$80,$80,$b0,$85,$80,$aa,$3f
	DATA BYTE $aa,$c4,$8a,$d4,$aa,$42,$d1,$01
	DATA BYTE $bf,$fc,$fc,$33,$c2,$04,$df,$04
	DATA BYTE $b1,$14,$0f,$b2,$2d,$e3,$82,$93
	DATA BYTE $03,$34,$e3,$f1,$09,$0e,$0f,$80
	DATA BYTE $84,$88,$01,$4b,$df,$1f,$08,$01
	DATA BYTE $81,$ff,$40,$30,$01,$fb,$00,$23
	DATA BYTE $41,$03,$03,$40,$22,$40,$02,$54
	DATA BYTE $93,$4f,$07,$37,$07,$d0,$e4,$19
	DATA BYTE $7f,$01,$ae,$30,$00,$fe,$03,$80
	DATA BYTE $04,$60,$ff,$01,$80,$c4,$aa,$60
	DATA BYTE $fc,$c3,$68,$c9,$10,$db,$a3,$f7
	DATA BYTE $00,$fe,$e0,$84,$97,$44,$88,$02
	DATA BYTE $00,$1f,$55,$0e,$0c,$a2,$4e,$00
	DATA BYTE $1f,$bf,$3f,$7e,$7c,$f8,$f1,$e3
	DATA BYTE $68,$c7,$85,$0f,$d5,$00,$f5,$54
	DATA BYTE $a0,$00,$01,$80,$51,$02,$4f,$28
	DATA BYTE $70,$ec,$03,$f8,$a8,$40,$80,$c7
	DATA BYTE $07,$04,$a8,$00,$07,$3a,$80,$50
	DATA BYTE $47,$b1,$22,$8a,$41,$07,$20,$2a
	DATA BYTE $55,$f9,$00,$be,$75,$aa,$1f,$82
	DATA BYTE $51,$a2,$e4,$c0,$fb,$04,$ea,$0a
	DATA BYTE $44,$aa,$7f,$00,$3f,$c4,$1f,$80
	DATA BYTE $2f,$c0,$84,$00,$a0,$54,$0f,$ac
	DATA BYTE $09,$17,$01,$22,$02,$27,$e8,$e0
	DATA BYTE $d1,$ea,$e0,$00,$0c,$fd,$7f,$3d
	DATA BYTE $fc,$00,$41,$54,$7f,$f2,$f7,$e3
	DATA BYTE $54,$22,$4f,$0a,$1f,$04,$e7,$cf
	DATA BYTE $07,$83,$18,$96,$06,$47,$02,$18
	DATA BYTE $ff,$c8,$44,$f7,$18,$b1,$38,$fb
	DATA BYTE $18,$b7,$12,$03,$88,$fd,$01,$db
	DATA BYTE $80,$fb,$10,$05,$ff,$54,$01,$10
	DATA BYTE $54,$14,$61,$54,$87,$83,$01,$01
	DATA BYTE $3f,$3f,$bf,$c0,$e1,$70,$fe,$fb
	DATA BYTE $56,$70,$47,$e8,$ff,$09,$f0,$6f
	DATA BYTE $82,$10,$06,$fd,$f9,$be,$8c,$f3
	DATA BYTE $aa,$57,$01,$82,$97,$99,$20,$75
	DATA BYTE $7a,$fb,$7b,$48,$7b,$0f,$a0,$d7
	DATA BYTE $50,$44,$95,$4a,$11,$07,$03,$4d
	DATA BYTE $00,$37,$a3,$d7,$8e,$f2,$fa,$f3
	DATA BYTE $fa,$18,$f1,$fa,$f5,$fb,$06,$50
	DATA BYTE $80,$10,$e9,$02,$05,$0a,$10,$10
	DATA BYTE $80,$00,$01,$f0,$c0,$80,$11,$7f
	DATA BYTE $0a,$15,$01,$1f,$3f,$35,$11,$8a
	DATA BYTE $0c,$8d,$ab,$57,$ac,$25,$05,$27
	DATA BYTE $08,$45,$a8,$44,$00,$01,$01,$8a
	DATA BYTE $05,$01,$2a,$0a,$82,$54,$51,$a0
	DATA BYTE $21,$5d,$0a,$45,$a7,$2b,$07,$a2
	DATA BYTE $22,$0b,$11,$63,$37,$77,$e2,$01
	DATA BYTE $02,$55,$e0,$1e,$a3,$22,$e5,$ea
	DATA BYTE $5d,$01,$c6,$ee,$fc,$3e,$0a,$0a
	DATA BYTE $0c,$06,$b2,$11,$01,$7f,$bf,$9f
	DATA BYTE $02,$d0,$e8,$d5,$ff,$14,$f1,$00
	DATA BYTE $67,$55,$82,$55,$f0,$ab,$f0,$01
	DATA BYTE $20,$50,$c2,$b5,$d0,$00,$08,$40
	DATA BYTE $e0,$01,$88,$41,$14,$55,$83,$fb
	DATA BYTE $28,$01,$23,$99,$f0,$ee,$04,$9f
	DATA BYTE $cf,$fe,$d7,$b1,$af,$20,$78,$00
	DATA BYTE $0f,$5f,$af,$07,$ef,$00,$e0,$fb
	DATA BYTE $3a,$e0,$c0,$00,$ff,$33,$ae,$d5
	DATA BYTE $0f,$01,$fd,$ef,$3c,$57,$01,$02
	DATA BYTE $50,$a2,$41,$8f,$1e,$be,$93,$40
	DATA BYTE $f7,$eb,$e7,$af,$00,$5f,$eb,$dd
	DATA BYTE $eb,$cd,$eb,$ed,$aa,$25,$e3,$c8
	DATA BYTE $8d,$c2,$9f,$11,$28,$ff,$28,$e5
	DATA BYTE $08,$0c,$1c,$0c,$d3,$0e,$ff,$50
	DATA BYTE $2b,$67,$2a,$01,$cc,$37,$bf,$eb
	DATA BYTE $18,$f9,$0c,$ab,$df,$d8,$18,$dd
	DATA BYTE $00,$54,$aa,$14,$a2,$05,$a0,$60
	DATA BYTE $d0,$a3,$a4,$01,$95,$94,$8f,$0a
	DATA BYTE $04,$28,$28,$55,$11,$af,$81,$0a
	DATA BYTE $05,$08,$14,$8a,$08,$0e,$40,$82
	DATA BYTE $41,$cf,$00,$22,$41,$02,$01,$22
	DATA BYTE $45,$69,$22,$3b,$16,$fb,$10,$fd
	DATA BYTE $51,$80,$03,$0e,$08,$7d,$2a,$15
	DATA BYTE $51,$9e,$bf,$01,$66,$af,$07,$81
	DATA BYTE $ac,$a6,$07,$c3,$63,$0c,$06,$03
	DATA BYTE $4a,$09,$0f,$c1,$32,$20,$10,$23
	DATA BYTE $31,$08,$41,$01,$01,$0c,$9f,$ff
	DATA BYTE $11,$01,$04,$55,$08,$29,$c7,$c7
	DATA BYTE $cf,$81,$51,$5f,$1f,$78,$7c,$07
	DATA BYTE $01,$38,$0c,$2c,$04,$03,$fb,$03
	DATA BYTE $f1,$8f,$53,$fc,$f4,$10,$f4,$80
	DATA BYTE $fb,$73,$47,$f1,$e9,$13,$ec,$30
	DATA BYTE $d9,$a0,$b3,$23,$10,$a0,$e6,$90
	DATA BYTE $c6,$15,$09,$23,$3d,$af,$89,$00
	DATA BYTE $03,$fb,$f7,$fb,$f5,$eb,$e5,$e9
	DATA BYTE $31,$c1,$80,$00,$e0,$e2,$00,$0c
	DATA BYTE $00,$c0,$c3,$98,$87,$ab,$55,$98
	DATA BYTE $01,$7f,$00,$ea,$47,$f5,$93,$0a
	DATA BYTE $eb,$eb,$ef,$43,$f8,$e0,$40,$9f
	DATA BYTE $10,$38,$71,$f8,$01,$cd,$cd,$75
	DATA BYTE $d8,$ad,$24,$af,$96,$8d,$d1,$32
	DATA BYTE $ea,$d4,$25,$60,$00,$f3,$41,$a7
	DATA BYTE $30,$04,$08,$08,$82,$40,$06,$0a
	DATA BYTE $01,$28,$05,$20,$24,$bd,$13,$8a
	DATA BYTE $43,$51,$99,$ce,$af,$03,$83,$a6
	DATA BYTE $9d,$e7,$3f,$6b,$37,$b2,$01,$af
	DATA BYTE $e7,$1d,$10,$07,$08,$ba,$42,$a9
	DATA BYTE $8f,$10,$cf,$c7,$ff,$fb,$0b,$05
	DATA BYTE $ff,$01,$03,$e2,$1f,$04,$3f,$be
	DATA BYTE $14,$bf,$0f,$bf,$25,$2b,$07,$90
	DATA BYTE $35,$23,$17,$82,$ff,$c3,$60,$e2
	DATA BYTE $f5,$f0,$f1,$81,$05,$e7,$ef,$aa
	DATA BYTE $cf,$cf,$d0,$8b,$fe,$29,$aa,$fe
	DATA BYTE $00,$f3,$f1,$ab,$21,$c5,$06,$15
	DATA BYTE $4d,$cd,$3d,$3d,$cc,$d9,$f3,$01
	DATA BYTE $e5,$c7,$c3,$c2,$c2,$e2,$51,$83
	DATA BYTE $a7,$c6,$a8,$c8,$08,$60,$71,$70
	DATA BYTE $51,$f5,$c2,$60,$40,$ce,$00,$f3
	DATA BYTE $6e,$e3,$58,$f9,$1d,$2a,$d7,$04
	DATA BYTE $15,$ae,$91,$af,$51,$88,$73,$2a
	DATA BYTE $01,$03,$b8,$1c,$18,$3c,$1c,$3e
	DATA BYTE $01,$fe,$f0,$b7,$98,$fc,$e5,$f1
	DATA BYTE $01,$a8,$54,$12,$ad,$e0,$dd,$11
	DATA BYTE $f5,$a2,$d5,$86,$89,$3b,$60,$1f
	DATA BYTE $bb,$d0,$2b,$99,$f4,$e0,$ce,$17
	DATA BYTE $00,$00,$72,$80,$c3,$ec,$f9,$5b
	DATA BYTE $ec,$01,$c7,$62,$e3,$00,$e1,$00
	DATA BYTE $82,$fb,$eb,$1d,$45,$9f,$50,$cf
	DATA BYTE $03,$f9,$54,$c3,$c3,$a0,$09,$fd
	DATA BYTE $3b,$17,$0b,$03,$58,$ff,$15,$f0
	DATA BYTE $00,$03,$06,$d0,$44,$25,$f1,$f3
	DATA BYTE $24,$a2,$c5,$b1,$99,$de,$03,$8f
	DATA BYTE $af,$c0,$f3,$e8,$b2,$18,$c5,$b0
	DATA BYTE $d7,$d0,$00,$89,$88,$a2,$40,$a2
	DATA BYTE $a3,$20,$d9,$31,$15,$aa,$c3,$88
	DATA BYTE $aa,$0c,$cf,$02,$00,$41,$a0,$10
	DATA BYTE $91,$31,$0a,$5f,$01,$59,$e3,$1f
	DATA BYTE $28,$2a,$d9,$fc,$d5,$20,$e0,$70
	DATA BYTE $60,$70,$36,$20,$78,$85,$08,$90
	DATA BYTE $71,$ed,$91,$bc,$a5,$c5,$57,$0e
	DATA BYTE $1f,$01,$55,$b8,$00,$1a,$cd,$f8
	DATA BYTE $cf,$35,$80,$25,$dc,$57,$05,$0f
	DATA BYTE $c5,$14,$9f,$c5,$20,$57,$eb,$18
	DATA BYTE $e1,$e3,$00,$63,$63,$32,$fc,$ea
	DATA BYTE $f7,$ef,$f7,$46,$fa,$ce,$10,$c1
	DATA BYTE $db,$47,$81,$00,$83,$03,$05,$f9
	DATA BYTE $29,$0e,$a7,$23,$05,$40,$cb,$b8
	DATA BYTE $18,$55,$8f,$01,$a7,$cd,$68,$5f
	DATA BYTE $9b,$7c,$e5,$b6,$40,$96,$07,$ad
	DATA BYTE $b6,$00,$07,$20,$7f,$07,$f9,$69
	DATA BYTE $c1,$00,$80,$da,$01,$cd,$03,$66
	DATA BYTE $c7,$00,$1b,$9b,$55,$4b,$83,$c3
	DATA BYTE $c3,$0b,$4f,$bf,$eb,$2e,$e5,$2e
	DATA BYTE $1b,$5f,$7f,$c9,$ce,$f7,$49,$9a
	DATA BYTE $c3,$ef,$e7,$e2,$c5,$6a,$c5,$02
	DATA BYTE $fb,$ff,$fb,$9f,$fb,$fd,$07,$00
	DATA BYTE $10,$51,$81,$51,$d1,$d0,$e1,$f1
	DATA BYTE $44,$a8,$93,$f9,$18,$f9,$18,$fc
	DATA BYTE $d5,$c0,$02,$45,$8a,$4f,$2a,$06
	DATA BYTE $ff,$1c,$3d,$e8,$71,$36,$29,$74
	DATA BYTE $55,$20,$e0,$00,$f9,$02,$fa,$f4
	DATA BYTE $a8,$e1,$63,$7b,$dd,$50,$7c,$7e
	DATA BYTE $3a,$3e,$1c,$3e,$3d,$03,$00,$07
	DATA BYTE $b2,$fd,$b1,$09,$f3,$99,$a0,$21
	DATA BYTE $97,$63,$b5,$7c,$83,$0d,$57,$9a
	DATA BYTE $07,$ba,$24,$af,$01,$e8,$ab,$5b
	DATA BYTE $a7,$4a,$fe,$fd,$a8,$8c,$83,$00
	DATA BYTE $a8,$4e,$04,$8d,$06,$19,$0e,$1e
	DATA BYTE $1e,$3c,$67,$00,$34,$04,$fd,$45
	DATA BYTE $fc,$e9,$b5,$44,$05,$65,$20,$b0
	DATA BYTE $85,$50,$a4,$24,$27,$b9,$29,$73
	DATA BYTE $0a,$e7,$71,$55,$9b,$d0,$0f,$b7
	DATA BYTE $75,$01,$f0,$38,$d5,$73,$c3,$a4
	DATA BYTE $ef,$81,$52,$c4,$0d,$23,$df,$f9
	DATA BYTE $0a,$ee,$94,$41,$44,$94,$f6,$2e
	DATA BYTE $4c,$02,$03,$93,$6b,$bc,$b2,$e7
	DATA BYTE $e8,$78,$83,$83,$0f,$1f,$38,$b1
	DATA BYTE $3f,$57,$01,$3e,$56,$36,$10,$02
	DATA BYTE $10,$e7,$ad,$bf,$96,$93,$6a,$02
	DATA BYTE $1f,$ae,$5f,$f2,$f5,$f6,$8b,$11
	DATA BYTE $d5,$ba,$f5,$28,$0b,$8a,$8b,$54
	DATA BYTE $5d,$a3,$20,$99,$6f,$f0,$00,$20
	DATA BYTE $b5,$80,$e2,$63,$62,$73,$97,$31
	DATA BYTE $00,$95,$f8,$b3,$ee,$09,$9f,$9a
	DATA BYTE $d5,$18,$3a,$af,$02,$fc,$00,$30
	DATA BYTE $21,$8b,$8a,$44,$00,$2e,$df,$4e
	DATA BYTE $03,$07,$e8,$4d,$12,$28,$14,$43
	DATA BYTE $f0,$c6,$4c,$01,$00,$f1,$d8,$61
	DATA BYTE $77,$a0,$50,$09,$99,$f8,$53,$2a
	DATA BYTE $75,$ee,$c9,$13,$d7,$02,$d4,$ad
	DATA BYTE $90,$23,$57,$01,$1e,$3f,$22,$99
	DATA BYTE $ab,$c5,$da,$7f,$27,$f7,$85,$df
	DATA BYTE $8b,$af,$e7,$85,$20,$02,$f3,$f4
	DATA BYTE $e7,$48,$b6,$61,$97,$a6,$b3,$01
	DATA BYTE $2d,$40,$c0,$99,$b2,$ed,$0f,$21
	DATA BYTE $2f,$07,$81,$62,$e1,$e1,$98,$e3
	DATA BYTE $e7,$00,$f3,$00,$f3,$f7,$bf,$15
	DATA BYTE $70,$78,$7d,$71,$05,$61,$55,$ef
	DATA BYTE $55,$28,$ad,$f6,$6d,$7e,$b3,$64
	DATA BYTE $b9,$25,$66,$d8,$3d,$88,$30,$d9
	DATA BYTE $4d,$20,$b1,$bf,$92,$d0,$00,$cb
	DATA BYTE $41,$9b,$0d,$07,$fc,$22,$07,$08
	DATA BYTE $a5,$07,$53,$18,$55,$7a,$75,$01
	DATA BYTE $55,$00,$87,$01,$ae,$5d,$ae,$5c
	DATA BYTE $a8,$59,$ea,$ea,$80,$da,$70,$d0
	DATA BYTE $03,$10,$04,$1f,$18,$38,$18,$83
	DATA BYTE $d7,$c0,$e6,$bc,$8f,$50,$7a,$45
	DATA BYTE $7a,$f6,$14,$fc,$b1,$d3,$00,$00
	DATA BYTE $5c,$00,$30,$2a,$14,$ad,$94,$05
	DATA BYTE $01,$ed,$0c,$0b,$d5,$21,$ab,$7f
	DATA BYTE $f8,$19,$b0,$f4,$fc,$17,$4d,$c0
	DATA BYTE $3f,$c7,$95,$15,$2e,$b6,$b1,$14
	DATA BYTE $6d,$5d,$08,$b9,$7e,$01,$6a,$e4
	DATA BYTE $d5,$81,$fc,$f5,$af,$cc,$f1,$80
	DATA BYTE $c6,$97,$4f,$a0,$01,$44,$e0,$17
	DATA BYTE $f7,$51,$01,$11,$c6,$fb,$0c,$07
	DATA BYTE $71,$01,$60,$b1,$64,$8a,$c7,$2a
	DATA BYTE $60,$07,$a7,$80,$1e,$1f,$fa,$1a
	DATA BYTE $45,$00,$9d,$81,$b9,$7c,$c6,$a3
	DATA BYTE $c0,$f8,$7c,$fc,$70,$38,$40,$01
	DATA BYTE $00,$18,$18,$1c,$1c,$39,$10,$c0
	DATA BYTE $e4,$fc,$da,$40,$fc,$a2,$08,$c4
	DATA BYTE $c6,$ce,$3e,$f1,$c7,$dc,$22,$14
	DATA BYTE $b0,$bf,$4d,$8e,$b4,$90,$04,$06
	DATA BYTE $04,$0e,$04,$f2,$59,$99,$c8,$03
	DATA BYTE $05,$aa,$5f,$23,$1f,$c0,$f7,$9d
	DATA BYTE $9b,$fe,$9e,$ce,$3c,$9a,$00,$0c
	DATA BYTE $46,$b0,$f8,$80,$38,$ed,$a9,$00
	DATA BYTE $33,$02,$4f,$9f,$01,$9d,$0e,$03
	DATA BYTE $94,$4f,$25,$92,$10,$ff,$3c,$08
	DATA BYTE $1f,$64,$13,$53,$5b,$02,$39,$04
	DATA BYTE $ea,$d3,$95,$05,$98,$7b,$6c,$f7
	DATA BYTE $e3,$18,$ef,$ef,$05,$18,$ed,$40
	DATA BYTE $f1,$79,$40,$d7,$34,$c1,$98,$e0
	DATA BYTE $a0,$ff,$81,$ef,$81,$03,$0f,$47
	DATA BYTE $91,$09,$df,$22,$11,$4b,$2b,$ad
	DATA BYTE $d3,$13,$6f,$06,$0b,$01,$0e,$05
	DATA BYTE $17,$a3,$01,$c7,$65,$3f,$c3,$a2
	DATA BYTE $0f,$33,$5c,$00,$a8,$30,$c1,$e2
	DATA BYTE $75,$40,$90,$bf,$20,$f0,$ec,$b6
	DATA BYTE $0c,$fa,$1f,$16,$06,$b5,$a8,$7f
	DATA BYTE $fb,$17,$62,$1f,$f0,$c6,$fb,$a8
	DATA BYTE $6d,$9c,$5b,$07,$97,$94,$81,$04
	DATA BYTE $9c,$9b,$1e,$1d,$24,$0e,$0e,$af
	DATA BYTE $80,$ab,$20,$61,$23,$e3,$54,$bd
	DATA BYTE $88,$2e,$c1,$c3,$31,$3f,$60,$db
	DATA BYTE $f7,$20,$fa,$6e,$f2,$01,$ee,$0e
	DATA BYTE $3e,$c1,$85,$cf,$d9,$54,$c5,$af
	DATA BYTE $9d,$01,$98,$d9,$54,$e9,$4f,$11
	DATA BYTE $02,$a7,$57,$d1,$29,$4d,$a8,$8f
	DATA BYTE $d3,$8d,$24,$5f,$c7,$a7,$61,$45
	DATA BYTE $1f,$06,$fe,$57,$fd,$88,$1f,$c0
	DATA BYTE $cf,$b5,$3f,$bd,$0a,$dd,$49,$da
	DATA BYTE $0b,$55,$74,$40,$87,$f4,$df,$06
	DATA BYTE $1f,$ff,$e0,$40,$dc,$ed,$bf,$da
	DATA BYTE $be,$9a,$87,$5a,$05,$63,$47,$eb
	DATA BYTE $14,$09,$0e,$0c,$ef,$f7,$00,$76
	DATA BYTE $41,$a8,$38,$a0,$83,$b1,$8f,$d8
	DATA BYTE $60,$b7,$04,$e1,$f5,$00,$0b,$81
	DATA BYTE $1a,$9f,$9f,$5f,$4c,$6f,$11,$7f
	DATA BYTE $8c,$84,$b0,$08,$88,$18,$02,$bb
	DATA BYTE $48,$24,$22,$87,$54,$01,$61,$01
	DATA BYTE $28,$c1,$c3,$f8,$a7,$22,$96,$00
	DATA BYTE $13,$31,$20,$60,$02,$4b,$28,$c0
	DATA BYTE $48,$e2,$b2,$10,$d3,$12,$22,$65
	DATA BYTE $cb,$a2,$8b,$da,$89,$f2,$6b,$91
	DATA BYTE $fa,$00,$fb,$43,$18,$0c,$6d,$1d
	DATA BYTE $b0,$0f,$a9,$36,$01,$c0,$a5,$3c
	DATA BYTE $22,$44,$df,$5c,$01,$36,$60,$54
	DATA BYTE $9f,$e7,$ab,$6d,$a5,$a1,$ee,$7c
	DATA BYTE $0b,$c7,$87,$e3,$1f,$54,$22,$2d
	DATA BYTE $75,$c5,$46,$0a,$87,$07,$83,$95
	DATA BYTE $1d,$ef,$45,$31,$ab,$22,$45,$fc
	DATA BYTE $d7,$8f,$0a,$38,$c9,$f8,$50,$ba
	DATA BYTE $35,$07,$88,$03,$cf,$ff,$80,$af
	DATA BYTE $3f,$14,$94,$e1,$08,$14,$11,$28
	DATA BYTE $10,$ff,$93,$f6,$be,$a0,$f0,$dc
	DATA BYTE $2c,$bf,$bf,$c2,$a0,$00,$db,$31
	DATA BYTE $8c,$cc,$00,$9c,$00,$f9,$1b,$5f
	DATA BYTE $7f,$86,$00,$d5,$fa,$f5,$56,$21
	DATA BYTE $e3,$d7,$50,$ef,$cd,$d7,$a1,$7f
	DATA BYTE $ff,$8e,$27,$aa,$bf,$f7,$84,$80
	DATA BYTE $b1,$48,$10,$b1,$62,$eb,$a7,$49
	DATA BYTE $c5,$2d,$0f,$87,$00,$0b,$a3,$11
	DATA BYTE $49,$02,$06,$1e,$06,$88,$0b,$20
	DATA BYTE $b8,$c5,$f0,$30,$21,$06,$dc,$17
	DATA BYTE $e5,$9f,$f6,$00,$21,$9d,$54,$2a
	DATA BYTE $80,$d3,$08,$f0,$93,$74,$b5,$0d
	DATA BYTE $e8,$cf,$c9,$5b,$82,$ec,$00,$ab
	DATA BYTE $45,$40,$63,$5d,$65,$5e,$55,$4b
	DATA BYTE $01,$81,$b4,$3f,$aa,$18,$b1,$90
	DATA BYTE $fc,$f8,$87,$bd,$37,$bb,$70,$01
	DATA BYTE $bb,$f7,$03,$84,$f7,$fe,$c4,$d5
	DATA BYTE $73,$c0,$80,$5b,$3d,$27,$00,$9c
	DATA BYTE $00,$fc,$43,$27,$ed,$3c,$50,$ea
	DATA BYTE $ef,$b3,$af,$5f,$34,$8f,$fd,$fa
	DATA BYTE $d1,$30,$ab,$14,$89,$0f,$7f,$b1
	DATA BYTE $07,$9e,$68,$01,$07,$7f,$c7,$d3
	DATA BYTE $b8,$e9,$b8,$d0,$9d,$31,$d8,$bc
	DATA BYTE $ce,$01,$05,$a0,$fc,$1b,$f1,$7e
	DATA BYTE $0b,$cf,$44,$0d,$60,$bf,$bf,$f0
	DATA BYTE $ff,$38,$0a,$c7,$17,$1c,$00,$87
	DATA BYTE $af,$17,$d5,$37,$87,$c9,$d5,$c1
	DATA BYTE $ff,$d8,$00,$a8,$55,$1e,$1b,$a5
	DATA BYTE $30,$13,$61,$5b,$ff,$c1,$ba,$82
	DATA BYTE $91,$d0,$3c,$a8,$51,$e7,$14,$40
	DATA BYTE $ef,$50,$84,$f6,$20,$82,$85,$05
	DATA BYTE $9b,$c2,$cd,$3f,$0f,$cf,$82,$f5
	DATA BYTE $03,$63,$20,$05,$fe,$0d,$4b,$e8
	DATA BYTE $e6,$88,$96,$01,$03,$66,$82,$f8
	DATA BYTE $3f,$1f,$05,$a7,$ab,$80,$f7,$bf
	DATA BYTE $47,$5f,$92,$f2,$9f,$c5,$02,$a8
	DATA BYTE $10,$a5,$72,$a5,$68,$b5,$b8,$b1
	DATA BYTE $d8,$19,$7f,$25,$fe,$7f,$cd,$bf
	DATA BYTE $b6,$3e,$f5,$97,$39,$d7,$ac,$00
	DATA BYTE $d7,$10,$ef,$57,$05,$04,$98,$00
	DATA BYTE $05,$99,$e9,$2a,$e0,$10,$15,$a3
	DATA BYTE $c8,$60,$40,$00,$f8,$c0,$f7,$01
	DATA BYTE $f7,$02,$e2,$ff,$70,$dc,$07,$78
	DATA BYTE $78,$38,$38,$07,$ff,$17,$d3,$1e
	DATA BYTE $8b,$3e,$09,$11,$8f,$0e,$c4,$d6
	DATA BYTE $a0,$5e,$e3,$94,$2c,$be,$30,$5f
	DATA BYTE $37,$04,$f8,$43,$f4,$f3,$6a,$fd
	DATA BYTE $a8,$cd,$44,$14,$ef,$55,$d7,$60
	DATA BYTE $d9,$68,$1c,$88,$05,$4c,$26,$03
	DATA BYTE $09,$05,$1e,$90,$0d,$99,$9e,$c0
	DATA BYTE $ff,$11,$91,$ff,$27,$b3,$07,$f0
	DATA BYTE $d0,$8c,$2b,$c7,$f8,$09,$ea,$3f
	DATA BYTE $e3,$bd,$3e,$03,$05,$6f,$e0,$df
	DATA BYTE $c0,$e1,$c9,$97,$d8,$ef,$2c,$79
	DATA BYTE $93,$35,$75,$f3,$63,$82,$57,$8b
	DATA BYTE $0c,$8f,$aa,$75,$bc,$b1,$aa,$df
	DATA BYTE $85,$8a,$ee,$3e,$29,$f0,$31,$d9
	DATA BYTE $a7,$85,$06,$86,$84,$00,$09,$92
	DATA BYTE $c3,$00,$bf,$96,$00,$9d,$cc,$00
	DATA BYTE $08,$00,$d6,$ff,$a5,$08,$77,$84
	DATA BYTE $df,$a0,$20,$02,$7f,$c2,$04,$01
	DATA BYTE $f8,$54,$7f,$d7,$c6,$31,$8f,$0a
	DATA BYTE $fb,$fd,$bf,$d3,$22,$bf,$13,$7d
	DATA BYTE $be,$5d,$97,$63,$8f,$f4,$a7,$e7
	DATA BYTE $cd,$16,$a0,$51,$13,$98,$d5,$50
	DATA BYTE $0d,$a2,$54,$0a,$01,$94,$bf,$20
	DATA BYTE $10,$02,$08,$4c,$8e,$47,$a3,$11
	DATA BYTE $dd,$2c,$ae,$a4,$07,$87,$2d,$fd
	DATA BYTE $88,$c7,$f0,$01,$2a,$f1,$f3,$ff
	DATA BYTE $06,$ce,$fe,$1e,$0e,$0f,$d8,$00
	DATA BYTE $b3,$76,$1c,$71,$a3,$0f,$17,$1f
	DATA BYTE $b9,$31,$77,$ab,$f7,$39,$17,$ff
	DATA BYTE $e1,$00,$02,$17,$dd,$87,$19,$03
	DATA BYTE $ae,$5f,$d1,$d0,$f5,$4b,$8f,$18
	DATA BYTE $93,$75,$fb,$f7,$b1,$07,$83,$c1
	DATA BYTE $f5,$ba,$75,$e8,$50,$b0,$fb,$4f
	DATA BYTE $99,$b0,$07,$ba,$55,$c3,$89,$00
	DATA BYTE $c1,$e3,$03,$cf,$e2,$8f,$0c,$83
	DATA BYTE $20,$30,$00,$89,$d6,$b8,$fe,$03
	DATA BYTE $fe,$01,$80,$f8,$08,$78,$83,$87
	DATA BYTE $f9,$18,$a8,$38,$95,$11,$ef,$ff
	DATA BYTE $9e,$ff,$89,$01,$fa,$fd,$fe,$9a
	DATA BYTE $01,$e3,$cf,$64,$cd,$51,$25,$b3
	DATA BYTE $ad,$0a,$8d,$dc,$37,$79,$08,$d9
	DATA BYTE $28,$e0,$d3,$cd,$50,$20,$c5,$17
	DATA BYTE $1e,$bb,$e8,$0c,$e9,$4c,$10,$10
	DATA BYTE $e2,$0d,$1b,$ef,$9b,$b5,$5a,$e7
	DATA BYTE $e1,$8b,$79,$0f,$00,$60,$4f,$fc
	DATA BYTE $62,$fd,$11,$2b,$57,$db,$a9,$2b
	DATA BYTE $1f,$ef,$06,$55,$fa,$d5,$97,$c1
	DATA BYTE $3c,$87,$81,$34,$01,$89,$a8,$f4
	DATA BYTE $fd,$d1,$ba,$85,$20,$54,$95,$e8
	DATA BYTE $25,$03,$ab,$7f,$11,$ef,$5f,$af
	DATA BYTE $91,$a7,$5f,$8f,$75,$eb,$a9,$29
	DATA BYTE $d5,$91,$67,$c7,$2d,$27,$d5,$18
	DATA BYTE $00,$78,$3c,$91,$5a,$6a,$00,$81
	DATA BYTE $f8,$d8,$00,$a3,$58,$de,$9a,$6f
	DATA BYTE $29,$42,$1b,$bb,$70,$17,$70,$af
	DATA BYTE $f7,$9b,$76,$00,$15,$ef,$01,$ae
	DATA BYTE $47,$cf,$d7,$b7,$bf,$c7,$ef,$91
	DATA BYTE $bd,$0f,$00,$f5,$7f,$f5,$91,$47
	DATA BYTE $c0,$c1,$d9,$00,$f7,$8c,$0f,$d7
	DATA BYTE $fe,$df,$e5,$82,$72,$01,$eb,$15
	DATA BYTE $8a,$1f,$01,$05,$2a,$2c,$91,$03
	DATA BYTE $f5,$45,$ab,$e1,$8a,$95,$ae,$96
	DATA BYTE $97,$0b,$04,$75,$fa,$d4,$ae,$f4
	DATA BYTE $77,$fe,$f9,$a1,$e2,$9b,$25,$05
	DATA BYTE $fb,$a7,$5d,$14,$29,$9d,$70,$a9
	DATA BYTE $75,$d3,$a3,$e0,$d7,$2d,$55,$3c
	DATA BYTE $9d,$c8,$01,$a5,$19,$60,$00,$b0
	DATA BYTE $fd,$3b,$02,$00,$50,$87,$39,$9e
	DATA BYTE $38,$a8,$9b,$f8,$53,$df,$7c,$e6
	DATA BYTE $17,$70,$84,$00,$78,$f8,$f1,$50
	DATA BYTE $e1,$f2,$eb,$78,$a7,$0f,$54,$c0
	DATA BYTE $ff,$a2,$59,$fb,$31,$41,$02,$9f
	DATA BYTE $18,$08,$ed,$dc,$8b,$78,$f7,$94
	DATA BYTE $61,$cd,$6f,$04,$50,$f8,$18,$10
	DATA BYTE $9b,$68,$0d,$e4,$20,$a5,$eb,$dd
	DATA BYTE $6d,$2a,$53,$85,$a5,$81,$fd,$e8
	DATA BYTE $26,$2a,$65,$7f,$d1,$e6,$00,$f5
	DATA BYTE $83,$6a,$3e,$9b,$da,$79,$07,$8b
	DATA BYTE $17,$9d,$2c,$fb,$a3,$bc,$a5,$fc
	DATA BYTE $fd,$0b,$ae,$59,$0b,$ff,$52,$b7
	DATA BYTE $96,$5c,$d7,$6f,$1f,$df,$df,$91
	DATA BYTE $ec,$84,$70,$01,$7e,$7c,$3e,$a3
	DATA BYTE $c5,$58,$27,$c0,$78,$96,$b5,$a4
	DATA BYTE $44,$18,$ac,$9f,$4d,$47,$e5,$83
	DATA BYTE $01,$45,$88,$44,$82,$8f,$b7,$0a
	DATA BYTE $f1,$25,$7f,$88,$45,$05,$f7,$0d
	DATA BYTE $ec,$87,$aa,$50,$49,$8d,$08,$ef
	DATA BYTE $6d,$21,$20,$55,$97,$7f,$14,$07
	DATA BYTE $fe,$c9,$df,$9c,$45,$01,$54,$d3
	DATA BYTE $ff,$07,$91,$2b,$15,$da,$77,$06
	DATA BYTE $9b,$70,$01,$fb,$76,$d0,$bf,$aa
	DATA BYTE $af,$97,$f1,$0f,$80,$4e,$03,$f5
	DATA BYTE $51,$68,$a0,$91,$28,$8a,$fd,$45
	DATA BYTE $51,$80,$fd,$ad,$ec,$ee,$93,$1f
	DATA BYTE $96,$1f,$ae,$01,$c7,$df,$3d,$e4
	DATA BYTE $00,$89,$e4,$c5,$ec,$bd,$00,$f7
	DATA BYTE $0f,$d9,$2f,$26,$a2,$8a,$8d,$0e
	DATA BYTE $04,$06,$35,$1e,$09,$67,$40,$82
	DATA BYTE $93,$33,$b0,$0e,$88,$51,$ed,$2d
	DATA BYTE $42,$93,$68,$51,$05,$41,$db,$d3
	DATA BYTE $ba,$01,$1b,$b8,$01,$8b,$94,$8a
	DATA BYTE $93,$e9,$21,$81,$d3,$8f,$9d,$7d
	DATA BYTE $1d,$6f,$01,$d1,$0f,$05,$54,$01
	DATA BYTE $e7,$a8,$c4,$9f,$e2,$47,$2b,$5d
	DATA BYTE $1d,$f8,$9f,$3b,$77,$e9,$27,$85
	DATA BYTE $79,$98,$07,$01,$41,$aa,$c1,$c9
	DATA BYTE $e9,$07,$6f,$8b,$cc,$0f,$a8,$07
	DATA BYTE $eb,$41,$07,$be,$f3,$97,$2e,$8a
	DATA BYTE $56,$ee,$a4,$a7,$6b,$35,$b1,$5b
	DATA BYTE $24,$cb,$00,$dc,$6f,$fc,$c9,$96
	DATA BYTE $a8,$af,$7e,$25,$e1,$7a,$17,$8c
	DATA BYTE $06,$93,$61,$0b,$d5,$67,$e3,$77
	DATA BYTE $95,$cd,$01,$45,$9d,$1d,$a2,$05
	DATA BYTE $07,$d5,$75,$fb,$26,$a7,$c6,$51
	DATA BYTE $09,$ed,$77,$47,$74,$25,$f9,$68
	DATA BYTE $a1,$99,$11,$7b,$3b,$24,$d7,$f9
	DATA BYTE $18,$01,$01,$b0,$01,$c7,$cd,$51
	DATA BYTE $2a,$1b,$ab,$ef,$df,$b9,$f1,$ab
	DATA BYTE $1b,$dd,$44,$a6,$b9,$31,$e9,$4b
	DATA BYTE $c9,$7b,$0b,$63,$a7,$fc,$85,$33
	DATA BYTE $51,$8a,$61,$f7,$ec,$19,$91,$0c
	DATA BYTE $e9,$12,$15,$a2,$8f,$44,$15,$95
	DATA BYTE $2b,$20,$ab,$09,$f5,$fb,$af,$ae
	DATA BYTE $05,$ff,$0d,$33,$9f,$5a,$5f,$a0
	DATA BYTE $fc,$05,$6f,$57,$c1,$ff,$b4,$67
	DATA BYTE $f3,$bf,$6d,$2f,$8e,$30,$bd,$7f
	DATA BYTE $ff,$ff,$ff,$e0

image_color:
	DATA BYTE $65,$eb,$eb,$e1,$01,$80,$00,$91
	DATA BYTE $f6,$91,$91,$61,$6c,$e1,$00,$f1
	DATA BYTE $18,$f1,$71,$71,$06,$a5,$8d,$00
	DATA BYTE $e4,$ed,$ac,$11,$09,$27,$fa,$00
	DATA BYTE $1b,$4d,$c4,$1f,$fe,$8f,$00,$f1
	DATA BYTE $fe,$34,$00,$19,$17,$ed,$ed,$c5
	DATA BYTE $29,$5f,$2d,$03,$3e,$13,$fe,$00
	DATA BYTE $5c,$ed,$0e,$04,$fe,$fb,$38,$7f
	DATA BYTE $8c,$00,$f6,$f6,$04,$9e,$42,$c5
	DATA BYTE $4a,$00,$63,$e6,$65,$6a,$ad,$05
	DATA BYTE $00,$1b,$7a,$eb,$1d,$06,$b6,$71
	DATA BYTE $00,$5f,$07,$1c,$3d,$00,$c4,$1d
	DATA BYTE $00,$a5,$1f,$1d,$00,$a6,$a6,$05
	DATA BYTE $d8,$56,$39,$eb,$c0,$dd,$13,$ed
	DATA BYTE $e5,$53,$00,$43,$39,$00,$17,$51
	DATA BYTE $9d,$00,$3a,$06,$ce,$9a,$41,$81
	DATA BYTE $f8,$cf,$0b,$71,$3e,$00,$13,$83
	DATA BYTE $d6,$6a,$07,$5a,$93,$08,$96,$6b
	DATA BYTE $05,$09,$02,$9c,$06,$f8,$e8,$33
	DATA BYTE $1f,$5b,$00,$b7,$2e,$83,$ff,$54
	DATA BYTE $87,$1b,$0d,$74,$97,$1e,$14,$cc
	DATA BYTE $3d,$01,$ac,$00,$60,$66,$cd,$67
	DATA BYTE $c0,$96,$a8,$00,$10,$c7,$27,$0f
	DATA BYTE $e6,$03,$94,$e6,$ce,$00,$f7,$0d
	DATA BYTE $40,$fe,$c0,$f0,$42,$e6,$02,$61
	DATA BYTE $71,$61,$00,$82,$f9,$07,$e7,$00
	DATA BYTE $f1,$63,$09,$00,$ed,$01,$cb,$c1
	DATA BYTE $0c,$01,$c1,$43,$c4,$fc,$06,$51
	DATA BYTE $c1,$fc,$18,$03,$e4,$71,$fc,$1a
	DATA BYTE $06,$fe,$c1,$ea,$f0,$ef,$69,$0d
	DATA BYTE $6e,$0c,$78,$79,$05,$b6,$ce,$08
	DATA BYTE $df,$03,$61,$fd,$38,$00,$ea,$36
	DATA BYTE $f0,$0a,$3f,$06,$fa,$0c,$4e,$be
	DATA BYTE $08,$c5,$0f,$33,$00,$62,$c0,$e1
	DATA BYTE $e0,$f9,$f5,$0f,$97,$c1,$bc,$70
	DATA BYTE $e6,$de,$60,$cc,$70,$d7,$63,$59
	DATA BYTE $e6,$6e,$7a,$00,$db,$10,$40,$fc
	DATA BYTE $36,$b6,$eb,$00,$c7,$12,$a6,$d0
	DATA BYTE $00,$9d,$e0,$ee,$1c,$b1,$a6,$f1
	DATA BYTE $16,$04,$9b,$05,$77,$1c,$27,$06
	DATA BYTE $fb,$cc,$00,$f1,$01,$a0,$00,$c7
	DATA BYTE $db,$10,$87,$b9,$87,$99,$5b,$00
	DATA BYTE $6b,$6d,$02,$0b,$e2,$b3,$1b,$6c
	DATA BYTE $82,$c9,$97,$44,$b6,$00,$e6,$96
	DATA BYTE $9c,$4d,$32,$b6,$a5,$0f,$15,$b4
	DATA BYTE $19,$99,$2d,$8f,$fc,$3c,$10,$9f
	DATA BYTE $37,$fe,$e6,$00,$2e,$ab,$cf,$17
	DATA BYTE $fb,$14,$c2,$fb,$fb,$dd,$03,$ff
	DATA BYTE $ef,$2f,$03,$d6,$c6,$57,$a5,$1f
	DATA BYTE $e2,$81,$95,$c5,$eb,$a1,$f1,$6d
	DATA BYTE $20,$f1,$96,$e0,$ed,$41,$ea,$cd
	DATA BYTE $cb,$c1,$f0,$f6,$00,$c3,$97,$cd
	DATA BYTE $cb,$b6,$4c,$2b,$a6,$c1,$58,$fe
	DATA BYTE $39,$04,$f1,$f5,$0e,$59,$b5,$0e
	DATA BYTE $06,$f6,$0f,$d0,$09,$fd,$68,$81
	DATA BYTE $64,$61,$fe,$e9,$3a,$68,$00,$7a
	DATA BYTE $f6,$be,$00,$a0,$5f,$01,$f9,$da
	DATA BYTE $b1,$fd,$1b,$da,$da,$05,$f9,$22
	DATA BYTE $f0,$f6,$7c,$06,$ed,$d2,$00,$f3
	DATA BYTE $5f,$e1,$97,$d7,$02,$fb,$47,$95
	DATA BYTE $d1,$ed,$ac,$f2,$7e,$34,$c2,$dc
	DATA BYTE $14,$74,$7d,$e1,$d0,$fd,$20,$ca
	DATA BYTE $e8,$02,$9c,$1b,$e1,$90,$00,$f7
	DATA BYTE $62,$f5,$40,$ab,$ea,$b4,$18,$9a
	DATA BYTE $7e,$ef,$0d,$3c,$28,$9a,$7d,$02
	DATA BYTE $bc,$23,$90,$b7,$c5,$ea,$a4,$00
	DATA BYTE $24,$b1,$e0,$0d,$21,$87,$86,$23
	DATA BYTE $fc,$bb,$05,$08,$03,$f5,$c0,$fb
	DATA BYTE $bc,$13,$1b,$53,$f1,$a4,$d9,$01
	DATA BYTE $38,$e3,$00,$da,$c8,$00,$51,$f6
	DATA BYTE $da,$d7,$b8,$00,$9e,$7b,$07,$c4
	DATA BYTE $bf,$e5,$a5,$d1,$01,$53,$e6,$00
	DATA BYTE $83,$88,$b8,$a6,$ff,$c5,$f7,$8f
	DATA BYTE $cf,$a3,$d2,$b4,$ed,$ee,$8b,$43
	DATA BYTE $e4,$8b,$f0,$e7,$56,$71,$5d,$e3
	DATA BYTE $87,$6b,$35,$a4,$01,$81,$b2,$cd
	DATA BYTE $e7,$9a,$0e,$45,$27,$f4,$9e,$03
	DATA BYTE $72,$00,$ff,$7c,$bf,$06,$ba,$b1
	DATA BYTE $22,$00,$fa,$00,$a1,$b1,$09,$a3
	DATA BYTE $01,$07,$c1,$21,$63,$af,$c1,$86
	DATA BYTE $d0,$b9,$f6,$6d,$36,$f1,$fb,$ad
	DATA BYTE $98,$cd,$fe,$37,$fe,$ff,$0f,$fb
	DATA BYTE $00,$62,$92,$c0,$8f,$f2,$aa,$f4
	DATA BYTE $ee,$90,$e1,$43,$53,$e1,$c7,$e5
	DATA BYTE $c7,$e7,$f5,$03,$ff,$81,$fa,$c5
	DATA BYTE $cb,$f7,$c3,$a6,$8c,$03,$71,$c2
	DATA BYTE $af,$9b,$f6,$0b,$96,$d0,$00,$c4
	DATA BYTE $f6,$5c,$e6,$3f,$a9,$90,$b8,$45
	DATA BYTE $da,$81,$fd,$5e,$05,$00,$07,$e9
	DATA BYTE $8a,$b7,$e1,$e7,$b7,$d3,$e6,$3b
	DATA BYTE $13,$a0,$00,$f1,$95,$f8,$eb,$8b
	DATA BYTE $dd,$5e,$07,$08,$2d,$e3,$07,$c6
	DATA BYTE $91,$fe,$64,$e4,$00,$b8,$9b,$1b
	DATA BYTE $b7,$e6,$b4,$34,$b7,$9d,$c0,$f8
	DATA BYTE $77,$e6,$0f,$07,$db,$4f,$cd,$c5
	DATA BYTE $ed,$e9,$07,$9e,$18,$6a,$07,$71
	DATA BYTE $00,$81,$fc,$ff,$1c,$03,$c5,$c5
	DATA BYTE $84,$c0,$d6,$f2,$96,$66,$04,$f1
	DATA BYTE $f3,$a2,$c8,$bd,$ec,$e0,$d7,$eb
	DATA BYTE $d1,$c6,$b2,$73,$ed,$bd,$6d,$82
	DATA BYTE $56,$5f,$60,$04,$ed,$47,$94,$d9
	DATA BYTE $cb,$c8,$c9,$fe,$a3,$87,$a3,$fa
	DATA BYTE $c8,$fe,$3c,$b1,$f1,$df,$4b,$6f
	DATA BYTE $cb,$93,$d9,$93,$5c,$d6,$07,$a5
	DATA BYTE $e5,$b2,$f1,$16,$a5,$dc,$2e,$ef
	DATA BYTE $1b,$1c,$05,$ea,$21,$04,$a6,$47
	DATA BYTE $b1,$8d,$c1,$a4,$d3,$92,$4f,$22
	DATA BYTE $62,$91,$02,$bb,$07,$bf,$a3,$67
	DATA BYTE $c8,$86,$da,$d4,$0a,$d2,$e0,$f6
	DATA BYTE $8d,$13,$e6,$a6,$b9,$1b,$e6,$3a
	DATA BYTE $50,$96,$79,$16,$b6,$77,$01,$1a
	DATA BYTE $0f,$86,$9d,$1b,$05,$a9,$b1,$bb
	DATA BYTE $60,$05,$fd,$5b,$f4,$6d,$0c,$b8
	DATA BYTE $b5,$00,$67,$f4,$b1,$ec,$00,$ec
	DATA BYTE $e0,$f4,$83,$f9,$aa,$85,$00,$d6
	DATA BYTE $8b,$ee,$61,$90,$04,$eb,$da,$70
	DATA BYTE $85,$68,$ff,$f7,$02,$6c,$5c,$85
	DATA BYTE $0c,$f6,$a4,$fe,$05,$90,$00,$91
	DATA BYTE $8d,$65,$05,$77,$01,$bf,$db,$db
	DATA BYTE $9d,$05,$1c,$b1,$d7,$c0,$f8,$91
	DATA BYTE $d3,$09,$16,$47,$d6,$08,$a6,$ca
	DATA BYTE $03,$b6,$f1,$a1,$a1,$a6,$82,$fb
	DATA BYTE $05,$e9,$00,$05,$c1,$8a,$a3,$ea
	DATA BYTE $c1,$93,$b8,$37,$d8,$28,$01,$f9
	DATA BYTE $3d,$91,$f2,$61,$07,$ff,$cd,$df
	DATA BYTE $e8,$e7,$9d,$2f,$b1,$af,$e9,$39
	DATA BYTE $f1,$fa,$7d,$07,$67,$13,$aa,$75
	DATA BYTE $9e,$74,$c6,$48,$ca,$61,$65,$a5
	DATA BYTE $c8,$d6,$03,$cb,$78,$9f,$99,$d7
	DATA BYTE $3d,$06,$dd,$e1,$d9,$e6,$95,$bd
	DATA BYTE $d7,$ed,$84,$f4,$41,$d9,$be,$28
	DATA BYTE $d3,$d0,$81,$1f,$a6,$25,$87,$07
	DATA BYTE $8d,$bd,$c2,$db,$c1,$e1,$df,$d1
	DATA BYTE $a0,$bc,$bf,$5f,$06,$91,$0e,$ed
	DATA BYTE $03,$72,$00,$61,$da,$d1,$b8,$ed
	DATA BYTE $ab,$00,$59,$1c,$04,$b6,$8a,$fb
	DATA BYTE $01,$fe,$a1,$21,$0f,$8c,$0d,$e2
	DATA BYTE $54,$03,$c2,$b6,$81,$fc,$e6,$c1
	DATA BYTE $f6,$ab,$cd,$ef,$f7,$ff,$ff,$70
	DATA BYTE $17,$96,$f2,$03,$e1,$e2,$0d,$bb
	DATA BYTE $b2,$ab,$9f,$07,$fd,$83,$9d,$83
	DATA BYTE $b9,$c3,$a0,$7c,$71,$e7,$bb,$0f
	DATA BYTE $c6,$66,$10,$18,$35,$f2,$2b,$ec
	DATA BYTE $bf,$1f,$02,$d6,$21,$9a,$14,$41
	DATA BYTE $25,$51,$70,$51,$89,$86,$a5,$65
	DATA BYTE $64,$d6,$9d,$e8,$e6,$00,$c2,$8f
	DATA BYTE $71,$dd,$07,$80,$ab,$d4,$b4,$55
	DATA BYTE $f6,$35,$d1,$21,$9a,$7b,$67,$fb
	DATA BYTE $de,$66,$4b,$d5,$71,$93,$d5,$9b
	DATA BYTE $e9,$63,$f1,$6c,$a6,$f4,$5d,$07
	DATA BYTE $a2,$eb,$69,$0d,$7c,$1d,$f5,$db
	DATA BYTE $06,$b6,$9e,$7b,$0e,$88,$d8,$27
	DATA BYTE $13,$f6,$f0,$97,$77,$bd,$cd,$ed
	DATA BYTE $85,$e8,$01,$84,$d1,$41,$4f,$00
	DATA BYTE $e9,$62,$0d,$de,$9a,$d1,$51,$80
	DATA BYTE $b9,$e6,$fa,$ca,$be,$99,$39,$a0
	DATA BYTE $3a,$ff,$ef,$00,$11,$b1,$f1,$b0
	DATA BYTE $a8,$9e,$a6,$51,$73,$f0,$65,$c6
	DATA BYTE $e3,$f9,$e9,$3e,$c5,$05,$21,$00
	DATA BYTE $d9,$d8,$79,$03,$77,$5b,$13,$6d
	DATA BYTE $d3,$b3,$f6,$29,$69,$23,$a5,$ae
	DATA BYTE $f1,$06,$ea,$f7,$59,$e1,$5b,$35
	DATA BYTE $a7,$73,$03,$86,$a6,$f3,$00,$42
	DATA BYTE $ea,$bc,$06,$8c,$5e,$f8,$0f,$b7
	DATA BYTE $08,$38,$00,$c1,$f7,$0b,$cd,$83
	DATA BYTE $a0,$e0,$fb,$ee,$78,$08,$6d,$e4
	DATA BYTE $00,$b0,$b4,$f2,$c3,$68,$f2,$57
	DATA BYTE $19,$f5,$e0,$6b,$07,$d1,$64,$00
	DATA BYTE $80,$66,$ce,$f8,$e9,$1c,$a6,$51
	DATA BYTE $e7,$6c,$90,$3a,$c1,$f9,$f0,$87
	DATA BYTE $be,$c1,$ef,$4d,$db,$00,$9f,$ad
	DATA BYTE $b6,$00,$b7,$95,$e6,$c4,$fe,$9a
	DATA BYTE $00,$7e,$83,$5f,$57,$3e,$c1,$ff
	DATA BYTE $f3,$89,$da,$31,$c9,$df,$dc,$07
	DATA BYTE $98,$9c,$98,$2f,$61,$27,$27,$0d
	DATA BYTE $65,$d5,$f7,$fd,$a8,$dd,$82,$59
	DATA BYTE $71,$77,$05,$79,$94,$89,$eb,$a2
	DATA BYTE $0d,$d0,$ec,$0a,$9a,$0d,$27,$87
	DATA BYTE $41,$af,$53,$e4,$a6,$f0,$fb,$60
	DATA BYTE $d4,$68,$d8,$1e,$47,$d3,$69,$0a
	DATA BYTE $0d,$ff,$7a,$00,$0b,$c8,$71,$5a
	DATA BYTE $0f,$f9,$0d,$6e,$9f,$ed,$00,$82
	DATA BYTE $f6,$01,$d9,$02,$c1,$78,$27,$bf
	DATA BYTE $3b,$85,$d2,$b2,$ff,$6f,$9f,$00
	DATA BYTE $0f,$9f,$b1,$88,$e8,$bb,$0f,$b1
	DATA BYTE $b1,$63,$ce,$38,$f1,$34,$f7,$0f
	DATA BYTE $83,$f5,$bb,$04,$00,$ef,$0f,$3d
	DATA BYTE $8d,$fe,$06,$fa,$35,$00,$fd,$87
	DATA BYTE $b1,$e2,$de,$f8,$8c,$3d,$1f,$8c
	DATA BYTE $f4,$ff,$0d,$87,$c0,$61,$8a,$d6
	DATA BYTE $83,$5f,$01,$b6,$f6,$6d,$19,$e5
	DATA BYTE $9c,$f1,$5f,$2d,$90,$29,$dd,$36
	DATA BYTE $2e,$6d,$00,$45,$5b,$07,$71,$77
	DATA BYTE $6c,$34,$7d,$0b,$f6,$1f,$6f,$0f
	DATA BYTE $06,$fb,$c5,$96,$c9,$f3,$c1,$d8
	DATA BYTE $bb,$78,$00,$d6,$0c,$fe,$02,$d3
	DATA BYTE $82,$fb,$0b,$c0,$fb,$66,$fb,$00
	DATA BYTE $d1,$3f,$a6,$f9,$17,$f5,$2f,$00
	DATA BYTE $dc,$6a,$ef,$9b,$7a,$68,$d6,$00
	DATA BYTE $7a,$f2,$b7,$fc,$ff,$1a,$5f,$04
	DATA BYTE $05,$16,$ff,$27,$ff,$74,$46,$8b
	DATA BYTE $f8,$e6,$0b,$43,$02,$3f,$05,$bc
	DATA BYTE $00,$1c,$eb,$00,$79,$92,$89,$07
	DATA BYTE $fb,$7c,$fb,$07,$e1,$af,$f9,$09
	DATA BYTE $eb,$e2,$fe,$6e,$05,$f9,$01,$78
	DATA BYTE $04,$f5,$18,$00,$fb,$d3,$04,$4e
	DATA BYTE $05,$07,$e0,$fd,$a8,$35,$03,$f9
	DATA BYTE $37,$41,$af,$8f,$87,$7f,$3f,$00
	DATA BYTE $b4,$37,$fc,$2c,$e3,$0e,$f8,$7a
	DATA BYTE $bb,$59,$65,$d3,$f4,$84,$18,$43
	DATA BYTE $43,$f8,$01,$c4,$54,$54,$41,$fa
	DATA BYTE $66,$00,$3c,$7d,$03,$0d,$b1,$e9
	DATA BYTE $75,$ff,$fb,$1b,$00,$41,$51,$5b
	DATA BYTE $00,$29,$7c,$07,$be,$2c,$eb,$67
	DATA BYTE $de,$81,$78,$ec,$be,$f1,$0d,$0b
	DATA BYTE $ee,$c7,$0a,$f1,$96,$0f,$0c,$b8
	DATA BYTE $af,$6e,$e6,$03,$f8,$dd,$9a,$71
	DATA BYTE $ec,$8f,$3a,$15,$96,$2b,$6f,$a3
	DATA BYTE $f5,$18,$82,$88,$c4,$ef,$35,$fb
	DATA BYTE $00,$73,$37,$34,$0b,$0e,$f8,$d8
	DATA BYTE $35,$be,$68,$91,$1f,$a0,$00,$9b
	DATA BYTE $66,$75,$00,$cd,$1d,$51,$c0,$f7
	DATA BYTE $e5,$93,$dc,$ff,$0d,$64,$f5,$fb
	DATA BYTE $12,$43,$8d,$e3,$a7,$ef,$29,$8e
	DATA BYTE $b3,$af,$9f,$86,$87,$e4,$9b,$00
	DATA BYTE $79,$97,$f2,$9b,$75,$c1,$87,$83
	DATA BYTE $f2,$b1,$00,$fc,$fc,$00,$de,$1b
	DATA BYTE $ec,$ea,$81,$bc,$c4,$bf,$c1,$fa
	DATA BYTE $ae,$9a,$00,$02,$fa,$43,$01,$79
	DATA BYTE $75,$00,$83,$a7,$2f,$6c,$51,$01
	DATA BYTE $83,$ad,$9b,$f0,$e0,$fc,$de,$81
	DATA BYTE $19,$05,$f1,$f1,$8f,$db,$07,$cd
	DATA BYTE $f1,$5d,$08,$0b,$0f,$ec,$87,$b0
	DATA BYTE $81,$b6,$b6,$00,$d3,$0d,$97,$fb
	DATA BYTE $fb,$87,$7c,$16,$fe,$ff,$0d,$30
	DATA BYTE $2f,$98,$77,$70,$fb,$b5,$3f,$09
	DATA BYTE $41,$8f,$69,$71,$28,$04,$f8,$00
	DATA BYTE $e5,$ed,$00,$03,$fc,$9e,$6b,$51
	DATA BYTE $cf,$32,$fe,$93,$ff,$fe,$00,$fc
	DATA BYTE $23,$d6,$89,$56,$a6,$8e,$05,$b1
	DATA BYTE $fc,$0c,$8e,$ef,$47,$fd,$dc,$af
	DATA BYTE $fd,$ac,$16,$01,$fa,$0b,$27,$eb
	DATA BYTE $af,$f8,$8d,$70,$b1,$db,$41,$f8
	DATA BYTE $00,$f1,$de,$85,$7d,$70,$87,$5e
	DATA BYTE $41,$36,$ee,$06,$f9,$05,$fb,$05
	DATA BYTE $d1,$f4,$bd,$bf,$6c,$f1,$0c,$ea
	DATA BYTE $39,$15,$d9,$c4,$e4,$03,$f4,$6f
	DATA BYTE $fe,$f7,$00,$85,$fc,$f0,$85,$ec
	DATA BYTE $fd,$55,$b7,$5f,$67,$7f,$e0,$e3
	DATA BYTE $f8,$79,$bf,$db,$74,$95,$81,$3f
	DATA BYTE $1f,$87,$86,$ac,$71,$f4,$da,$0e
	DATA BYTE $01,$df,$cb,$00,$42,$d9,$9f,$7e
	DATA BYTE $17,$60,$74,$84,$a6,$65,$00,$c4
	DATA BYTE $2c,$df,$00,$43,$e9,$d9,$d8,$99
	DATA BYTE $7d,$a8,$8f,$d1,$41,$f9,$84,$75
	DATA BYTE $fb,$07,$53,$a3,$fb,$c2,$fa,$06
	DATA BYTE $00,$d7,$4b,$97,$c2,$fc,$d0,$a9
	DATA BYTE $70,$85,$d5,$a9,$00,$03,$0e,$a7
	DATA BYTE $c7,$84,$aa,$85,$ec,$c3,$95,$b1
	DATA BYTE $b9,$76,$06,$c9,$fb,$b8,$f5,$1f
	DATA BYTE $c7,$82,$f4,$3a,$f4,$0c,$a5,$00
	DATA BYTE $c0,$f8,$f1,$88,$5e,$51,$2d,$91
	DATA BYTE $21,$b7,$3e,$76,$51,$06,$fe,$13
	DATA BYTE $fd,$e1,$c9,$86,$dd,$3d,$c7,$b6
	DATA BYTE $99,$c5,$eb,$07,$ff,$40,$00,$ef
	DATA BYTE $a9,$d3,$00,$ba,$09,$00,$07,$ff
	DATA BYTE $ff,$ff,$ff,$c0
