/***********************************************************
Programa car.p

intuito do jogo eh ultrapassar os carros

Por Jaison Antoniazzi

Em 25 de fevereiro de 2001

Qualquer duvida falar comigo atraves do email 
jaison@neostep.com.br
jaison.a@zipmail.com.br
katolllzi@zipmail.com.br

Alteracoes - Por Jaison Antoniazzi
12/10/2002 
Incluido OCX Timer, melhorado a performance e feito alteracoes 
para diminuir a velocidade em caso de batida.

***********************************************************/

DEF VAR i AS WIDGET-HANDLE NO-UNDO.
DEF VAR w AS WIDGET-HANDLE NO-UNDO.
DEF VAR f AS WIDGET-HANDLE NO-UNDO.
DEF VAR t LIKE f NO-UNDO.
DEF VAR c LIKE t.

DEF VAR tra LIKE f EXTENT 1. /* arvores */
DEF VAR trb LIKE f EXTENT 1. /* arvores */
DEF VAR trc LIKE f EXTENT 2. /* arvores */
DEF VAR plane LIKE trc. 

DEF VAR en LIKE f EXTENT 8.
DEF VAR num-en-car AS i INIT 8.
DEF VAR ia AS i NO-UNDO.
DEF VAR imarcha LIKE i. /* imagem da marcha */
DEF VAR turbo LIKE i. /* imagem do turbo */
DEF VAR lturbo AS LOG NO-UNDO INIT NO.
DEF VAR velocidade AS INT INIT 1 NO-UNDO.
DEF VAR n AS INT INIT 49 NO-UNDO.
DEF VAR tvel LIKE f.
DEF VAR vidas LIKE f EXTENT 7.
DEF VAR icolisao LIKE ia.
DEF VAR imcolisao LIKE f.
DEF VAR icarros LIKE f.
DEF VAR pontos LIKE f.
Def Var byjaison Like f.
Def Var CtrlFrame As Widget-handle No-undo.
Def Var chCtrlFrame As Component-handle No-undo.

&scoped-define titulo Marginal Tiete by Jaison Antoniazzi
&scoped-define pausado Jogo Paralizado - Aperte P para continuar

CREATE WIDGET-POOL.

CREATE WINDOW w
    ASSIGN
        STATUS-AREA = NO
        MESSAGE-AREA = NO
        WIDTH = 150
        HEIGHT = 26
        TITLE = '{&pausado}'
        ROW = 1
        SENSITIVE = YES
        VISIBLE = YES.

CREATE FRAME f
    ASSIGN
        WIDTH = w:WIDTH
        HEIGHT = w:HEIGHT
        PARENT = w
        THREE-D = YES
        BGCOLOR = 1
        BOX = NO
        SENSITIVE = YES
        VISIBLE = YES.

CREATE Rectangle t
    ASSIGN
        ROW = 10.5
        COL = 1
        WIDTH = f:width - 1
        Height = 0.1
        FGCOLOR = 15
        FRAME = f:HANDLE
        SENSITIVE = YES
        VISIBLE = YES.

CREATE Rectangle c
    ASSIGN
        ROW = 17
        COL = 1        
        WIDTH = f:width - 1
        Height = 0.1
        FGCOLOR = 15
        FRAME = f:HANDLE
        SENSITIVE = YES
        VISIBLE = YES.

CREATE TEXT pontos
    ASSIGN
        FORMAT = 'x(8)'
        ROW = 1.3
        COL = 2
        FGCOLOR = 15
        SCREEN-VALUE = 'Pontos'
        FRAME = f:HANDLE
        SENSITIVE = YES
        VISIBLE = YES.

CREATE TEXT icarros
    ASSIGN
        ROW = 1.3
        COL = 10
        FORMAT = 'xxxxxxxxxxxxxx'
        FGCOLOR = 15
        WIDTH = 25
        SCREEN-VALUE = '0'
        FRAME = f:HANDLE
        SENSITIVE = YES
        VISIBLE = YES.

CREATE TEXT byjaison
    ASSIGN
        FORMAT = 'x(30)'
        ROW = 25
        COL = 110
        FGCOLOR = 15
        Auto-resize = True
        SCREEN-VALUE = 'by Jaison Antoniazzi'
        Font = 6
        FRAME = f:HANDLE
        SENSITIVE = YES
        VISIBLE = YES.

REPEAT ia = 1 TO 2:
    CREATE IMAGE plane[ia]
        ASSIGN 
            WIDTH-PIXELS    = 35
            HEIGHT-PIXELS   = 30
            FRAME   = f:HANDLE
            Y = RANDOM(10,100)
            X = RANDOM(30,190)
            &IF PROVERSION >= '10' &THEN
                TRANSPARENT = YES
            &ENDIF
            SENSITIVE   = YES
            VISIBLE     = YES.
END.


CREATE IMAGE i
    ASSIGN 
        WIDTH-PIXELS    = 35
        HEIGHT-PIXELS   = 17
        FRAME       = f:HANDLE
        Y = 256
        X = 34
        &IF PROVERSION >= '10' &THEN
            &IF PROVERSION >= '10' &THEN
            TRANSPARENT = YES
        &ENDIF
        &ENDIF
        SENSITIVE   = YES
        VISIBLE     = YES.

CREATE IMAGE imcolisao
    ASSIGN 
        WIDTH-PIXELS    = 35
        HEIGHT-PIXELS   = 17
        FRAME       = f:HANDLE
        &IF PROVERSION >= '10' &THEN
            TRANSPARENT = YES
        &ENDIF
        SENSITIVE   = YES
        VISIBLE     = NO.

REPEAT ia = 1 TO 7:
    CREATE IMAGE vidas[ia]
        ASSIGN 
            WIDTH-PIXELS    = 35
            HEIGHT-PIXELS   = 17
            FRAME       = f:HANDLE
            Y = 5.5
            X = 300 + (37 * (ia))
            &IF PROVERSION >= '10' &THEN
                TRANSPARENT = YES
            &ENDIF
            SENSITIVE   = YES
            VISIBLE     = YES.
END.

CREATE IMAGE turbo
    ASSIGN 
        WIDTH-PIXELS    = 157
        HEIGHT-PIXELS   = 56
        FRAME       = f:HANDLE
        Y = 370
        X = 200
        &IF PROVERSION >= '10' &THEN
            TRANSPARENT = YES
        &ENDIF
        SENSITIVE   = YES
        VISIBLE     = YES.

CREATE IMAGE imarcha
    ASSIGN 
        WIDTH-PIXELS    = 157
        HEIGHT-PIXELS   = 158
        FRAME   = f:HANDLE
        Y = 370
        X = 20
        &IF PROVERSION >= '10' &THEN
            TRANSPARENT = YES
        &ENDIF
        SENSITIVE   = YES
        VISIBLE     = YES.

REPEAT ia = 1 TO num-en-car:
    CREATE IMAGE en[ia]
    ASSIGN 
        WIDTH-PIXELS = 35
        HEIGHT-PIXELS = 17
        FRAME = f:HANDLE
        Y = RANDOM(208,322)
        X = RANDOM(400,700)
        &IF PROVERSION >= '10' &THEN
            TRANSPARENT = YES
        &ENDIF
        SENSITIVE = YES
        VISIBLE = YES.
END.

CREATE TEXT tvel
    ASSIGN
        FRAME = f:HANDLE
        FGCOLOR = 15
        FORMAT = 'Km/h xxxxxxxxxxxxx'
        SCREEN-VALUE = '10'
        X = 225
        Y = 5.5
        SENSITIVE = YES
        VISIBLE = YES.

REPEAT ia = 1 TO 1:
    CREATE IMAGE tra[ia]
    ASSIGN 
        WIDTH-PIXELS = 35
        HEIGHT-PIXELS = 30
        FRAME = f:HANDLE
        Y = 150
        X = RANDOM(50,600)
        SELECTABLE = YES
        MOVABLE = YES
        &IF PROVERSION >= '10' &THEN
            TRANSPARENT = YES
        &ENDIF
        SENSITIVE = YES
        VISIBLE = YES.
END.

REPEAT ia = 1 TO 1:
    CREATE IMAGE trb[ia]
    ASSIGN 
        WIDTH-PIXELS = 35
        HEIGHT-PIXELS = 30
        FRAME = f:HANDLE
        Y = 150
        X = RANDOM(50,700)
        SELECTABLE = YES
        MOVABLE = YES
        &IF PROVERSION >= '10' &THEN
            TRANSPARENT = YES
        &ENDIF
        SENSITIVE = YES
        VISIBLE = YES.
END.

REPEAT ia = 1 TO 2:
    CREATE IMAGE trc[ia]
    ASSIGN 
        WIDTH-PIXELS = 35
        HEIGHT-PIXELS = 30
        FRAME = f:HANDLE
        Y = 360
        X = RANDOM(50,700)
        SELECTABLE = YES
        MOVABLE = YES
        &IF PROVERSION >= '10' &THEN
            TRANSPARENT = YES
        &ENDIF
        SENSITIVE = YES
        VISIBLE = YES.
END.

Create Control-frame CtrlFrame 
Assign
   Frame           = f:HANDLE
   Row             = 3.38
   Column          = 14
   Height          = 4.76
   Width           = 20
   Hidden          = yes
   Sensitive       = yes.

CtrlFrame:NAME = "CtrlFrame":U .

ON 'T','t' ANYWHERE DO:
    IF  chCtrlFrame:PSTimer:enabled THEN DO:
        ASSIGN
            lturbo = NOT lturbo.
        
        CASE LASTKEY:
            WHEN 84 Or 
            When 116 THEN DO:
                {turbo.i}
            END.
        END CASE.
    
        {veloc.i n}
    END.
END.

ON '1','2','3','4','5','6' ANYWHERE DO:
    IF  chCtrlFrame:PSTimer:enabled THEN DO:
        ASSIGN 
            n = LASTKEY.

        {veloc.i n}
    END.
END.

ON 'cursor-up':U ANYWHERE DO: 
    IF  i:Y > 208 AND chCtrlFrame:PSTimer:enabled THEN
        ASSIGN 
            i:Y = i:Y - 5.
END.

ON 'cursor-down':U ANYWHERE DO:
    IF  i:Y < 320 AND chCtrlFrame:PSTimer:enabled THEN
        ASSIGN 
            i:Y = i:Y + 5.
END.

ON 'p':U ANYWHERE DO:
    Assign 
        chCtrlFrame:PSTimer:enabled = Not chCtrlFrame:PSTimer:Enabled.
    If  Not chCtrlFrame:PSTimer:enabled Then
        w:title = '{&pausado}'.
    Else
        w:title = '{&titulo}'.
END.

REPEAT ia = 1 TO num-en-car:
    en[ia]:LOAD-IMAGE('en.bmp').
END.


REPEAT ia = 1 TO 2:
    plane[ia]:LOAD-IMAGE('plane.bmp').
END.

REPEAT ia = 1 TO 1:
    tra[ia]:LOAD-IMAGE('tree.bmp').
END.

REPEAT ia = 1 TO 1:
    trb[ia]:LOAD-IMAGE('tree2.bmp').
END.

REPEAT ia = 1 TO 2:
    trc[ia]:LOAD-IMAGE('tree3.bmp').
END.

ASSIGN 
    CURRENT-WINDOW = w.

REPEAT ia = 1 TO 7:
    vidas[ia]:LOAD-IMAGE('car.bmp').
END.

i:LOAD-IMAGE('car.bmp').
turbo:LOAD-IMAGE('turbooff.bmp').
imarcha:LOAD-IMAGE('1marcha.bmp').
imcolisao:LOAD-IMAGE('colisao.bmp').

Run Control_load.

APPLY 'entry' TO w.

&IF PROVERSION <= '9.0' &THEN
    MESSAGE 'O seu Progress eh versao' + string (Proversion) + ' abaixo do necessario para executar corretamente' SKIP 
            'Algumas funcionalidades do jogo poderao nao funcionar corretamente.' VIEW-AS ALERT-BOX INFORMATION TITLE 'Progress antigo'.
&ENDIF

MESSAGE 'LEIA ATENTAMENTE ANTES DE INICIAR' Skip
        'Utilize as teclas 1,2,3,4,5,6 para trocar marcha.' SKIP
        'As setas PARA CIMA e PARA BAIXO movimentam o carro.' SKIP
        'A letra T liga ou desliga o Turbo, adicao de 50 km/h.' SKIP 
        'A letra P pausa ou tira a pausa do jogo.' SKIP(2)
        'Bom divertimento.' Skip(2)
        '{&titulo}' Skip
        'jaison@neostep.com.br' Skip
        'jaison.a@zipmail.com.br' Skip
        'katolllzi@zipmail.com.br'

    VIEW-AS ALERT-BOX TITLE 'COMO JOGAR'.

PROCEDURE control_load :

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "car.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? Then DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U).

END.
ELSE 
    Message 
        "car.wrx":U SKIP(1)
        "Arquivo WRX nao foi localizado."
    VIEW-AS ALERT-BOX TITLE "OCX nao carregado".

&ENDIF

Assign 
    chCtrlFrame:PSTimer:enabled = False
    chCtrlFrame:PSTimer:Interval = 10.

END PROCEDURE.

PROCEDURE CtrlFrame.PSTimer.Tick .
    
{moven.i 1 en}
{moven.i 2 en}
{moven.i 3 en}
{moven.i 4 en}
{moven.i 5 en}
{moven.i 6 en}
{moven.i 7 en}
{moven.i 8 en}
{movplane.i 1 plane}
{movplane.i 2 plane}

{movtra.i 1 tra} 

{movtrb.i 1 trb}

{movtrc.i 1 trc}        
{movtrc.i 2 trc}  
{vermov.i en[1]}
{vermov.i en[2]}
{vermov.i en[3]}
{vermov.i en[4]}
{vermov.i en[5]}
{vermov.i en[6]}
{vermov.i en[7]}
{vermov.i en[8]}

END PROCEDURE.

WAIT-FOR Window-close OF w.

If  Valid-handle(f) Then
    Delete Object f.
If  Valid-handle(w) Then
    Delete Object w.

DELETE WIDGET-POOL.
