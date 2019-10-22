/*
 * Harbour Project source code:
 *    RDD tests
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

REQUEST dbfcdx
#define _TESTRDD "dbfcdx"
#include "rddtst.prg"

FUNCTION test_main()

RDDTESTC {0,.t.,.t.,.f.}, LOCAL n
RDDTESTF "DBFCDX", {0,.t.,.t.,.f.}, RDDSETDEFAULT()
RDDTESTC {1,.t.,.t.,.f.}, USE "_tst" SHARED
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTOP()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBSKIP(0)
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTO(0)
RDDTESTF NIL, {1,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(0)
RDDTESTC {1,.t.,.f.,.f.}, SET DELETE ON
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTOP()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBSKIP(0)
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTO(0)
RDDTESTF NIL, {1,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(0)
RDDTESTC {1,.t.,.f.,.f.}, SET DELETE OFF
RDDTESTC {1,.t.,.t.,.f.}, INDEX on FNUM tag TG_N to "_tst"
RDDTESTC {1,.t.,.t.,.f.}, INDEX on FSTR tag TG_C to "_tst"
RDDTESTF "TG_C", {1,.t.,.t.,.f.}, ORDSETFOCUS()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTOP()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBSKIP(0)
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTO(0)
RDDTESTF NIL, {1,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(0)
RDDTESTC {1,.t.,.f.,.f.}, SET DELETE ON
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTOP()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBSKIP(0)
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTO(0)
RDDTESTF NIL, {1,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(0)
RDDTESTF NIL, {1,.t.,.t.,.f.}, DBGOTO(0)
RDDTESTF .f., {1,.t.,.t.,.f.}, DBSEEK("", .T.,.F.)
RDDTESTF .f., {1,.t.,.t.,.f.}, DBSEEK("", .T.,.T.)
RDDTESTF .f., {1,.t.,.t.,.f.}, DBSEEK("", .F.,.F.)
RDDTESTF .f., {1,.t.,.t.,.f.}, DBSEEK("", .F.,.T.)
RDDTESTC {1,.t.,.t.,.f.}, SET DELETE OFF
RDDTESTC {15,.f.,.f.,.f.}, for n:=1 to 15                    ; dbappend()                          ; replace FNUM with int((n+2)/3)      ; replace FSTR with chr(FNUM+48)      ; next
RDDTESTF NIL, {15,.f.,.f.,.f.}, dbcommit()
RDDTESTF NIL, {15,.f.,.f.,.f.}, dbunlock()
RDDTESTF "TG_C", {15,.f.,.f.,.f.}, ORDSETFOCUS(1)
RDDTESTF "TG_N", {15,.f.,.f.,.f.}, ORDSETFOCUS()
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(0,.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(0,.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(0.5,.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(0.5,.T.,.T.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK(1.0,.T.,.F.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK(1.0,.T.,.T.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK(2.0,.T.,.F.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK(2.0,.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(2.5,.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(2.5,.T.,.T.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK(5.0,.T.,.F.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK(5.0,.T.,.T.)
RDDTESTF "TG_N", {13,.f.,.f.,.t.}, ORDSETFOCUS(2)
RDDTESTF "TG_C", {13,.f.,.f.,.t.}, ORDSETFOCUS()
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK("", .T.,.F.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK("", .T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(" ",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(" ",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("0",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("0",.T.,.T.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK("1",.T.,.F.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK("1",.T.,.T.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK("2",.T.,.F.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK("2",.T.,.T.)
RDDTESTF .t., {7,.f.,.f.,.t.}, DBSEEK("3",.T.,.F.)
RDDTESTF .t., {7,.f.,.f.,.t.}, DBSEEK("3",.T.,.T.)
RDDTESTF .t., {10,.f.,.f.,.t.}, DBSEEK("4",.T.,.F.)
RDDTESTF .t., {10,.f.,.f.,.t.}, DBSEEK("4",.T.,.T.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK("5",.T.,.F.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK("5",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("6",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("6",.T.,.T.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK("", .T.,.F.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK("", .T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(" ",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(" ",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("0",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("0",.T.,.T.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK("1",.T.,.F.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK("1",.T.,.T.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK("2",.T.,.F.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK("2",.T.,.T.)
RDDTESTF .t., {7,.f.,.f.,.t.}, DBSEEK("3",.T.,.F.)
RDDTESTF .t., {7,.f.,.f.,.t.}, DBSEEK("3",.T.,.T.)
RDDTESTF .t., {10,.f.,.f.,.t.}, DBSEEK("4",.T.,.F.)
RDDTESTF .t., {10,.f.,.f.,.t.}, DBSEEK("4",.T.,.T.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK("5",.T.,.F.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK("5",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("6",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("6",.T.,.T.)
RDDTESTC {15,.f.,.f.,.f.}, INDEX on FSTR tag TG_C to "_tst" DESCEND
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(" ",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(" ",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("0",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("0",.T.,.T.)
RDDTESTF .t., {3,.f.,.f.,.t.}, DBSEEK("1",.T.,.F.)
RDDTESTF .t., {3,.f.,.f.,.t.}, DBSEEK("1",.T.,.T.)
RDDTESTF .t., {6,.f.,.f.,.t.}, DBSEEK("2",.T.,.F.)
RDDTESTF .t., {6,.f.,.f.,.t.}, DBSEEK("2",.T.,.T.)
RDDTESTF .t., {9,.f.,.f.,.t.}, DBSEEK("3",.T.,.F.)
RDDTESTF .t., {9,.f.,.f.,.t.}, DBSEEK("3",.T.,.T.)
RDDTESTF .t., {12,.f.,.f.,.t.}, DBSEEK("4",.T.,.F.)
RDDTESTF .t., {12,.f.,.f.,.t.}, DBSEEK("4",.T.,.T.)
RDDTESTF .t., {15,.f.,.f.,.t.}, DBSEEK("5",.T.,.F.)
RDDTESTF .t., {15,.f.,.f.,.t.}, DBSEEK("5",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("6",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK("6",.T.,.T.)
RDDTESTC {1,.f.,.f.,.f.}, INDEX on FSTR tag TG_C to "_tst"
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBSKIP(0)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(0)
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBSKIP(0)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(0)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(5)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBSKIP(5)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(5)
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {10,.f.,.f.,.f.}, DBSKIP(-5)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(10)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBSKIP(-5)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(16)
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {5,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {3,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBGOTO(6)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBGOTO(6)
RDDTESTF NIL, {5,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {8,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOTO(12)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOTO(12)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBGOTO(13)
RDDTESTF NIL, {14,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBGOTO(13)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {14,.f.,.f.,.f.}, DBGOTO(14)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(16)
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF "TG_C", {15,.f.,.f.,.f.}, ORDSETFOCUS(0)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-10)
RDDTESTC {7,.f.,.f.,.f.}, INDEX on FSTR tag TG_C to "_tst" FOR FNUM>2 .AND. FNUM<=4
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {7,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {7,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBGOTO(6)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBGOTO(6)
RDDTESTF NIL, {7,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {8,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {7,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOTO(12)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOTO(12)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBGOTO(13)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBGOTO(13)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {14,.f.,.f.,.f.}, DBGOTO(14)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(16)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTC {1,.f.,.f.,.f.}, INDEX on FSTR tag TG_C to "_tst" FOR FNUM!=2 .AND. FNUM<4
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {3,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {8,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {3,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {10,.f.,.f.,.f.}, DBGOTO(10)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {10,.f.,.f.,.f.}, DBGOTO(10)
RDDTESTF NIL, {9,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBGOTO(13)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBGOTO(13)
RDDTESTF NIL, {9,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {14,.f.,.f.,.f.}, DBGOTO(14)
RDDTESTF NIL, {9,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(16)
RDDTESTF NIL, {9,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {1,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {9,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {9,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {8,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {9,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTC {16,.t.,.t.,.f.}, INDEX on FSTR tag TG_C to "_tst" FOR FNUM==6
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTOP()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOBOTTOM()
RDDTESTC {15,.f.,.f.,.f.}, INDEX on FSTR tag TG_C to "_tst" FOR RECNO()!=5 DESCEND
RDDTESTF NIL, {5,.f.,.f.,.f.}, DBGOTO(5)
RDDTESTF NIL, {8,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {5,.f.,.f.,.f.}, DBGOTO(5)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTC {1,.f.,.f.,.f.}, INDEX on FSTR tag TG_C to "_tst" FOR RECNO()!=5
RDDTESTF NIL, {5,.f.,.f.,.f.}, DBGOTO(5)
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {5,.f.,.f.,.f.}, DBGOTO(5)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF "TG_C", {16,.f.,.t.,.f.}, ORDSETFOCUS(0)
RDDTESTC {16,.f.,.t.,.f.}, SET DELETE ON
RDDTESTF .t., {16,.t.,.t.,.f.}, FLOCK()
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {3,.f.,.f.,.f.}, DBGOTO(3)
RDDTESTF NIL, {3,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBGOTO(6)
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBGOTO(13)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {14,.f.,.f.,.f.}, DBGOTO(14)
RDDTESTF NIL, {14,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBGOTO(15)
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(16)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBDELETE()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBCOMMIT()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBUNLOCK()
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-10)
RDDTESTF NIL, {10,.f.,.f.,.f.}, DBSKIP(5)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBSKIP(-5)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBSKIP(6)
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-7)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(8)
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-20)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(10)
RDDTESTF NIL, {8,.f.,.f.,.f.}, DBSKIP(-5)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(5)
RDDTESTF NIL, {5,.f.,.f.,.f.}, DBSKIP(-6)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(7)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBSKIP(-8)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(20)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-20)
RDDTESTF .t., {2,.t.,.f.,.f.}, FLOCK()
RDDTESTC {16,.f.,.t.,.f.}, DELETE ALL
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBUNLOCK()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTOP()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {16,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {16,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {16,.t.,.f.,.f.}, DBSKIP(0)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(0)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {16,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF .t., {7,.f.,.f.,.f.}, DBRLOCK()
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBRECALL()
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBUNLOCK()
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {7,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBGOTO(11)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBGOTO(11)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTC {16,.f.,.t.,.f.}, SET DELETE OFF
RDDTESTF .t., {16,.t.,.t.,.f.}, FLOCK()
RDDTESTC {16,.f.,.t.,.f.}, RECALL ALL
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBUNLOCK()
RDDTESTC {16,.f.,.t.,.f.}, SET DELETE ON
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBCOMMIT()
RDDTESTF "", {15,.f.,.f.,.f.}, ORDSETFOCUS(1)
RDDTESTC {15,.f.,.f.,.f.}, SET DELETE ON
RDDTESTF .t., {15,.f.,.f.,.f.}, FLOCK()
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTO(1)
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {3,.f.,.f.,.f.}, DBGOTO(3)
RDDTESTF NIL, {3,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBGOTO(6)
RDDTESTF NIL, {6,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBGOTO(13)
RDDTESTF NIL, {13,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {14,.f.,.f.,.f.}, DBGOTO(14)
RDDTESTF NIL, {14,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBGOTO(15)
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBDELETE()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(16)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBDELETE()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBCOMMIT()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBUNLOCK()
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-10)
RDDTESTF NIL, {10,.f.,.f.,.f.}, DBSKIP(5)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBSKIP(-5)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBSKIP(6)
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-7)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(8)
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-20)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(10)
RDDTESTF NIL, {8,.f.,.f.,.f.}, DBSKIP(-5)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(5)
RDDTESTF NIL, {5,.f.,.f.,.f.}, DBSKIP(-6)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(7)
RDDTESTF NIL, {2,.f.,.f.,.f.}, DBSKIP(-8)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(20)
RDDTESTF NIL, {12,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {2,.t.,.f.,.f.}, DBSKIP(-20)
RDDTESTF .t., {2,.t.,.f.,.f.}, FLOCK()
RDDTESTC {16,.f.,.t.,.f.}, DELETE ALL
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBCOMMIT()
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBUNLOCK()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTOP()
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF NIL, {16,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {16,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {16,.t.,.f.,.f.}, DBSKIP(0)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(0)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {16,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {16,.t.,.t.,.f.}, DBGOTO(0)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBGOTO(7)
RDDTESTF .t., {7,.f.,.f.,.f.}, DBRLOCK()
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBRECALL()
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBUNLOCK()
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {7,.t.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {4,.f.,.f.,.f.}, DBGOTO(4)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(1)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBGOTO(11)
RDDTESTF NIL, {7,.f.,.f.,.f.}, DBSKIP(-1)
RDDTESTF NIL, {11,.f.,.f.,.f.}, DBGOTO(11)
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBSKIP(1)
RDDTESTC {16,.f.,.t.,.f.}, SET DELETE OFF
RDDTESTF .t., {16,.t.,.t.,.f.}, FLOCK()
RDDTESTC {16,.f.,.t.,.f.}, RECALL ALL
RDDTESTF NIL, {16,.f.,.t.,.f.}, DBUNLOCK()
RDDTESTC {16,.f.,.t.,.f.}, SET DELETE ON
RDDTESTF NIL, {1,.f.,.f.,.f.}, DBGOTOP()
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBGOBOTTOM()
RDDTESTF NIL, {15,.f.,.f.,.f.}, DBCOMMIT()
RDDTESTC {1,.f.,.f.,.f.}, INDEX on FSTR tag TG_C to "_tst"
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr(" ",10)+" ",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr(" ",10)+" ",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr("0",10)+" ",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr("0",10)+" ",.T.,.T.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK(padr("1",10)+" ",.T.,.F.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK(padr("1",10)+" ",.T.,.T.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK(padr("2",10)+" ",.T.,.F.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK(padr("2",10)+" ",.T.,.T.)
RDDTESTF .t., {7,.f.,.f.,.t.}, DBSEEK(padr("3",10)+" ",.T.,.F.)
RDDTESTF .t., {7,.f.,.f.,.t.}, DBSEEK(padr("3",10)+" ",.T.,.T.)
RDDTESTF .t., {10,.f.,.f.,.t.}, DBSEEK(padr("4",10)+" ",.T.,.F.)
RDDTESTF .t., {10,.f.,.f.,.t.}, DBSEEK(padr("4",10)+" ",.T.,.T.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK(padr("5",10)+" ",.T.,.F.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK(padr("5",10)+" ",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr("6",10)+" ",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr("6",10)+" ",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr(" ",10)+"*",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr(" ",10)+"*",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr("0",10)+"*",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr("0",10)+"*",.T.,.T.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK(padr("1",10)+"*",.T.,.F.)
RDDTESTF .t., {1,.f.,.f.,.t.}, DBSEEK(padr("1",10)+"*",.T.,.T.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK(padr("2",10)+"*",.T.,.F.)
RDDTESTF .t., {4,.f.,.f.,.t.}, DBSEEK(padr("2",10)+"*",.T.,.T.)
RDDTESTF .t., {7,.f.,.f.,.t.}, DBSEEK(padr("3",10)+"*",.T.,.F.)
RDDTESTF .t., {7,.f.,.f.,.t.}, DBSEEK(padr("3",10)+"*",.T.,.T.)
RDDTESTF .t., {10,.f.,.f.,.t.}, DBSEEK(padr("4",10)+"*",.T.,.F.)
RDDTESTF .t., {10,.f.,.f.,.t.}, DBSEEK(padr("4",10)+"*",.T.,.T.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK(padr("5",10)+"*",.T.,.F.)
RDDTESTF .t., {13,.f.,.f.,.t.}, DBSEEK(padr("5",10)+"*",.T.,.T.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr("6",10)+"*",.T.,.F.)
RDDTESTF .f., {16,.f.,.t.,.f.}, DBSEEK(padr("6",10)+"*",.T.,.T.)

RETURN NIL
