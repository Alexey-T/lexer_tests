SET PROCEDURE TO lib.prg
SET LIBRARY TO com.ffl

PROCEDURE myProc()
	foo = 0
endproc

function myFync()
	foo = 0
    DEFINE POPUP myMenu
    DEFINE	BAR 1 OF myMenu PROMPT 'Команда меню'
endfunc

DEFINE CLASS myClass AS form
	ADD OBJECT myCtrl as Label
	PROCEDURE Init()
    endproc 
ENDDEFINE