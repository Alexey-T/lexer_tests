LogToFile(TextToLog)
{
    global LogFileName  ; This global variable was previously given a value somewhere outside this function.
    FileAppend, %TextToLog%`n, %LogFileName%
}
LogToFile(TextToLog)
{
    static LoggedLines := 0
    LoggedLines += 1  ; Maintain a tally locally (its value is remembered between calls).
    global LogFileName
    FileAppend, %LoggedLines%: %TextToLog%`n, %LogFileName%
}
#1::
	a:=new test
	MsgBox, This msgbox will happen despite the EXIT.
return

#2::
	b:=test.__new()
    MsgBox, This msgbox will never happen because of the EXIT
return
#3::MsgBox, % "a.a: "  a.a " b.a: " b.a
class test{
	a:="a"
	__new(){
		;throw exception("e")
		exit
	}
}

$`:: ;backtick hotkey (you can remove the dollar sign)
SendInput, a
return

^`:: ;ctrl+backtick hotkey
SendInput, b
return

c:: ;send backtick
SendInput, ``
return
#EscapeChar
