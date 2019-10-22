Local $xmlPath = @ScriptDir & '\data.xml'
If Not FileExists($xmlPath) Then Exit
$file = FileRead($xmlPath)
$file = StringReplace($file, @LF, '')
$file = StringSplit($file, @CR, 1)

MsgBox(0, 'DONE!!!', XMLget($file, 'adc_database\currentconditions\realfeel'))
MsgBox(0, 'DONE!!!', XMLget($file, 'adc_database\planets\sun'))
MsgBox(0, 'DONE!!!', XMLget($file, 'adc_database\forecast\day number="1"\daytime\txtlong'))
MsgBox(0, 'DONE!!!', XMLget($file, 'adc_database\currentconditions\pressure'))

Func XMLget($file, $Path); XMLget($file,'adc_database\currentconditions\realfeel')
    $Path = StringSplit($Path, '/\|', 0)
    $lastline = 0
    For $lvl = 1 To $Path[0] Step 1
        For $line = $lastline To $file[0] Step 1
            $lastline = $line
            $hstart = StringInStr($file[$line], '<' & $Path[$lvl] & '>', 0)
            $hstarta = StringInStr($file[$line], '<' & $Path[$lvl] & ' ', 0)
            If $hstart Or $hstarta Then
                If $lvl == $Path[0] Then
                    If $hstart Then
                        $end = StringInStr($file[$line], '</' & $Path[$lvl] & '>', 0)
                        If $end Then
                            $hstart = $hstart + StringLen('<' & $Path[$lvl] & '>')
                            Return StringMid($file[$line], $hstart, $end - $hstart)
                        EndIf
                    EndIf
                    If $hstarta Then
                        $end = StringInStr($file[$line], '/>', 0)
                        If $end Then
                            $hstarta = $hstarta + StringLen('<' & $Path[$lvl] & ' ')
                            $return = StringMid($file[$line], $hstarta, $end - $hstarta)
                            Return $return
                        EndIf
                        $ends = StringInStr($file[$line], '>', 0)
                        If $ends Then
                            $hstart = $ends + 1
                            $end = StringInStr($file[$line], '</' & $Path[$lvl] & '>', 0)
                            If $end Then
                                $return = StringMid($file[$line], $hstart, $end - $hstart)
                                Return $return
                            EndIf
                        EndIf
                    EndIf
                EndIf
                ContinueLoop 2
            EndIf
        Next
        If $line == $file[0] Then ExitLoop
    Next
    Return 'not found'
EndFunc ;==>XMLget
