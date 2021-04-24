run_Editor()
{
    if ( IsWinVis(WinDesc) )
    {
        WinActivate, %WinDesc%
    }
    else if IsWinVis(WinDesc1)
    {
        WinActivate, %WinDesc%
    }
    else if WinExist(WinDesc)
    {
        WinRestore, %WinDesc%
        ; WinShow, %WinDesc%
        ; WinWait, %WinDesc%
        WinActivate, %WinDesc%
    }
    else
        Run("%PROGRAM_Editor%",,,,,1)
}