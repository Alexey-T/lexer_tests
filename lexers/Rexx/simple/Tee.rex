/* TEE port */

/* Parse and check command line arguments */
Parse Upper Arg ."/D:"maxwait .
If maxwait = "" Then Do
	Parse Arg outfile dummy
	maxwait = 1
End
Else Do
	Parse Arg .":"maxwait outfile dummy
End
If outfile = "" Then Call Syntax
If dummy  <> "" Then Call Syntax
If maxwait = "" Then Call Syntax
If DataType( maxwait, "W" ) = 0 Then Call Syntax
If maxwait <  1 Then Call Syntax

/* Initialize variables */
stopflag = 0
counter  = 0

/* Initialize RexxUtil */
If RxFuncQuery( "SysLoadFuncs" ) <> 0 Then Do
	Call RxFuncAdd "SysLoadFuncs", "RexxUtil", "SysLoadFuncs"
End
Call SysLoadFuncs

/* Read, display and redirect standard input */
Do Until stopflag = 1
	If lines( STDIN ) = 0 Then Do
		counter = counter + 1
		If counter > maxwait Then stopflag = 1
		Call SysSleep 1
	End
	Else Do
		Parse Pull line
		Say line
		Call LineOut outfile, line
	End
End

/* Normal program termination */
Exit 0


Syntax:
	Say
	Say "Tee.rex,  Version 1.00 for (Regina) Rexx"
	Say "Port of Unix' TEE command"
	Say "Redirects its input to the console and to a file simultaneously"
	Say
	Say "Usage:  any_command  |  <REXX>  TEE.REX  [/D:nn]  output_file"
	Say
	Say 'Where:  "any_command" is the command whose output you want to redirect'
	Say '        "output_file" is the file where any_command'||"'s output is redirected to"
	Say '        "nn"          is the max idle time (in seconds) allowed for any_command'
	Say '        "<REXX>"      is your Rexx interpreter:'
	Say "                    - Windows:  REGINA.EXE with RexxUtil"
	Say "                    - OS/2:     no need to specify, just rename script to *.cmd"
	Say
	Say "Written by Rob van der Woude"
	Say "http://www.robvanderwoude.com"
	Say
	Exit 1
Return
