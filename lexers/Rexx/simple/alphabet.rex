/* Create an alphabet for HTML files */

/* Display blank line */
Say

/* Initialize RexxUtil library */
if RxFuncAdd( 'sysloadfuncs', 'RexxUtil', 'sysloadfuncs' ) = 0 then do
	call sysloadfuncs
end
else do
	call Syntax "Could not initialize RexxUtil."
end

/* Check command line parameter */
Parse Arg html dummy .
If dummy <> "" Then Call Syntax "Please specify only 1 HTML file."
If html  =  "" Then Call Syntax "Please specify an HTML file."
If Pos( "?", html ) > 0 Then Call Syntax

/* Check if file exists */
Call SysFileTree html, files.
If files.0 = 1 Then Do
	Parse Value files.1 with . . size . html
	html = Strip( html )
End
Else Do
	Call Syntax "Specified file not found."
End

/* Check extension of specified file */
lmth = Reverse( html )
Parse Upper Value lmth With txe"."eman"\".
If Right( txe, 3 ) <> "MTH" Then Call Syntax "Please specify HTML files only."

/* Define output file name */
alph = Reverse( eman )||".SRC"

/* Check if file exists */
Call SysFileTree alph, files.
If files.0 <> 0 Then Call Syntax "Output file already exists."

/* Create output file */
Call LineOut alph, ""

/* Check if file exists */
Call SysFileTree alph, files.
If files.0 <> 1 Then Call Syntax "Error creating output file."

/* Read HTML file into 1 single string */
strHTML = CharIn( html, 1, size )

/* Create table header */
Call LineOut alph, '<TABLE BORDER="0" WIDTH="100%">'
Call LineOut alph, '<TR>'

/* Create entries for numbers and for each letter of the alpabet */
Call Entry "0", "0..9"
Call Separator
Do i = 65 By 1 To 89
	Call Entry D2C( i )
	Call Separator
End
Call Entry "Z"

/* Close table */
Call LineOut alph, '</TR>'
Call LineOut alph, '</TABLE>'

/* Close output file */
Call LineOut alph

/* Done */
EXIT 0


Entry:
	Parse Arg link, descr
	If descr = "" Then descr = link
	strChk = '<A NAME="'||link||'">'
	resChk = Pos( strChk, strHTML )
	if resChk = 0 Then Do
		Call LineOut alph, '    <TD><FONT COLOR="Gray">'||link||'</FONT></TD>'
	End
	Else Do
		Call LineOut alph, '    <TD><A HREF="#'||link||'">'||descr||'</A></TD>'
	End
Return


Separator:
	Call LineOut alph, '    <TD><FONT SIZE="+6">|</FONT></TD>'
Return


Syntax:
	Parse Arg error
	If error <> "" Then Do
		Say error
		Say
	End
	Say "Alphabet.rex,  Version 1.01"
	Say "Create an HTML table to navigate a specified HTML file alphabetically"
	Say
	Say "Usage:    <REXX>  ALPHABET.REX  html_file"
	Say
	Say 'Where:    "<REXX>" is your Rexx interpreter:'
	Say "                   - Windows:  REGINA.EXE with RexxUtil installed"
	Say "                               (RexxUtil is not compatible with REXX.EXE)"
	Say "                   - OS/2:     no need to specify, just rename script to *.cmd"
	say
	Say "Output:   HTML table code in a file with name of HTML file and"
	Say '          extension ".SRC", located in current directory.'
	say
	Say "Written by Rob van der Woude"
	Say "http://www.robvanderwoude.com"

	EXIT 1
Return
