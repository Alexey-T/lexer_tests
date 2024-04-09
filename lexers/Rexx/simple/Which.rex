/* WHICH, Version 1.00 for Windows    */
/* Rexx "port" of UNIX' WHICH command */
/* that also looks for DLL's          */
/* Written by Rob van der Woude       */

/* Initialize RexxUtil */
If RxFuncQuery( "SysLoadFuncs" ) <> 0 Then Do
	Call RxFuncAdd "SysLoadFuncs", "RexxUtil", "SysLoadFuncs"
	Call SysLoadFuncs
End

/* Display empty line */
Say

/* Parse command line parameters */
Parse Upper Arg params
invalid.0 = 7
invalid.1 = "*"
invalid.2 = "?"
invalid.3 = "/"
invalid.4 = "\"
invalid.5 = ","
invalid.6 = ";"
invalid.7 = ":"
Do i = 1 To invalid.0
	If Pos( invalid.i, params ) > 0 Then Call Syntax
End
Parse Value params With dummy1'"'longfilename'"'dummy2
If longfilename <> "" Then Do
	If dummy1||dummy2 <> "" Then Call Syntax
	filename = longfilename
	longname = 1
End
Else Do
	Parse Value params With filename dummy
	If filename <> "" Then Do
		If dummy <> "" Then Call Syntax
		longname = 0
	End
	Else Do
		Call Syntax
	End
End

/* Check if extension was given */
pathext = Translate( Value( "PATHEXT", , "ENVIRONMENT" ) )
Do i = 1 By 1 Until pathext = ""
	Parse Value pathext With commandext.i";"pathext
	If commandext.i <> "" Then commandext.0 = i
End
i = commandext.0 + 1
commandext.i = ".DLL"
i = commandext.0 + 2
commandext.i = ".SYS"
i = commandext.0 + 3
commandext.i = ".OCX"
commandext.0 = i
addext       = 1
chk4ext      = Right( filename, 4 )
Do i = 1 To commandext.0
	If chk4ext = commandext.i Then addext = 0
End

/* Read PATH from environment */
path = Translate( Value( "PATH", , "ENVIRONMENT" ) )
path.  = ""
path.0 = 1
/* Add current directory as first PATH entry */
path.1 = Strip( Directory( ), "T", "\" )||"\"
Do i = 2 By 1 Until path = ""
	Parse Value path With path.i";"path
	If Right( path.i, 1 ) <> "\" Then path.i = path.i||"\"
	path.0 = i
End

/* Check if file can be found in PATH */
Do i = 1 To path.0 Until found.0 = 1
	If addext = 0 Then Do
		file = path.i||filename
		If longname = 1 Then Do
			file = Translate( file, "?", " " )
		End
		Call SysFileTree file, "found.", "FO"
		If found.0 = 1 Then Do
			If longname = 0 Then Do
				Call FileFound file
			End
			Else Do
				file = Translate( Translate( file, " ", "?" ) )
				If file = Translate( found.1 ) Then Do
					Call FileFound file
				End
			End
		End
	End
	Else Do j = 1 To commandext.0 - 3
		file = path.i||filename||commandext.j
		If longname = 1 Then Do
			file = Translate( file, "?", " " )
		End
		Call SysFileTree file, "found.", "FO"
		If found.0 = 1 Then Do
			If longname = 0 Then Do
				Call FileFound file
			End
			Else Do
				file = Translate( Translate( file, " ", "?" ) )
				If file = Translate( found.1 ) Then Do
					Call FileFound file
				End
			End
		End
	End
End
Say "Not found: "||filename
Exit 1


FileFound:
	If longname = 1 Then Do
		Say 'Found: "'||arg( 1 )||'"'
	End
	Else Do
		Say "Found: "||arg( 1 )
	End
	Exit 0
Return

Syntax: Procedure
	Say
	Say "Which.rex, Version 1.00 for Regina Rexx for Windows"
	Say "UNIX-like WHICH utility for Windows that can search for"
	Say ".DLL, .OCX and .SYS files as well"
	Say
	Say "Usage:    WHICH  program_name"
	Say
	Say 'Where:    "program_name"  is any program name with or without extension,'
	Say "                          but always without drive and/or path."
	Say
	Say "Returns:  The first file with a valid or specified extension found in the PATH."
	Say
	Say "If program_name is specified without extension, all extensions from the PATHEXT"
	Say "environment variable will be searched for."
	Say "Files with .DLL, .OCX or .SYS extension will be ignored unless the extension is"
	Say "specified."
	Say "Spaces in long file names are allowed, wildcards aren't."
	Say
	Say "Written by Rob van der Woude"
	Say "http://www.robvanderwoude.com"
	Exit 9
Return
