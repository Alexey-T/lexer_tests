/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   Attempt to "port" the Unix CUT command to Rexx                          */
/*   Rob van der Woude, May 16 1998 - January 4 2003                         */
/*   Usage:                                                                  */
/*   any_command | CUT { -C:n | -F:n [ -D:"any_string" [ -I ] ] } [ -L:n ]   */
/*   Note: The -D switch isn't functional yet, so neither is -I              */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


/* Specify maximum number of empty lines */
maxEmpty = 50

/* Initialize RexxUtil */
If RxFuncQuery( "sysloadfuncs" ) <> 0 Then Do
	Call RxFuncAdd "sysloadfuncs", "RexxUtil", "sysloadfuncs"
	Call sysloadfuncs
End

/* Parse and check command line */
Parse arg cmdline
cmdline = Translate( cmdline, '-', '/' )
cmdline = strip( cmdline )
Parse Upper Value cmdline With .'-C:'column .
Parse       Value cmdline With .'-d:"'delimiter'"'.
If delimiter = '' Then Parse Value cmdline With .'-D:"'delimiter'"'.
Parse Upper Value cmdline With .'-F:'field .
Parse Upper Value cmdline With .'-L:'length .
ipos = Pos( ' -I', Translate( cmdline ) )
If ipos = 0 Then Do
	case = ''
End
Else Do
	If ipos > LastPos( '"', cmdline ) | ipos < Pos( '"', cmdline ) Then Do
		case = 'Upper'
		delimiter = Translate( delimiter )
	End
	Else Do
		case = ''
	End
End
spos = Pos( ' -S', Translate( cmdline ) )
If spos = 0 Then Do
	skip = 0
End
Else Do
	If spos > LastPos( '"', cmdline ) | spos < Pos( '"', cmdline ) Then Do
		skip = 1
	End
	Else Do
		skip = 0
	End
End
Select
	When column <> '' & field <> '' Then Call Syntax
	When length <> '' & DataType( length, 'W' ) <> 1 Then Call Syntax
	When field  <> '' Then Do
		If DataType( field,  'W' ) <> 1 Then Call Syntax
		cuttype = 'WORD'
		cutpos  = field
	End
	When column <> '' Then Do
		If delimiter <> '' Then Call Syntax
		If DataType( column, 'W' ) <> 1 Then Call Syntax
		cuttype = 'CHAR'
		cutpos  = column
	End
	Otherwise Call Syntax
End
cutlen = 0
If length <> '' Then cutlen = length
If delimiter = '' Then delimiter = ' '

/* Read Standard Input */
empty = 0
Do i = 1 By 1 While Lines( ) > 0
	line.i = LineIn( "STDIN" )
	If line.i = "00"X Then Leave
	If line.i = "1A"X Then Leave
	If line.i = "" Then empty = empty + 1; Else empty = 0
	/* Stop after <maxEmpty> empty lines */
	If empty > maxEmpty Then Leave
End
/* Ignore those empty lines */
line.0 = i - empty

/* Cut lines as specified on command line */
cuttot  = cutpos + cutlen
Do i = 1 to line.0
	If line.i = "" Then Do
		msg = ""
		Call Output
		Iterate
	End
	If cuttype = "CHAR" Then Do
		linelen = Length( line.i )
		If linelen >= cutpos Then Do
			Select
				When cutlen = 0 Then Do
					msg = substr( line.i, cutpos )
					Call Output
				End
				When cutlen > 0 Then Do
					If linelen < cuttot Then Do
						msg = substr( line.i, cutpos )
						Call Output
					End
					Else Do
						msg = substr( line.i, cutpos, cutlen )
						Call Output
					End
				End
				Otherwise Call Syntax
			End
		End
		Else Do
			msg = ""
			Call Output
		End
	End
	If cuttype = "WORD" & delimiter = " " Then Do
		linelen = Words( line.i )
		If linelen >= cutpos Then Do
			Select
				When cutlen = 0 Then Do
					msg = SubWord( line.i, cutpos )
					Call Output
				End
				When cutlen > 0 Then Do
					If linelen < cuttot Then Do
						msg = SubWord( line.i, cutpos )
						Call Output
					End
					Else Do
						msg = SubWord( line.i, cutpos, cutlen )
						Call Output
					End
				End
				Otherwise Call Syntax
			End
		End
		Else Do
			msg = ""
			Call Output
		End
	End
	If cuttype = "WORD" & delimiter <> " " Then Do
		string = line.i
		dlen = Length( delimiter )
		Do j = 1 by 1 until string = ""
			interpret 'Parse '||case||' Value string With word.'||j||'"'||delimiter||'"string'
			word.0 = j
		End
		linelen = word.0
		line = word.cutpos
		If linelen > cutpos Then Do j = cutpos + 1 by 1 to linelen
			line = line||delimiter||word.j
		End
		If linelen >= cutpos Then Do
			Select
				When cutlen = 0 Then msg = line
				When cutlen > 0 Then Do
					If linelen < cuttot Then Do
						msg = line
					End
					Else Do
						dpos = 0
						Do j = 1 to cutlen
							dpos = Pos( delimiter, line, dpos + dlen )
						End
						msg = substr( line, 1, dpos )
					End
				End
				Otherwise Call Syntax
			End
			Call Output
		End
		Else Do
			msg = ""
			Call Output
		End
	End
End

/* Normal program end */
Exit 0


Output:
	If skip = 0 | strip( msg ) <> "" Then Say msg
Return


Syntax: procedure
	Call beep 220, 240
	Say
	Say "Cut.rex,  Version 0.50 beta for Regina Rexx"
	Say 'Attempt to "port" the Unix CUT command to Rexx'
	Say
	Say "Usage:  <any_command>  |  CUT  <options>"
	Say
	Say "Options:               Function:                                Dependency"
	Say "__________________________________________________________________________"
	Say
	Say "   -C:<column_number>  Parse by Columns or Characters"
/*
	Say '   -D:"<delimiter>"    Delimiter character or string             -F'
*/
	Say "   -F:<field_number>   Parse by Fields or words"
/*
	Say "   -I                  Case Insensitive delimiter (should        -D"
*/
	Say "                       be the first or last parameter)"
	Say "   -L:<string_length>  Number of characters to display           -C"
	Say "or -L:<fields>         Number of fields (words) to display       -F"
	Say
	Say "Examples:"
	Say
	Say "   ECHO 1234567890 | CUT -C:4"
	Say "   VER | DATE | REGINA CUT.REX -F:6"
	Say "   VER | DATE | REGINA CUT.REX -F:6 | REGINA CUT.REX -C:7"
	Say
	Say "Written by Rob van der Woude"
	Say "http://www.robvanderwoude.com"
	Exit 1
Return
