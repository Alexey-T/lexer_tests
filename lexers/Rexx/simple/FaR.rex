/* FaR, Find and Replace */

/* Check command line arguments */
Parse Arg string0 string1 string2 caseoff dummy
If string1 = "" Then Call Syntax
If dummy  <> "" Then Call Syntax
If caseoff = "" Then Do
	case = 1
End
Else Do
	If Translate( caseoff ) = "-I" Then Do
		case     = 0
		ustring0 = Translate( string0 )
		ustring1 = Translate( string1 )
	End
	Else Do
		Call Syntax
	End
End
len1  = Length( string1 )
len2  = Length( string2 )

/* Search and replace */
found = 1
start = 1
Do Forever
	If case = 1 Then Do
		found = Pos( string1, string0, start )
	End
	Else Do
		found = Pos( ustring1, ustring0, start )
	End
	If found = 0 Then Leave
	string0  = SubStr( string0, 1, found - 1 )||string2||SubStr( string0, found + len1 )
	start    = Max( 1, start + len2 )
	If case  = 0 Then ustring0 = Translate( string0 )
End

/* Display the end result */
Say string0

/* Normal program termination */
Exit 0

Syntax:
	Say
	Say "FaR -- Find and Replace,  Version 1.01"
	Say "Search a string for a substring and replace it with another substring"
	Say
	Say 'Usage:  FAR.REX  "string0"  "string1"  [ "string2"  [ -I ] ]'
	Say
	Say 'Where:  "string0" is the string to be searched'
	Say '        "string1" is the substring to search for'
	Say '        "string2" is the substring to replace string1 (default: empty)'
	Say "        -I        makes the search case insensitive   (requires string2)"
	Say
	Say "Written by Rob van der Woude"
	Say "http://www.robvanderwoude.com"
	Exit 1
Return
