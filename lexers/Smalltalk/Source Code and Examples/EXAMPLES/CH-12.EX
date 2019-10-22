 "Examples for Chapter 12, Strings.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

string := 'here is a string'.
string := String with: $X.	
string := String new: 5 withAll: Character space.

'here is a string' size.

'now ', 'is ', 'the'

'Why' copyWith: $?.	
'Why' , (String with: $?).

'abcde' = 'ABCDE'.
'abcde' sameAs: 'ABCDE'.

'abcde' < 'ABCDF'.
'ABCDE' < 'abcdf'.

'abcdef' sameCharacters: 'abcxyz'.

'HELLO' asLowercase.
'there' asUppercase.

'54' asNumber.
'3.14' asNumber.
'abcd' asNumber.
'a12c' asNumber.

'Hello *' match: 'Hello Alec'.
'Hello #ave' match: 'HELLO Dave'.
'Hello #ave' match: 'Hello Alec'.

'HELLO' match: 'hello' ignoreCase: true.
'HELLO' match: 'hello' ignoreCase: false.

'Now is the time for all' isEmpty.
'Now is the time for all' includes: $f.
'Now is the time for all' contains: [ :each | each asUppercase == $F].
'Now is the time for all' occurrencesOf: $t.

'Now is the time for all' indexOf: $t.
'Now is the time for all' lastIndexOf: $t.
'Now is the time for all' findFirst: [ :char | char isLowercase].
'Now is the time for all' findLast: [ :char | char isSeparator].

'Now is the time for all' findString: 'time' startingAt: 4.
'Now is the time for all' 
	findString: 'T#ME' 
	startingAt: 1 
	ignoreCase: true 
	useWildcards: true.

'now is the time' changeFrom: 12 to: 15 with: 'tune'.
'now is the time' changeFrom: 12 to: 11 with: 'right '.
'now is the time' changeFrom: 1 to: 0 with: 'right '.
'now is the time' changeFrom: 16 to: 15 with: ' for'.
'now is the time' changeFrom: 8 to: 11 with: ''.
'now is the time' changeFrom: 8 to: 10 with: 'a'.
'now is the time' changeFrom: 8 to: 10 with: 'a great'.

'now is not the time' copyReplaceAll: 'no' with: 'yes'.

| string |
string := 'now is the time'.
string at: 1 put: $N.
string.

'now is the time' replaceAll: $t with: $T.
'now is the time' replaceAll: $t with: $T from: 1 to: 10.
'now is the time' replaceFrom: 12 to: 15 with: 'tune'.

'now is the time' copyUpTo: $m.

'now is the time' copyFrom: 5 to: 10.

| sortedSentence |
sortedSentence :=
	'now is the time for all good men to come' asArrayOfSubstrings asSortedCollection 
		inject: String new 
		into: [ :string :word | string , word, (String with: Character space)].
sortedSentence inspect.

"VisualWorks 2.5 and above only"
| count text |
count := 1.
text := 'There <1#is:are> <1p> apple<1#:s> in the box' expandMacrosWith: count.
text string inspect.

"VisualWorks 2.5 and above only"
| text |
text := 'Variables are:<nt>Var1=<1p><nt>Var2=<2p>' 
	expandMacrosWithArguments: #(11 22). 
text string inspect.
