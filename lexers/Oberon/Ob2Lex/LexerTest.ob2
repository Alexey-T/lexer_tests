<* compiler pragma -- *> *> wing comments are allowed
   is a multiline thing *>
(* single line comment *)
(* multiline
   comment *)
(* multiline
   comment (*
   with nesting *) *)
(*(**) this is still a comment *)

*) - error: not in comment
*> - error: not in pragma
<* (* *) *) *> - superfluous comment end terminates pragma

<*<**> - error: "pragmas can't be nested"
<* pragma (* comments in pragmas are allowed (* even nested *) *)
   still pragma *>
<* this pragma mustn't be highlighted as a comment *>--
(* this comment mustn't be highlighted as a string *)''

-- <* (* this is a wing comment, not pragma or multiline comment
(* -- *) *) - error: not in comment
<* -- *> *> - wing comment in pragma *) *)
 still pragma *> pragma is terminated

Empty strings:
''
""
'this is a string constant'                              , and then some normal text
'this is a string constant with "double quotes" embedded', and then some normal text
"this is a string constant"                              , and then some normal text
"this is a string constant with 'single quotes' embedded", and then some normal text
error: 'string is not terminated"
error: "string is not terminated'
error: quote at EOL'
error: quote at EOL"
no wing comment in here: '--', just a string constant
no wing comment in here: "--", just a string constant
no comment in here: '(*', just a string constant
no comment in here: "(*", just a string constant
no pragma in here: '<*', just a string constant
no pragma in here: "<*", just a string constant

number: 0123456789
valid numbers:
1
1.
1.E4
1.E-4
1.E+4
1.0
1.0E4
1.0E-4
1.0E+4
1EH
invalid numbers:
1.E
1.E+
1.E-
invalid number+ident combinations:
1E
1EHX
1.EX
1E4
1E+4
1.0EH
1.0FX
1.0E+0AX
1.E+4%

0ABCDEFH - hexadecimal integer
1EX - character constant
0A1B2C3D4E5F6789X - character constant
 A1B2C3D4E5F6789X - identifier
0a1b2c3d4e5f6789X - invalid character constant
test lexing at EOF: 1.0E+4