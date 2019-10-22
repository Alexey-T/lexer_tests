 "Examples for Chapter 2, Messages. 
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

'elephant' copyFrom: 3 to: 5.

3.14 truncated negated.

| number negatedNumber string |
number := '42' asNumber.
negatedNumber := number negated.
string := negatedNumber printString.

| string |
string := ( ( '42' asNumber ) negated ) printString.

| string |
string := '42' asNumber negated printString.

1 + 2 * 3.
1 + (2 * 3).
2 + '4' asNumber max: 5.
30 max: 3 + 4 * 5.
30 max: 3 + (4 * 5).

