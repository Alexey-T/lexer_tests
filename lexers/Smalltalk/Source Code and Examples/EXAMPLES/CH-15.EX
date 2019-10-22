 "Examples for Chapter 15, Printing Objects.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

Transcript cr; show: 'Hello' printString.
Transcript cr; show: 'Hello' displayString.

234 printString. 
Date today printString.
Time now printString.
Timestamp now printString.

(Date newDay: 34 year: 1996) printFormat: #(2 1 3 $/ 1 2)

| pc |
pc := PrintConverter for: #number withFormatString: '00000'.
(pc formatStringFor: 234) string.

| pc |
pc := PrintConverter for: #date withFormatString: 'dddd, mmmm d, yyyy'.
(pc formatStringFor: Date today) string.


| pc |
pc := PrintConverter for: #timestamp withFormatString: 'mmm d, hh:mm:ss.ffff'.
(pc formatStringFor: Timestamp now) string.

(NumberPrintPolicy print: 1234 using: '#,###') string.
(NumberPrintPolicy print: -1234 using: '#,###;(#,###)') string.
(TimestampPrintPolicy print: Date today using: 'yymmdd') string.
(TimestampPrintPolicy print: Time now using: 'mm:ss.ff') string.

PrintConverter print: 234 formattedBy: '#####.###'.

"VisualWorks 2.5 and above"
Date today longPrintString.
Date today shortPrintString.

('Hello <2s>.<n>There are <1p> <3?apples:oranges> in the basket' 
	expandMacrosWithArguments: #(4 'Alec' true)) asString.
