 "Examples for Chapter 6, Special Variables.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

Transcript cr; show: thisContext printString.

#(1.1 $a 'hi') inspect.
#(1 $a #(1.1 'hi' #(2 #symbol))) inspect.

3 == 3.
$a == $a.
#abc == #abc.
true == true.
nil == nil.
'abc' == 'abc'.
3.14 == 3.14.
#(1 $a) == #(1 $a).

| x y |
x := #(1 $a).
y := x.
x == y.

'abc' = 'abc'.
3.14 = 3.14.
#(1 $a) = #(1 $a).

