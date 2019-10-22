 "Examples for Chapter 8, Control Structures.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

3 < 4 ifTrue: [Transcript cr; show: 'True'].

3 < 4
	ifTrue: [Transcript cr; show: 'True']
	ifFalse: [Transcript cr; show: 'False'].

| count |
count := 0.
[Transcript cr; show: count printString.
count := count + 1.
count < 10] whileTrue.

[Dialog confirm: 'Continue'] 
	whileTrue:
		[Transcript cr; show: 'Continuing'].

[(Dialog confirm: 'Continue') ifFalse: [^self].
Transcript cr; show: 'Continuing'] repeat.

5 timesRepeat: [Transcript cr; show: 'hello'].

1 to: 5 do: [ :index | Transcript cr; show: index printString].

15 to: 1 by: -2 do: [ :index | Transcript cr; show: index printString].


