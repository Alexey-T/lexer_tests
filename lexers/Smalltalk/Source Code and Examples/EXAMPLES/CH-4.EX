 "Examples for Chapter 4, Variables.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

(Date newDay: 115 year: 1960) monthName. 
Date today monthName.	
Date today daysInMonth.

| block |
block := [ :nameString |Transcript cr; show: 'The name is ', nameString ].
block value: 'Alec'.

| block |
block := [ :nameString :age | 
		Transcript cr; show: 'The age of ', nameString, ' is ', age printString].
block value: 'Dave' value: 12.

[ :parm1 :parm2 :parm3 :parm4 :parm5 | Transcript cr; show: parm3 printString ] 
	valueWithArguments: #(99 88 77 66 55).

#(5 3 1) do: [ :each | Transcript cr; show: each printString].
#(5 3 1)  collect: [ :each | each * 30].
#(5 3 1)  inject: 0 into: [ :subtotal :each | subtotal + each ].

1 to: 5 do: [ :index | Transcript cr; show: index printString ].
'now is the time' do: [ :char | Transcript cr; show: char printString].

#('cat' 'dog' 'gerbil') 
		collect: [ :pet | #('milk' 'water' 'oj') 
			collect: [ :drink | pet -> drink]]

| a b c d |
a := b := c := d := OrderedCollection new.
a add: 1.
b add: 2.
c add: 3.
d inspect.

| a b c d |
a := b := c := d := OrderedCollection new.
a add: 1.
b := 2.
c add: 3.
b inspect.
d inspect.
