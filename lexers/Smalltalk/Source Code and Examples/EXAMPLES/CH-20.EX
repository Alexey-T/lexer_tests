 "Examples for Chapter 20, Error Handling.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

ArithmeticValue divisionByZeroSignal
	handle: [:ex | Transcript cr; show: ex errorString]
	do: [ 4 / 0 ].

ArithmeticValue domainErrorSignal
	handle: [:ex | Transcript cr; show: ex errorString]
	do: [ 4 / 0 ].

ArithmeticValue errorSignal
	handle: [:ex | Transcript cr; show: ex errorString]
	do: [ 4 / 0 ].

Object errorSignal
	handle: [:ex | Transcript cr; show: ex errorString]
	do: [ 4 / 0 ].

| parent1 parent2 child2 |
parent1 := Object errorSignal newSignal notifierString: 'Error1:'.
parent2 := Object errorSignal newSignal notifierString: 'Error2:'.
child2 := parent2 newSignal.
parent1 
	handle: [:ex | Transcript cr; show: 'in handler']
	do: [child2 raise]

| parent1 parent2 child2 |
parent1 := Object errorSignal newSignal notifierString: 'Error1:'.
parent2 := Object errorSignal newSignal notifierString: 'Error2:'.
child2 := parent2 newSignal.
parent2
	handle: [:ex | Transcript cr; show: 'in handler']
	do: [child2 raise]

| sigCollection |
sigCollection := SignalCollection 
	with: Dictionary keyNotFoundSignal
	with: Dictionary valueNotFoundSignal.
sigCollection 
	handle: [ :ex | Transcript cr; show: ex errorString]
	do: [Dictionary new at: 3].

| sigCollection |
sigCollection := SignalCollection 
	with: Dictionary keyNotFoundSignal
	with: Dictionary valueNotFoundSignal.
sigCollection 
	handle: [ :ex | Transcript cr; show: ex errorString]
	do: [Dictionary new keyAtValue: 4].

"VisualWorks 2.0"
Object errorSignal 
	raiseWith: #(3 'm:\temp') 
	errorTemplate: 'Received bad value % and bad value %.'

"VisualWorks 2.5"
Object errorSignal 
	raiseWith: #(3 'm:\temp') 
	errorPattern: 'Received bad value <1p> and bad value <2s>.'

Transcript cr; show:
	(self errorSignal
		handle: [:ex | ex return]
		do: [self errorSignal raise]) printString.

Transcript cr; show:
	(self errorSignal
		handle: [:ex | ex returnWith: 3]
		do: [self errorSignal raise]) printString.

self errorSignal
	handle: [:ex | 
		Transcript cr; show: ex errorString.
		ex reject]
	do: [self errorSignal raiseWith: 3 errorString: 'Error '].

self errorSignal
	handle: [:ex |
		Dialog warn: 'You divided by zero'.
		ex restart]
	do: [| answer |
		answer := Dialog request: 'Divide 4 by what?' initialAnswer: '0'.
		Transcript cr; show: (4 / answer asNumber) printString].



