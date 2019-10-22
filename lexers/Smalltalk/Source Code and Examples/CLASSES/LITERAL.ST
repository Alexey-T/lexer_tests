Object subclass: #LiteralModifier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-LiteralModification'!
LiteralModifier comment:
'I demonstrate the danger of modifying literal arrays. I do this by modifying an element of a literal array, and by creating a writestream on a literal array.

If the examples are each run twice, the non-corrected method will give strange results the second time, even though the code looks like it should behave correctly. The corrected methods will give the correct results when they are run twice. They achieve this by copying the literal array before modifying it.

The moral of the story is that it''s safer to return copies of literal arrays from methods.
'!


!LiteralModifier methodsFor: 'examples'!

methodOne
	| literalArray |
	Transcript cr; show: thisContext printString.
	literalArray := #(1 2 3 4 5).
	Transcript cr; show: literalArray printString.
	(literalArray at: 3) == 3
		ifTrue: [literalArray at: 3 put: 99]
		ifFalse: [Transcript cr; show: 'On no. It is already 99']!

methodOneCorrected
	| literalArray |
	Transcript cr; show: thisContext printString.
	literalArray := #(1 2 3 4 5) copy.
	Transcript cr; show: literalArray printString.
	(literalArray at: 3) == 3
		ifTrue: [literalArray at: 3 put: 99]
		ifFalse: [Transcript cr; show: 'On no. It is already 99']!

methodTwo
	| stream |
	Transcript cr; show: thisContext printString.
	stream := WriteStream with: #(1 2 3 4 5).
	Transcript cr; show: stream contents printString.
	stream nextPutAll: #(6 7 8 9).
	Transcript cr; show: stream contents printString!

methodTwoCorrected
	| stream |
	Transcript cr; show: thisContext printString.
	stream := WriteStream with: #(1 2 3 4 5) copy.
	Transcript cr; show: stream contents printString.
	stream nextPutAll: #(6 7 8 9).
	Transcript cr; show: stream contents printString! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

LiteralModifier class
	instanceVariableNames: ''!


!LiteralModifier class methodsFor: 'examples'!

example
	"self example"

	self new
		methodOne;
		methodOne;
		methodOneCorrected;
		methodOneCorrected;
		methodTwo;
		methodTwo;
		methodTwoCorrected;
		methodTwoCorrected! !

