 "Examples for Chapter 22, Common Errors.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

"This will raise an exception"
| collection |
collection:= OrderedCollection withAll: #(1 2 3 4 5 6).
collection do: [ :each | each even ifTrue: [collection remove: each]].

"This will work"
| collection |
collection:= OrderedCollection withAll: #(1 2 3 4 5 6).
collection copy do: [ :each | each even ifTrue: [collection remove: each]].
collection inspect.

(ExternalStream classPool at: #OpenStreams) copy 
	do: [ :each | each name = 'somePath' ifTrue: [each close]].

Integer withAllSubclasses 
	inject: OrderedCollection new
	into: [:coll :each | coll addAll: each allInstances; yourself]

#(3 $x 'hello' #mySymbol) 
	inject: String new writeStream 
	into: [ :stream :each | 
		stream print: each class; nextPutAll: ' value '; print: each; cr.
		stream]

