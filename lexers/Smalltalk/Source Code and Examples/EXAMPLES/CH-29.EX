 "Examples for Chapter 29, Meta-Programming
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

Object allSubclasses size.
Smalltalk classNames size.

OrderedCollection selectors size + OrderedCollection class selectors size.

Object allSubclasses select:
	[:each | each organization categories includes: #'(additions)'].

Object allSubclasses select:
	[:each | each isMeta and: [each organization categories includes: #'(additions)']].

| collection |
collection := OrderedCollection new.
Smalltalk allBehaviorsDo: 
	[:each | (each organization categories includes: #'(additions)')
		ifTrue: [collection add: each]].
collection inspect.

Browser allClassMethodsInProtocol: #'(additions)'.
Browser browseAllClassMethodsInProtocol: #'(additions)'.

"You'll need to write browseAllMethodsInProtocol: before this will work"
Browser browseAllMethodsInProtocol: #'(additions)'.

