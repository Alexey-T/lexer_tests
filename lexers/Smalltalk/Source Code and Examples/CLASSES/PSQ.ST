SharedQueue subclass: #PrioritySharedQueue
	instanceVariableNames: 'defaultPriority '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-PrioritySharedQueue'!
PrioritySharedQueue comment:
'I am a subclass of SharedQueue. I store elements in priority order, and chronologically within a priority.

Instance Variables:

defaultPriority		<Integer>	The default priority for elements. This allows me to be used as a regular
								SharedQueue by using nextPut:.
'!


!PrioritySharedQueue methodsFor: 'accessing'!

defaultPriority
	^defaultPriority isNil
		ifTrue: [defaultPriority := 0]
		ifFalse: [defaultPriority]!

defaultPriority: aPriority
	defaultPriority := aPriority!

next
	"Get the next entry and return the value part since it's an 
	association	(or nil if there is no next entry)"

	^super next value!

nextPut: anObject 
	^self nextPut: anObject priority: self defaultPriority!

nextPut: anObject priority: aPriority 
	[accessProtect critical: 
		[ |foundElement |
		foundElement := contents 
				reverseDetect: [:element | aPriority <= element key]
				ifNone: [nil].
		foundElement isNil
			ifTrue: [contents addFirst: (aPriority -> anObject)]
			ifFalse: [contents add: (aPriority -> anObject) after: foundElement]]
	] valueUninterruptably.
	readSynch signal.
	^anObject!

peek
	"See what the next item is (without removing it) and return the value 
	part since it's an association (or nil if no next item)"

	^super peek value.! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

PrioritySharedQueue class
	instanceVariableNames: ''!


!PrioritySharedQueue class methodsFor: 'testing'!

createPrioritySharedQueue
	| psq |
	psq := PrioritySharedQueue new.
	psq nextPut: '0a'.
	psq nextPut: '3a' priority: 3.
	psq nextPut: '1a' priority: 1.
	psq nextPut: '2a' priority: 2.
	psq nextPut: '0b'.
	psq nextPut: '2b' priority: 2.
	psq nextPut: '3b' priority: 3.
	psq nextPut: '4a' priority: 4.
	psq nextPut: '0c'.
	psq nextPut: '1b' priority: 1.
	psq nextPut: '4b' priority: 4.
	psq nextPut: '0d'.
	^psq!

test
	"self test"

	| psq |
	psq := self createPrioritySharedQueue.
	Transcript cr; cr; show: 'Straight priorities'.
	[psq peek notNil]
		whileTrue: [Transcript cr; show: psq next printString]! !

