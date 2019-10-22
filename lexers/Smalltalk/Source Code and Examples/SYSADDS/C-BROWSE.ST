'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 3, 1996 at 10:22:22 am'!



!Browser class methodsFor: '(additions)'!

allMethodsInProtocol: aProtocol 
	"Answer a SortedCollection of all the methods that appear in the protocol called aProtocol."
	"(self allMethodsInProtocol: #'(additions)') inspect"

	| aCollection theProtocolSymbol |
	self newMethod: 'When. Who. Why'.
	aCollection := SortedCollection new.
	theProtocolSymbol := Symbol findInterned: aProtocol asString.
	theProtocolSymbol isNil ifTrue: [^aCollection].
	Smalltalk allBehaviorsDo: [:class | 
		(class organization listAtCategoryNamed: theProtocolSymbol)
			do: [:selector | aCollection add: class name , ' ' , selector]].
	^aCollection!

browseAllMethodsInProtocol: aProtocol
	"Browse all methods in a protocol called aProtocol."
	"self browseAllMethodsInProtocol: #'(additions)'"

	self newMethod: 'When. Who. Why'.
	MethodListBrowser
		openListBrowserOn: (self allMethodsInProtocol: aProtocol)
		label: 'class ''', aProtocol, ''' protocol'! !
