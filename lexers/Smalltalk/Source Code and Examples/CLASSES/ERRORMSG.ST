Object subclass: #MyErrorMessages
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ErrorMsg'!
MyErrorMessages comment:
'I am the abstract superclass for error messages. I provide behavior that allows my subclasses to access error numbers and error text. My subclasses are responsible for filling in the Message dictionary that I create for them in initialize.

Class Instance Variables:

	Messages		<IdentityDictionary>		A dictionary of error messages, keyed by error symbol'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyErrorMessages class
	instanceVariableNames: 'Messages '!


!MyErrorMessages class methodsFor: 'accessing'!

notFoundError: aSymbol
	^0->('Symbol <', aSymbol, '> not found')!

numberFor: aSymbol 
	^(Messages 
		at: aSymbol 
		ifAbsent: [self notFoundError: aSymbol]) key printString!

textFor: aSymbol
	^(Messages 
		at: aSymbol 
		ifAbsent: [self notFoundError: aSymbol]) value.! !

!MyErrorMessages class methodsFor: 'class initialization'!

initialize
	"self initialize"

	Messages := IdentityDictionary new.! !

MyErrorMessages subclass: #ComponentOneErrorMessages
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ErrorMsg'!
ComponentOneErrorMessages comment:
'I provide error messages for component one.'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ComponentOneErrorMessages class
	instanceVariableNames: ''!


!ComponentOneErrorMessages class methodsFor: 'class initialization'!

initialize
	"self initialize"

	super initialize.
	Messages
		at: #notFound	put: 1 -> 'Account not found';
		at: #duplicate	put: 2 -> 'Duplicate account'.! !

!ComponentOneErrorMessages class methodsFor: 'accessing'!

numberFor: aSymbol
	^'C1-', (super numberFor: aSymbol)! !

MyErrorMessages subclass: #ComponentTwoErrorMessages
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ErrorMsg'!
ComponentTwoErrorMessages comment:
'I provide error messages for component two.'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ComponentTwoErrorMessages class
	instanceVariableNames: ''!


!ComponentTwoErrorMessages class methodsFor: 'class initialization'!

initialize
	"self initialize"

	super initialize.
	Messages
		at: #duplicate	put: 1 -> 'File already exists';
		at: #notFound	put: 2 -> 'File not found'.! !

!ComponentTwoErrorMessages class methodsFor: 'accessing'!

numberFor: aSymbol
	^'C2-', (super numberFor: aSymbol)! !

Object subclass: #ErrorMessageExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ErrorMsg'!
ErrorMessageExample comment:
'I provide the means to demonstrate the error message facility in operation.'!


!ErrorMessageExample methodsFor: 'examples'!

existentSymbol
	self printToTranscript: #notFound!

nonexistentSymbol
	self printToTranscript: #tooFew!

printToTranscript: aSymbol
	Transcript 
		cr; 
		nextPutAll: (ComponentOneErrorMessages numberFor: aSymbol);
		tab;
		nextPutAll: (ComponentOneErrorMessages textFor: aSymbol);
		flush! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ErrorMessageExample class
	instanceVariableNames: ''!


!ErrorMessageExample class methodsFor: 'examples'!

example
	"self example"

	self new
		existentSymbol;
		nonexistentSymbol! !
MyErrorMessages initialize!

ComponentOneErrorMessages initialize!

ComponentTwoErrorMessages initialize!


