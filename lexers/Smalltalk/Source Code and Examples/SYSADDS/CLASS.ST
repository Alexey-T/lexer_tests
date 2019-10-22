'From VisualWorks(R), Release 2.5 of September 26, 1995 on January 27, 1996 at 8:43:14 am'!



!Class methodsFor: '(additions)'!

copyTo: aSymbolOrString
	| writeStream newSourceCode |
	self newMethod: 'When. Who. Why'.
	writeStream := (String new: 1000) writeStream.
	self fileOutSourceOn: (SourceCodeStream on: writeStream).
	newSourceCode:= writeStream contents 
		copyReplaceAll: self name asString
		with: aSymbolOrString asString.
	newSourceCode readStream fileIn!

getAccessor: aName 
	| stream |
	self newMethod: 'When. Who. Why'.
	stream := String new writeStream.
	stream nextPutAll: aName.
	^self getAccessor: aName stream: stream!

getAccessor: aVariableName stream: aStream
	self newMethod: 'When. Who. Why'.
	^aStream
		crtab; 
		nextPut: $^; 
		nextPutAll: aVariableName; 
		contents!

myGetAccessor: aName 
	| stream |
	self newMethod: 'When. Who. Why'.
	stream := String new writeStream.
	stream nextPutAll: 'my'.
	stream nextPutAll: aName capitalize.
	^self getAccessor: aName stream: stream!

mySetAccessor: aName 
	| stream |
	self newMethod: 'When. Who. Why'.
	stream := String new writeStream.
	stream nextPutAll: 'my'.
	stream nextPutAll: aName capitalize.
	^self setAccessor: aName stream: stream!

mySubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat 
	| newClass |
	self newMethod: 'When. Who. Why'.
	newClass := self
				subclass: t
				instanceVariableNames: f
				classVariableNames: d
				poolDictionaries: s
				category: cat.
	newClass instVarNames
		do: 
			[:each | 
			newClass compile: (self getAccessor: each) classified: #accessing.
			newClass compile: (self setAccessor: each) classified: #accessing.
			newClass compile: (self myGetAccessor: each) classified: #'private-accessing'.
			newClass compile: (self mySetAccessor: each) classified: #'private-accessing'].
	^newClass!

setAccessor: aName 
	| stream |
	self newMethod: 'When. Who. Why'.
	stream := String new writeStream.
	stream nextPutAll: aName.
	^self setAccessor: aName stream: stream!

setAccessor: aVariableName stream: aStream
	| value prefix |
	self newMethod: 'When. Who. Why'.
	value := aVariableName capitalize.
	prefix := (value at: 1) isVowel ifTrue: ['an'] ifFalse: ['a'].
	value := prefix, value.
	^aStream
		nextPutAll: ': ';
		nextPutAll: value;
		crtab; 
		nextPutAll: aVariableName; 
		nextPutAll: ' := ';
		nextPutAll: value;
		contents! !
