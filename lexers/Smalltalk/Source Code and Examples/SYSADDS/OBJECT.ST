'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 4, 1996 at 9:28:27 am'!



!Object methodsFor: '(additions)'!

altCtrlDo: aBlock
	self newMethod: 'When. Who. Why'.
	(InputState default altDown and: [InputState default ctrlDown])
			ifTrue: [aBlock value]!

altCtrlHalt
	"Note the two mechanisms we can use for detecting if the key is down.
	The first one is about 50% faster than the second."
	self newMethod: 'When. Who. Why'.
	(InputState default altDown 
		and: [ScheduledControllers activeController sensor ctrlDown])
			ifTrue: [Object haltSignal 
				raiseRequestWith: thisContext 
				errorString: 'Halt in ' , thisContext sender printString]!

changedMethod: aString
	"Does nothing. Used to find system class methods that have been changed"
	self newMethod: 'When. Who. Why'.!

echo
	self newMethod: 'When. Who. Why'.
	Transcript cr; nextPutAll: self displayString; flush!

echo: anObject
	self newMethod: 'When. Who. Why'.
	Transcript
		cr; nextPutAll: self displayString; 
		space; nextPutAll: anObject displayString;
		flush!

echoYourself
	self newMethod: 'When. Who. Why'.
	self echo!

echoYourself: anObject
	self newMethod: 'When. Who. Why'.
	self echo: anObject!

haltIf: aBlock 
	self newMethod: 'When. Who. Why'.
	aBlock value ifTrue: 
		[Object haltSignal 
			raiseRequestWith: thisContext 
			errorString: 'Halt in ' , thisContext sender printString]!

myHalt
	self newMethod: 'When. Who. Why'.
	Object haltSignal 
		raiseRequestWith: thisContext 
		errorString: 'Halt in ' , thisContext sender printString!

myHalt: aString 
	self newMethod: 'When. Who. Why'.
	Object haltSignal 
		raiseRequestWith: thisContext 
		errorString: 'Halt in ' , thisContext sender printString , ' - ' , aString!

newMethod: aString
	"Does nothing. Used to find methods added to system classes"!

obsoleteMethod: aString
	"Does nothing. Used to find obsolete methods"
	self newMethod: 'When. Who. Why'.!

optimizePoint: aString 
	"Does nothing. Used to find methods that need additional work"
	self newMethod: 'When. Who. Why'.!

printAllOn: aStream 
	| myClass |
	self newMethod: 'When. Who. Why'.
	myClass := self class.
	aStream nextPutAll:
		((myClass name at: 1) isVowel ifTrue: ['an '] ifFalse: ['a ']).
	aStream print: myClass.
	myClass allInstVarNames
		do: 
			[:each | 	| index |
			index := myClass instVarIndexFor: each.
			aStream 
				crtab; 
				nextPutAll: each; 
				nextPutAll: ': ';
				print: (self instVarAt: index)]! !
