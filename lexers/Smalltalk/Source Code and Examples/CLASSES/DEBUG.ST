"NOTE: When you file this in, you will get an unhandled exception telling you that one of the classes is being defined with a nil superclass. This is exactly what we want, so press the Proceed button."

Object subclass: #WatcherExample
	instanceVariableNames: 'watchedObject '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Debug'!


!WatcherExample methodsFor: 'examples'!

doPrintString
	watchedObject printString.!

example
	"self new example"

	watchedObject := Watcher on: (Array new: 3).
	self put.
	self getWatchedObjectValue.
	self doPrintString.
	self getSize!

getSize
	watchedObject size!

getWatchedObjectValue
	watchedObject at: 2!

put
	watchedObject at: 2 put: 'hello'! !

nil subclass: #Watcher
	instanceVariableNames: 'watchedObject '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Debug'!
Watcher comment:
'I take the place of an object created by the application. When messages are sent to me I log them, send them to the application object, then log the result. I am subclassed off nil, so I understand very few messages. I trap messages using doesNotUnderstand: then log the message and pass it on. When this class is filed it, an exception will be raised complaining about a nil superclass. This is what we want and need, so press the proceed button.

Instance Variables:

	watchedObject		<Any object>	The object whose place I am taking, and to whom 
										I will pass on the message.'!


!Watcher methodsFor: 'prototypes'!

class
	"Answer the object which is the receiver's class."

	<primitive: 111>
	self primitiveFailed! !

!Watcher methodsFor: 'message handling'!

doesNotUnderstand: aMessage 
	| result |
	Transcript
		cr; print: thisContext sender; nextPutAll: ' sent ';
		print: watchedObject class; nextPutAll: '>>'; nextPutAll: aMessage selector asString.
	aMessage arguments size > 0 ifTrue: 
		[Transcript print: aMessage arguments].
	result := watchedObject 
		perform: aMessage selector 
		withArguments: aMessage arguments.
	Transcript nextPutAll: ' ^'; print: result; flush.
	^result! !

!Watcher methodsFor: 'initialize-release'!

initializeWatcher: anObject 
	watchedObject := anObject! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Watcher class
	instanceVariableNames: ''!


!Watcher class methodsFor: 'instance creation'!

on: anObject 
	^self new initializeWatcher: anObject! !

!Watcher class methodsFor: 'examples'!

example
	"self example"

	| array |
	array := self on: (Array new: 3).
	array at: 2 put: 'hello'.
	array at: 2.
	array printString.
	array size! !

Object subclass: #LoggerExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Debug'!


!LoggerExample methodsFor: 'examples'!

method1
	self method2!

method2
	self method3!

method3
	self method4!

method4
	| logger |
	logger := Logger new.
	Transcript cr; show: 'About to log to Transcript'.
	logger log: 'Logging a message to the Transcript on ', Timestamp now printString.

	logger filePath: 'c:\tempx\err.txt'.
	Transcript cr; cr; show: 'About to log to file <', logger filePath, '>'; cr.
	logger log: 'Logging a message to file on ', Timestamp now printString.!

run
	self method1! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

LoggerExample class
	instanceVariableNames: ''!


!LoggerExample class methodsFor: 'examples'!

example
	"In a Workspace, type and evaluate:

	LoggerExample new run

	"! !

Object subclass: #Logger
	instanceVariableNames: 'file '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Debug'!
Logger comment:
'I log information to a file or to the Transcript. I also log the full context (ie, the message stack). If I am unable to write to the file, I write to the Transcript.

Instance Variables:

	file		<Filename>		If this is nil, I write to the Transcript. Otherwise I append to the file.
							If writing to a file, I open the file, write to it, then close it.'!


!Logger methodsFor: 'logging'!

filePath
	^file isNil
		ifTrue: ['Transcript']
		ifFalse: [file asString]!

filePath: aFilePath 
	OSErrorHolder errorSignal
		handle: 
			[:ex | 
			file := nil.
			ex return]
		do: [file := aFilePath asFilename]!

log: aString 
	"We get the context up front to avoid having to filter out the signal handler context"
	| contextString |
	contextString := self myContextString.
	Object errorSignal
		handle: 
			[:ex | 
			"Log to the Transcript if we can't log to the file"
			file := nil.
			ex restartDo: [self log: aString]]
		do: [self
				myWrite: self myWriteStream
				string: aString
				context: contextString]!

myContextString
	| context stream |
	stream := (String new: 100) writeStream.
	context := thisContext sender.

	["Unwind the stack until we are out of Logger"
	self == context receiver] whileTrue: [context := context sender].

	"Unwind the stack as far as possible, printing out as we go"
	[context notNil and: [context receiver notNil]]
		whileTrue: 
			[stream cr; print: context.
			context := context sender].
	^stream contents!

myWrite: aStream string: aString context: aContextString
	[aStream 
		cr; cr; 
		nextPutAll: aString; 
		nextPutAll: aContextString; 
		flush]
			valueNowOrOnUnwindDo: 
				[aStream == Transcript ifFalse: [aStream close]]!

myWriteStream
	^file notNil
		ifTrue: [file appendStream]
		ifFalse: [Transcript]! !

Object subclass: #ModifyingValues
	instanceVariableNames: 'color size things '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Debug'!


!ModifyingValues methodsFor: 'initialize-release'!

initialize
	color := #blue.
	size := 5 -> 10.
	things := OrderedCollection with: 'one' with: 'two' with: 'three' with: 'four'.
	self halt: 'Use an inspector to modify the values'! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ModifyingValues class
	instanceVariableNames: ''!


!ModifyingValues class methodsFor: 'examples'!

example
	"self example"
	self new initialize! !

