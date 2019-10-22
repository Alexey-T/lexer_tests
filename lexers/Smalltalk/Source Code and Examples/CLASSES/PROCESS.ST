Process subclass: #MyProcess
	instanceVariableNames: 'processName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Processes'!
MyProcess comment:
'I allow the programmer to specify a name when creating a forked process. The name allows people to better track which processes are which. I rely on new methods being added to BlockClosure which allow a name to be specified as part of the fork message.

Instance Variables:

	processName		<String>	The name of the process'!


!MyProcess methodsFor: 'accessing'!

processName
	^processName!

processName: aString
	processName := aString! !

!MyProcess methodsFor: 'printing'!

printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' ('.
	aStream nextPutAll: self processName displayString.
	aStream nextPut: $)! !

