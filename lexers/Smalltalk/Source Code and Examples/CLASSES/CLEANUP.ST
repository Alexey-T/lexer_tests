Object subclass: #CleanUp
	instanceVariableNames: 'processes files sockets '
	classVariableNames: 'AccessProtect '
	poolDictionaries: ''
	category: 'STBE-CleanUp'!
CleanUp comment:
'I hold onto files and sockets that are opened, and long-term processes that are running. I then close and terminated them when commanded to do so. Objects should add files, sockets, and processes when they want them stored for possible cleanup. If the application closes a file or socket, or a process terminates, the application should inform me by sending one of the remove messages.

Instance Variables:

	processes		<OrderedCollection>		A collection of processes that are running
	files				<OrderedCollection>		A collection of files that are open
	sockets			<OrderedCollection>		A collection of sockets that are open

Class Variables:

	AccessProtect	<Semaphore>			A mutual exclusion semaphore that protects access
											to the collections held by the instance variables.'!


!CleanUp methodsFor: 'cleaning up'!

doCleanUp
	AccessProtect
		critical: 
			[processes do: [:each | each terminate].
			files do: [:each | each close].
			sockets do: [:each | each close].
			processes := files := sockets := nil]! !

!CleanUp methodsFor: 'removing'!

removeFile: aFile 
	^AccessProtect critical: [files remove: aFile]!

removeProcess: aProcess 
	^AccessProtect critical: [processes remove: aProcess]!

removeSocket: aSocket 
	^AccessProtect critical: [sockets remove: aSocket]! !

!CleanUp methodsFor: 'adding'!

addFile: aFile
    ^AccessProtect critical: [files add: aFile]!

addProcess: aProcess
    ^AccessProtect critical: [processes add: aProcess]!

addSocket: aSocket
    ^AccessProtect critical: [sockets add: aSocket]! !

!CleanUp methodsFor: 'initialize-release'!

initialize
	processes := OrderedCollection new.
	files := OrderedCollection new.
	sockets := OrderedCollection new! !

!CleanUp methodsFor: 'printing'!

printOn: aStream
	"Relies on the existence of Object>>printAllOn:"
	^super printAllOn: aStream! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

CleanUp class
	instanceVariableNames: ''!


!CleanUp class methodsFor: 'testing'!

test
	"self test"
	| instance |
	instance := self new.
	instance addFile: ('c:\temp\test1.txt' asFilename writeStream).
	instance addProcess: [(Delay forSeconds: 60) wait] fork.
	instance addFile: ('c:\temp\test2.txt' asFilename writeStream).
	instance addProcess: [(Delay forSeconds: 60) wait] fork.
	Transcript cr; show: instance printString.
	(Delay forSeconds: 5) wait.
	instance doCleanUp.
	Transcript cr; show: instance printString.! !

!CleanUp class methodsFor: 'instance creation'!

new
	^super new initialize! !

!CleanUp class methodsFor: 'class initialization'!

initialize
	"self initialize"

	AccessProtect := Semaphore forMutualExclusion! !
CleanUp initialize!


