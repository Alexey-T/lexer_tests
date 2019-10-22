'From VisualWorks(R), Release 2.5 of September 26, 1995 on March 19, 1996 at 7:15:33 am'!



!Filename class methodsFor: '(additions)'!

construct: aFileString directories: anArrayOfDirectoryStrings 
	| stream |
	self newMethod: 'When. Who. Why'.
	stream := (String new: 30) writeStream.
	anArrayOfDirectoryStrings do: [:each | stream nextPutAll: each; nextPut: Filename separator].
	aFileString == nil ifFalse: [stream nextPutAll: aFileString].
	^self named: stream contents! !
