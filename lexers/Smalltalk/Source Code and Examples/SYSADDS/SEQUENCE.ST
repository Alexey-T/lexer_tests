'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 2, 1996 at 6:27:17 am'!



!SequenceableCollection methodsFor: '(additions)'!

+ anObject 
	self newMethod: 'When. Who. Why'.
	^self copyWith: anObject!

findFirst: aBlock startingAt: anIndex 
	"Extended findFirst to allow caller to specify starting point"

	self newMethod: 'When. Who. Why'.
	anIndex to: self size do: [:index | (aBlock value: (self at: index))
			ifTrue: [^index]].
	^0! !
