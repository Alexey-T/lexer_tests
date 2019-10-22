'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 3, 1996 at 10:36:04 am'!



!Inspector methodsFor: '(additions)'!

inspectOwners
	self newMethod: 'When. Who. Why'.
	((self fieldValue allOwnersWeakly: true) reject: 
		[:each | self shouldReject: each]) inspect!

inspectPaths
	self newMethod: 'When. Who. Why'.
	(ReferencePathCollector allReferencePathsTo: self fieldValue) inspect!

shouldReject: anObject
	self newMethod: 'When. Who. Why'.
	"The WeakArray often gives an owner of 0"
	anObject == 0 ifTrue: [^true].
	"We don't want to see the object we are inspecting"
	anObject == object ifTrue: [^true].
	"We don't want to see this inspector"
	anObject == self ifTrue: [^true].
	"We don't want to see the methods that got us here"
	(anObject class == MethodContext 
		and: [anObject selector == #allOwners 
		or: [anObject selector == #allOwnersWeakly:]]) ifTrue: [^true].
	"We don't want to see the stack array that contains the object"
	(anObject class == Array 
		and: [anObject size == 12 
		and: [(anObject at: 3) == self fieldValue]]) ifTrue: [^true].
	"We want to see anything that's left after this filter"
	^false! !
