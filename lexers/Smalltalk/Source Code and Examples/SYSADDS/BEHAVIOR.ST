'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 2, 1996 at 6:11:44 am'!



!Behavior methodsFor: '(additions)'!

allInstancesAndSubInstances
	"Answer a collection of all instances of this class and its subclasses"

	self newMethod: 'When. Who. Why'.
	^self withAllSubclasses 
		inject: OrderedCollection new
		into: [:coll :each | coll addAll: each allInstances; yourself]!

removeInstances
	self newMethod: 'When. Who. Why'.
	self allInstancesDo: [ :each | each become: String new]! !
