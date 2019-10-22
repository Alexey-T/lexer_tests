'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 3, 1996 at 10:13:28 am'!



!CharacterArray methodsFor: '(additions)'!

asArrayOfSubstrings
	"Create an array of substrings. Each element is a whitespace bounded word."

	| first last collection |
	self newMethod: 'When. Who. Why'.
	collection := OrderedCollection new.
	last := 0.
	
	[first := self findFirst: [:ch | ch isSeparator not]
				startingAt: last + 1.
	first ~= 0]
		whileTrue: 
			[last := (self findFirst: [:ch | ch isSeparator]
						startingAt: first)
						- 1.
			last < 0 ifTrue: [last := self size].
			collection add: (self copyFrom: first to: last)].
	^Array withAll: collection!

capitalize
	| newString |
	self newMethod: 'When. Who. Why'.
	newString := self copy.
	newString size > 0 ifTrue: [newString at: 1 put: (newString at: 1) asUppercase].
	^newString!

trimWhiteSpace
	| first |
	self newMethod: 'When. Who. Why'.
	first := self findFirst: [:ch | ch isSeparator not].
	^first = 0
		ifTrue: [String new]
		ifFalse: [self copyFrom: first to: (self findLast: [:ch | ch isSeparator not])]! !
