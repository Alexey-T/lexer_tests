'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 2, 1996 at 6:25:14 am'!



!Stream methodsFor: '(additions)'!

nextUncommentedLine
	"Read a line, remove any comment, then trim the white space"

	self newMethod: 'When. Who. Why'.
	^((self upTo: Character cr)
		copyUpTo: $#) trimWhiteSpace! !
