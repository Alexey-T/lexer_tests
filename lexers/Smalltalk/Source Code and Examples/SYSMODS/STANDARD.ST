'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 2, 1996 at 6:09:15 am'!



!StandardSystemController methodsFor: '(modifications)'!

blueButtonActivity

	| choice |
	self changedMethod: 'When. Who. Send message to MyVisualLauncher if we cannot perform it'.
	choice :=  self
				trackMenu: ScheduledBlueButtonMenu
				at:  self sensor globalCursorPoint
				centered: false.
	(choice = 0 or: [choice isNil] ) ifTrue: [^self].
	(self respondsTo: choice)
		ifTrue: [self perform: choice]
		ifFalse: [MyVisualLauncher new perform: choice]! !
