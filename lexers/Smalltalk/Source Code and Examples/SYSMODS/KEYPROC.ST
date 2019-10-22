'From VisualWorks(R), Release 2.5 of September 26, 1995 on January 30, 1996 at 8:53:40 pm'!



!KeyboardProcessor methodsFor: '(modifications)'!

requestFocusFor: aController
	"Answer true if aController is willing to take keyboard focus.  Otherwise,
	answer false."

	self changedMethod: 'When. Who. Added controllerRequestingFocus'.
	currentConsumer == aController ifTrue: [^true].
	(currentConsumer == nil or: [currentConsumer = 0])
		ifTrue: [^true].
	controllerRequestingFocus := aController.
	currentConsumer requestFocusOut
		ifFalse: [^false].
	^aController requestFocusIn! !
