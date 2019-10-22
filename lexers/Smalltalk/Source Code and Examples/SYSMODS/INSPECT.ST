'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 2, 1996 at 5:34:57 am'!



!Inspector methodsFor: '(modifications)'!

fieldMenu
	"Answer a Menu of operations on the variables that is to be displayed 
	when the operate menu button is pressed."

	"Inspector flushMenus"

	self changedMethod: 'When. Who. Added owners and path to the menu'.
	field == nil ifTrue: [^ nil].
	ListMenu == nil ifTrue:
		[ListMenu := Menu
			labels: 'inspect\owners\paths' withCRs
			values: #(inspectField inspectOwners inspectPaths)].
	^ ListMenu! !
