VisualLauncher subclass: #MyVisualLauncher
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Launcher'!
MyVisualLauncher comment:
'I am a subclass of VisualLauncher for release 2.0. I provide the following additional capabilities:
	A different window label
	Some additional options in the Change toolbar menu and some rearranging of menu items
	An additional toolbar menu item called Other, which provides various new functions
	The Browse All Classes button and menu pick now open a Full Browser rather than a System Browser.'!


!MyVisualLauncher methodsFor: 'interface opening'!

postBuildWith: aBuilder
	super postBuildWith: aBuilder.
	aBuilder window label: 'My VisualLauncher'! !

!MyVisualLauncher methodsFor: 'menus'!

newMenuBar
	| menuItem |
	menuItem := MenuItem labeled: 'Other'.
	menuItem submenu: self class otherMenu.
	^(super newMenuBar) addItem: menuItem; yourself! !

!MyVisualLauncher methodsFor: 'actions'!

browseAllClasses
	"Open new System Browser"
	
	self openApplicationForClassNamed: #FullBrowser!

changesCondenseChanges
	SourceFileManager default condenseChanges!

otherCloseFile
	| path count |
	self newMethod: ''.
	count := 0.
	path := Dialog request: 'Full path for file you want to close?' initialAnswer: ''.
	(ExternalStream classPool at: #OpenStreams) copy 
		do: [:each | each name = path
			ifTrue: 
				[each close.
				count := count + 1]].
	Dialog warn: 'Closed ' , count printString , ' files'!

otherCopyClass
	| sourceName sourceClass destinationName writeStream newSourceCode |
	self newMethod: ''.
	sourceName := Dialog request: 'Class to Copy?'.
	sourceClass := Smalltalk 
		at: sourceName asSymbol 
		ifAbsent: [^Dialog warn: 'This class does not exist'].
	destinationName := Dialog request: 'New Class?' initialAnswer: ''.
	destinationName = '' ifTrue: [^self].
	(Smalltalk at: destinationName asSymbol ifAbsent: [nil]) notNil 
		ifTrue: [^Dialog warn: 'This class already exists'].
	writeStream := (String new: 1000) writeStream.
	sourceClass fileOutSourceOn: (SourceCodeStream on: writeStream).
	newSourceCode := writeStream contents copyReplaceAll: sourceName with: destinationName.
	newSourceCode readStream fileIn!

otherInspectInstances
	| class className |
	self newMethod: ''.
	className := Dialog request: 'Inspect instances of which class?' initialAnswer: ''.
	class := Smalltalk at: className asSymbol ifAbsent: [nil].
	class notNil
		ifTrue: [class allInstances inspect]
		ifFalse: [Dialog warn: 'This class does not exist']!

otherInspectOpenFiles
	self newMethod: ''.
	(ExternalStream classPool at: #OpenStreams) copy inspect!

otherInspectSmalltalk
	self newMethod: ''.
	Smalltalk inspect!

otherInspectUndeclared
	self newMethod: ''.
	Undeclared inspect!

otherOpenCodingAssistant
	self newMethod: ''.
	CodingAssistant open!

otherRemoveDialog
	self newMethod: ''.
	ScheduledControllers scheduledControllers 
		do: [:each | (each isKindOf: ApplicationDialogController)
			ifTrue: [each closeAndUnschedule]]!

otherRemoveInstances
	| class className count |
	self newMethod: ''.
	className := Dialog request: 'Class for which to remove instances?' initialAnswer: ''.
	class := Smalltalk at: className asSymbol ifAbsent: [nil].
	class notNil
		ifTrue: 
			[count := class allInstances size.
			class allInstancesDo: [:each | each become: String new].
			Dialog warn: 'Removed ' , count printString , ' instances']
		ifFalse: [Dialog warn: 'This class does not exist']!

otherRemoveWindow
	| label |
	self newMethod: ''.
	label := Dialog request: 'Label for window you want to remove?' initialAnswer: ''.
	ScheduledControllers scheduledControllers 
		do: [:each | 
			(each view label = label or: [each view label = label asSymbol])
				ifTrue: [each closeAndUnschedule]]! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyVisualLauncher class
	instanceVariableNames: ''!


!MyVisualLauncher class methodsFor: 'resources'!

changesMenu
	"UIMenuEditor new openOnClass: self andSelector: #changesMenu"

	<resource: #menu>
	^#(#Menu #(
			#(#MenuItem 
				#label: 'Inspect ChangeSet' 
				#accessCharacterPosition: 9 ) 
			#(#MenuItem 
				#label: 'Changed Methods' 
				#accessCharacterPosition: 9 ) 
			#(#MenuItem 
				#label: 'Open Change List' 
				#accessCharacterPosition: 13 ) 
			#(#MenuItem 
				#label: 'File Out Changes...' 
				#accessCharacterPosition: 1 ) 
			#(#MenuItem 
				#label: 'Empty Changes...' 
				#accessCharacterPosition: 1 ) 
			#(#MenuItem 
				#label: 'Condense Changes' ) 
			#(#MenuItem 
				#label: 'Open Project' 
				#accessCharacterPosition: 6 ) 
			#(#MenuItem 
				#label: 'Exit Project' 
				#accessCharacterPosition: 1 ) ) #(2 4 2 ) #(#changesInspectChangeSet #changesChangedMethods #changesOpenChangeList #changesFileOutChanges #changesEmptyChanges #changesCondenseChanges #changesOpenProject #changesExitProject ) ) decodeAsLiteralArray!

otherMenu
	"UIMenuEditor new openOnClass: self andSelector: #otherMenu"

	<resource: #menu>
	^#(#Menu #(
			#(#MenuItem 
				#label: 'Coding Assistant' ) 
			#(#MenuItem 
				#label: 'Copy Class' ) 
			#(#MenuItem 
				#label: 'Remove...' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#label: 'Window' ) 
						#(#MenuItem 
							#label: 'Dialogs' ) ) #(2 ) #(#otherRemoveWindow #otherRemoveDialog ) ) ) 
			#(#MenuItem 
				#label: 'Open Files...' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#label: 'Inspect' ) 
						#(#MenuItem 
							#label: 'Close' ) ) #(2 ) #(#otherInspectOpenFiles #otherCloseFile ) ) ) 
			#(#MenuItem 
				#label: 'Inspect...' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#label: 'Smalltalk' ) 
						#(#MenuItem 
							#label: 'Undeclared' ) ) #(2 ) #(#otherInspectSmalltalk #otherInspectUndeclared ) ) ) 
			#(#MenuItem 
				#label: 'Instances...' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#label: 'Inspect' ) 
						#(#MenuItem 
							#label: 'Remove' ) ) #(2 ) #(#otherInspectInstances #otherRemoveInstances ) ) ) ) #(6 ) #(#otherOpenCodingAssistant #otherCopyClass nil nil nil nil ) ) decodeAsLiteralArray! !

