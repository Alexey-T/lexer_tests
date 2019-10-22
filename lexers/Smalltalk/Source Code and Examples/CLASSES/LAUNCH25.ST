'From VisualWorks(R), Release 2.5 of September 26, 1995 on October 24, 1996 at 2:53:35 pm'!

VisualLauncher subclass: #MyVisualLauncher
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Launcher'!
MyVisualLauncher comment:
'I am a subclass of VisualLauncher for release 2.5. I provide the following additional capabilities:
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
	| menuItem menuLabels menuSelectors changesMenu newMenuBar |
	newMenuBar := super newMenuBar.
	menuItem := MenuItem labeled: 'Other'.
	menuItem submenu: self class otherMenu.
	newMenuBar addItem: menuItem.
	
	"The changes below will add the Condense changes to the end of the menu.
	Uncomment if you want these changes"
"	menuLabels := #('Condense changes').
	menuSelectors := #(#changesCondenseChanges).
	changesMenu := newMenuBar menuItemLabeled: 'Changes'.
	changesMenu submenu addItemGroupLabels: menuLabels values: menuSelectors.
"
	^newMenuBar! !

!MyVisualLauncher methodsFor: 'actions'!

browseAllClasses
	"Open new System Browser"

	self openApplicationForClassNamed: #FullBrowser!

changesCondenseChanges
	SourceFileManager default condenseChanges!

otherCloseFile
	| count labels choice |
	count := 0.
	labels := (ExternalStream classPool at: #OpenStreams) 
		collect: [:each | each name].
	labels := labels asSet asSortedCollection.
	choice := Dialog
				choose: 'Close which file?'
				fromList: labels
				values: labels
				lines: 8
				cancel: [^self].
	(ExternalStream classPool at: #OpenStreams) copy 
		do: [:each | each name = choice
			ifTrue: 
				[each close.
				count := count + 1]].
	Dialog warn: 'Closed ' , count printString , ' files'!

otherCopyClass
	| sourceName sourceClass destinationName writeStream newSourceCode |
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
	className := Dialog request: 'Inspect instances of which class?' initialAnswer: ''.
	class := Smalltalk at: className asSymbol ifAbsent: [nil].
	class notNil
		ifTrue: [class allInstances inspect]
		ifFalse: [Dialog warn: 'This class does not exist']!

otherInspectOpenFiles
	(ExternalStream classPool at: #OpenStreams) copy inspect!

otherInspectSmalltalk
	Smalltalk inspect!

otherInspectUndeclared
	Undeclared inspect!

otherOpenCodingAssistant
	CodingAssistant open!

otherRemoveDialog
	ScheduledControllers scheduledControllers 
		do: [:each | (each isKindOf: ApplicationDialogController)
			ifTrue: [each closeAndUnschedule]]!

otherRemoveInstances
	| class className count |
	className := Dialog request: 'Class for which to remove instances?' initialAnswer: ''.
	class := Smalltalk at: className asSymbol ifAbsent: [nil].
	class notNil
		ifTrue: 
			[count := class allInstances size.
			class allInstancesDo: [:each | each become: String new].
			Dialog warn: 'Removed ' , count printString , ' instances']
		ifFalse: [Dialog warn: 'This class does not exist']!

otherRemoveWindow
	| labels choice |
	labels := ScheduledControllers scheduledControllers 
				collect: [:each | each view label].
	labels := labels asSet asSortedCollection.
	choice := Dialog
				choose: 'Close which window(s)?'
				fromList: labels
				values: labels
				lines: 8
				cancel: [^self].
	ScheduledControllers scheduledControllers 
		do: [:each | each view label = choice 
				ifTrue: [each closeAndUnschedule]]! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyVisualLauncher class
	instanceVariableNames: ''!


!MyVisualLauncher class methodsFor: 'resources'!

menuBar
	"MenuEditor new openOnClass: self andSelector: #menuBar"

	<resource: #menu>
	^#(#Menu #(
			#(#MenuItem 
				#rawLabel: '&File' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#rawLabel: '&Save As...' 
							#value: #imageSaveAs ) 
						#(#MenuItem 
							#rawLabel: '&Perm Save As...' 
							#value: #filePermSaveAs ) 
						#(#MenuItem 
							#rawLabel: 'Perm &Undo As...' 
							#value: #filePermUndoAs ) 
						#(#MenuItem 
							#rawLabel: '&Collect Garbage' 
							#value: #collectGarbage ) 
						#(#MenuItem 
							#rawLabel: 'Collect All &Garbage' 
							#value: #collectAllGarbage ) 
						#(#MenuItem 
							#rawLabel: 'Se&ttings' 
							#value: #visualWorksSettings ) 
						#(#MenuItem 
							#rawLabel: 'E&xit VisualWorks...' 
							#value: #visualWorksExit ) ) #(3 2 1 1 ) nil ) ) 
			#(#MenuItem 
				#rawLabel: '&Browse' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#rawLabel: '&All Classes' 
							#value: #browseAllClasses 
							#labelImage: #(#ResourceRetriever nil #allClassesIcon ) ) 
						#(#MenuItem 
							#rawLabel: 'Class &Named...' 
							#value: #browseClassNamed ) 
						#(#MenuItem 
							#rawLabel: '&Resources' 
							#value: #browseApplications 
							#labelImage: #(#ResourceRetriever nil #finderIcon ) ) 
						#(#MenuItem 
							#rawLabel: 'References &To...' 
							#value: #browseSendersOf ) 
						#(#MenuItem 
							#rawLabel: '&Implementors Of...' 
							#value: #browseImplementorsOf ) ) #(3 2 ) nil ) ) 
			#(#MenuItem 
				#rawLabel: '&Tools' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#rawLabel: '&File List' 
							#value: #openFileList 
							#labelImage: #(#ResourceRetriever nil #fileListIcon ) ) 
						#(#MenuItem 
							#rawLabel: 'File &Editor...' 
							#value: #openFileEditor ) 
						#(#MenuItem 
							#rawLabel: '&Workspace' 
							#value: #toolsNewWorkspace 
							#labelImage: #(#ResourceRetriever nil #workspaceIcon ) ) 
						#(#MenuItem 
							#rawLabel: 'Parcel &List' 
							#value: #openParcelList ) 
						#(#MenuItem 
							#rawLabel: '&New Canvas' 
							#value: #toolsNewCanvas 
							#labelImage: #(#ResourceRetriever nil #newCanvasIcon ) ) 
						#(#MenuItem 
							#rawLabel: '&Palette' 
							#value: #toolsPalette ) 
						#(#MenuItem 
							#rawLabel: '&Canvas Tool' 
							#value: #toolsCanvasTool ) 
						#(#MenuItem 
							#rawLabel: '&Image Editor' 
							#value: #toolsMaskEditor ) 
						#(#MenuItem 
							#rawLabel: '&Menu Editor' 
							#value: #toolsMenuEditor ) 
						#(#MenuItem 
							#rawLabel: '&Advanced' 
							#nameKey: #advanced ) 
						#(#MenuItem 
							#rawLabel: '&DLL and C Connect' 
							#nameKey: #dllcc 
							#value: #openExternalFinder 
							#labelImage: #(#ResourceRetriever nil #extFinderIcon ) ) 
						#(#MenuItem 
							#rawLabel: 'System &Transcript' 
							#nameKey: #transcript 
							#value: #toggleSystemTranscript 
							#indication: true ) ) #(4 5 2 1 ) nil ) ) 
			#(#MenuItem 
				#rawLabel: '&Changes' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#rawLabel: 'Inspect &ChangeSet' 
							#value: #changesInspectChangeSet ) 
						#(#MenuItem 
							#rawLabel: 'Changed &Methods' 
							#value: #changesChangedMethods ) 
						#(#MenuItem 
							#rawLabel: '&File Out Changes...' 
							#value: #changesFileOutChanges ) 
						#(#MenuItem 
							#rawLabel: '&Empty Changes...' 
							#value: #changesEmptyChanges ) 
						#(#MenuItem 
							#rawLabel: 'Open Change &List' 
							#value: #changesOpenChangeList ) 
						#(#MenuItem 
							#rawLabel: 'Condense Changes' 
							#value: #changesCondenseChanges ) 
						#(#MenuItem 
							#rawLabel: 'Open &Project' 
							#value: #changesOpenProject ) 
						#(#MenuItem 
							#rawLabel: 'E&xit Project' 
							#value: #changesExitProject ) ) #(2 4 2 ) nil ) ) 
			#(#MenuItem 
				#rawLabel: '&Database' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#rawLabel: 'Ad Hoc &SQL' 
							#nameKey: #adHoc 
							#value: #openAdHocQuery ) 
						#(#MenuItem 
							#rawLabel: 'Data &Modeler' 
							#nameKey: #dataModeler 
							#value: #openDataModelBrowser 
							#labelImage: #(#ResourceRetriever nil #dbToolIcon ) ) 
						#(#MenuItem 
							#rawLabel: 'Canvas &Composer' 
							#nameKey: #canvasComposer 
							#value: #openCanvasComposer ) 
						#(#MenuItem 
							#rawLabel: 'New Data &Form...' 
							#nameKey: #dataForm 
							#value: #newDataForm ) 
						#(#MenuItem 
							#rawLabel: 'New Database &Application...' 
							#nameKey: #dataBaseAp 
							#value: #newDataMain ) ) #(1 4 ) nil ) ) 
			#(#MenuItem 
				#rawLabel: '&Window' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#rawLabel: 'Re&fresh All' 
							#value: #winRefreshAll ) 
						#(#MenuItem 
							#rawLabel: '&Collapse All' 
							#value: #winCollapseAll ) 
						#(#MenuItem 
							#rawLabel: 'Re&store All' 
							#value: #winRestoreAll ) 
						#(#MenuItem 
							#rawLabel: '&Windows' 
							#nameKey: #windowsMenu ) ) #(3 1 ) nil ) ) 
			#(#MenuItem 
				#rawLabel: '&Help' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#rawLabel: 'Open Online &Documentation' 
							#value: #openHelpBrowser 
							#labelImage: #(#ResourceRetriever nil #helpIcon ) ) 
						#(#MenuItem 
							#rawLabel: '&Quick Start Guides...' 
							#value: #openGuidingDialog ) 
						#(#MenuItem 
							#rawLabel: 'About &VisualWorks...' 
							#value: #helpAbout ) ) #(2 1 ) nil ) ) ) #(7 ) nil ) decodeAsLiteralArray!

otherMenu
	"UIMenuEditor new openOnClass: self andSelector: #otherMenu"

	<resource: #menu>
	^#(#Menu #(
			#(#MenuItem 
				#label: 'Coding Assistant' ) 
			#(#MenuItem 
				#label: 'Copy Class...' ) 
			#(#MenuItem 
				#label: 'Remove...' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#label: 'Window...' ) 
						#(#MenuItem 
							#label: 'Dialogs' ) ) #(2 ) #(#otherRemoveWindow #otherRemoveDialog ) ) ) 
			#(#MenuItem 
				#label: 'Open Files...' 
				#submenu: #(#Menu #(
						#(#MenuItem 
							#label: 'Inspect' ) 
						#(#MenuItem 
							#label: 'Close...' ) ) #(2 ) #(#otherInspectOpenFiles #otherCloseFile ) ) ) 
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
							#label: 'Inspect...' ) 
						#(#MenuItem 
							#label: 'Remove...' ) ) #(2 ) #(#otherInspectInstances #otherRemoveInstances ) ) ) ) #(6 ) #(#otherOpenCodingAssistant #otherCopyClass nil nil nil nil ) ) decodeAsLiteralArray! !
