Object subclass: #Customizer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Customizing'!
Customizer comment:
'I customize your environment. You''ll have to modify all the methods in the names protocol to refer to your working environment. You''ll also need to modify the keyboard mappings to meet your needs. In particular, if you choose to keep the emacs keybindings, you''ll need to get the code for these bindings from the Smalltalk archives.'!


!Customizer methodsFor: 'names'!

myLauncherClass
	^MyVisualLauncher!

myNewClasses
	^	#(
		'Launch25.st'
		)!

myNewClassesDir
	^'c:\st\classes' asFilename!

mySysAdditionsDir
	^'c:\st\sysadds' asFilename!

mySysModificationsDir
	^'c:\st\sysmods' asFilename! !

!Customizer methodsFor: 'customization'!

customize
	"self new customize"

	self myFileInSystemAdditions.
	self myFileInSystemModifications.
	self myFileInNewClasses.
	self myKeyboardChanges.
	self myLauncherMenu.
	self myResetChangeSet.
	self myOpenWindows.!

myFileInNewClasses
	self myNewClasses do: 
		[:each | (self myNewClassesDir construct: each) fileIn]!

myFileInSystemAdditions
	(self mySysAdditionsDir filesMatching: '*.st')
		do: [:each | each asFilename fileIn]!

myFileInSystemModifications
	(self mySysModificationsDir filesMatching: '*.st')
		do: [:each | each asFilename fileIn]!

myKeyboardChanges
	"self myKeyboardChanges"

	"Delete key should delete forwards"
	LookPreferences deleteForward: true.	

	"Change the interrupt key"
	InputState interruptKeyValue: (TextConstants at: #Ctrlq).

	ParagraphEditor keyboard
		"Windows compatibility"
		bindValue: #cutKey: to: (TextConstants at: #Ctrlx);
		bindValue: #copyKey: to: (TextConstants at: #Ctrlc);
		bindValue: #pasteKey: to: (TextConstants at: #Ctrlv);
		"Accept and Undo"
		bindValue: #acceptKey: to: (TextConstants at: #Ctrla);
		bindValue: #undoKey: to: (TextConstants at: #Ctrlu);
		"Search and replace"
		bindValue: #findDialogKey: to: (TextConstants at: #Ctrlo); "Query Search"
		bindValue: #replaceDialogKey: to: (TextConstants at: #Ctrlp); "Query Replace"
		"emacs type things"
		bindValue: #endKey: to: (TextConstants at: #Ctrle);
		bindValue: #deleteForwardChar: to: (TextConstants at: #Ctrld);
		bindValue: #deleteToEndOfLineKey: to: (TextConstants at: #Ctrlk);
		bindValue: #deleteForwardWordKey: to: (TextConstants at: #ESC) followedBy: $d.

	"If this is not VW2.0, we need to initialize the TextEditorController 
	otherwise we don't get the above changes in the Browsers"
	(Smalltalk version findString: '2.0' startingAt: 1) == 0
		ifTrue: [TextEditorController initialize].

	self myPrintKeyboardChanges!

myLauncherMenu
	| windowMenu menuItems subMenu launcherItem |
	windowMenu := StandardSystemController classPool at: #ScheduledBlueButtonMenu.
	launcherItem := windowMenu menuItemLabeled: 'Launcher' ifNone: [nil].
	launcherItem notNil ifTrue: [windowMenu removeItem: launcherItem].
	menuItems := self myLauncherClass new newMenuBar menuItems.
	subMenu := Menu new addItemGroup: menuItems.
	windowMenu addItem: 
		((MenuItem labeled: 'Launcher') submenu: subMenu)!

myOpenWindows
	self myLauncherClass open!

myPrintKeyboardChanges
	Transcript 
		cr; cr;
		nextPutAll: '*****************************************************************';
		cr; nextPutAll: 'Ctrl-A,U: Accept, Undo';
		cr; nextPutAll: 'Ctrl-X,C,V: Cut, Copy, Paste';
		cr; nextPutAll: '***** Ctrl-C has been remapped to Ctrl-Q *****';
		flush.!

myResetChangeSet
	"When things are filed out, remove them from the Change Set"
	Browser removeChangesOnFileOut: true.

	"Empty the change set"
	ChangeSet noChanges.! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Customizer class
	instanceVariableNames: ''!


!Customizer class methodsFor: 'examples'!

customize
	"self customize"

	self new customize! !

!Customizer class methodsFor: 'class initialization'!

initialize
	"If you want to invoke the customization code automatically 
	when you file in this class, uncomment the following line"

	"self customize"! !
Customizer initialize!


