"local.st - Alec's specific environment changes
When. Who. Why.
"

| genericDir localDir |

Window platformName = 'X11'
	ifTrue: 
		[genericDir := '/visual/generic'.
		localDir := '/home/alec/st/fileIns']
	ifFalse:
		[genericDir := 'c:\visual\generic'.
		localDir := 'c:\alec\st\fileIns'].

"Do any personal and generic development environment fileIns"
(localDir asFilename filesMatching: '*.st')
	do: [ :each | each asFilename fileIn].
(genericDir asFilename filesMatching: '*.st')
	do: [ :each | each asFilename fileIn].

"Remap the keyboard"
ParagraphEditor keyboard
	bindValue: #cutKey: to: (TextConstants at: #Ctrlx);
	bindValue: #copyKey: to: (TextConstants at: #Ctrlc);
	bindValue: #pasteKey: to: (TextConstants at: #Ctrlv);
	bindValue: #acceptKey: to: (TextConstants at: #Ctrla).

"If this is not VW2.0, initialize the TextEditorController so it gets the new keyboard mappings"
(Smalltalk version findString: '2.0' startingAt: 1) == 0
	ifTrue: [TextEditorController initialize].

"Change the Interrupt key from Ctrl-C to Ctrl-Q"
InputState interruptKeyValue: (TextConstants at: #Ctrlq).

"Change my window colors"
Win3WidgetPolicy defaultColorWidgetColors 
	matchAt: SymbolicPaint background put: ColorValue lightCyan;
	matchAt: SymbolicPaint selectionBackground put: ColorValue pink;
	matchAt: SymbolicPaint selectionForeground put: ColorValue black.
Screen default updatePaintPreferences.
ScheduledControllers restore.

"Create some new windows with my new key bindings"
prompt := ScheduledWindow promptForOpen.
ScheduledWindow promptForOpen: false.
FullBrowser open.
ComposedTextView
	open: (ValueHolder with: 
		('c:\alec\st\useful' asFilename contentsOfEntireFile)) 
	label: 'Useful Things'
	icon: nil
	extent: 500@200.
ComposedTextView open.
ScheduledWindow promptForOpen: prompt.
