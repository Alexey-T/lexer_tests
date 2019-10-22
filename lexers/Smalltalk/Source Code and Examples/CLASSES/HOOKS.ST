ApplicationModel subclass: #HooksExample
	instanceVariableNames: 'inputOne inputTwo invisibleButton '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-HooksExample'!
HooksExample comment:
'I demonstrate the use of the methods:
	initialize				Invoked before any of the other hook
	preBuildWith:		Invoked before the window is built
	postBuildWith:		Invoked after the window is built but before it is opened
	postOpenWith:		Invoked after the window is open

Instance Variables:

	inputOne		<ValueHolder>		Holds a string for the first input field
	inputTwo		<ValueHolder>		Holds a string for the second input field
	invisibleButton	<Boolean>			True if the second button should be invisible'!


!HooksExample methodsFor: 'actions'!

doAction: aSymbol
	Transcript cr; show: 'Action ', aSymbol! !

!HooksExample methodsFor: 'initialize-release'!

initialize
	"Initialize things before the builder gets a chance to run."

	super initialize.
	inputOne := 'Text in input field one' asValue.
	inputTwo := 'Text in input field two' asValue! !

!HooksExample methodsFor: 'aspects'!

inputOne
	^inputOne!

inputTwo
	^inputTwo! !

!HooksExample methodsFor: 'accessing'!

invisibleButton
	^invisibleButton isNil
		ifTrue: [invisibleButton := Dialog confirm: 'Invisible button?']
		ifFalse: [invisibleButton]! !

!HooksExample methodsFor: 'interface opening'!

postBuildWith: aBuilder 
	"Invoked after the window is built but before it is opened.
	Do things that require a screen, but where we don't want
	the user to have alread seen the screen."

	super postBuildWith: aBuilder.
 	self invisibleButton
		ifTrue: 
			[(aBuilder componentAt: #actionOneAB) labelString: 'Yes'.
			(aBuilder componentAt: #actionTwoAB) beInvisible]
		ifFalse: 
			[(aBuilder componentAt: #actionOneAB) labelString: 'No'.
			(aBuilder componentAt: #actionTwoAB) beVisible]!

postOpenWith: aBuilder 
	"Invoked after the window is displayed.
	Do things that require the screen to be visible"

	| component |
	super postOpenWith: aBuilder.
	self invisibleButton
		ifTrue: 
			[component := aBuilder componentAt: #inputOneIF.
			component takeKeyboardFocus.
			component widget controller selectAt: self inputOne value size + 1]
		ifFalse: 
			[component := aBuilder componentAt: #inputTwoIF.
			component takeKeyboardFocus.
			component widget controller selectAt: 1]!

preBuildWith: aBuilder
	"Invoked before the window is built.
	Do things that will replace things defined in the window spec."

	super preBuildWith: aBuilder.
	aBuilder actionAt: #actionOne put: [ self doAction: #one ].
	aBuilder actionAt: #actionTwo put: [ self doAction: #two ].! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HooksExample class
	instanceVariableNames: ''!


!HooksExample class methodsFor: 'examples'!

example
	"self example"

	self open! !

!HooksExample class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Example of Hooks' 
			#min: #(#Point 262 270 ) 
			#max: #(#Point 262 270 ) 
			#bounds: #(#Rectangle 714 424 976 694 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 22 14 95 57 ) 
					#name: #actionOneAB 
					#model: #actionOne 
					#label: 'One' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 22 68 95 111 ) 
					#name: #actionTwoAB 
					#model: #actionTwo 
					#label: 'Two' 
					#defaultable: true ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 22 126 190 153 ) 
					#name: #inputOneIF 
					#model: #inputOne ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 23 162 191 189 ) 
					#name: #inputTwoIF 
					#model: #inputTwo ) 
				#(#LabelSpec 
					#layout: #(#Point 22 205 ) 
					#label: 'Notice which field has the focus' ) 
				#(#LabelSpec 
					#layout: #(#Point 22 220 ) 
					#label: 'and where the cursor is' ) ) ) )! !

