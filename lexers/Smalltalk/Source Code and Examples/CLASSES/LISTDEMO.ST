ApplicationModel subclass: #ListDemo
	instanceVariableNames: 'pets keyboardSelectors '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ListDemo'!
ListDemo comment:
'I demonstrate the use of double-click blocks, double-click selectors, and keyboard events in a list. When the user double-clicks on a selection in the list, I take an action, then install a new double-click handler. I alternate between installing a selector and a block to handle double-clicks.

If the user selects an item in the list there are two keys that will cause actions to be taken:
	Right-arrow brings up a Dialog that simulates showing another screen with additional details.
	Delete deletes the currently selected item from the list.
We achieve these actions by install a keyboard hook.

Instance Variables:

	list					<SelectionInList>	Where we store our list of items to be displayed.
	keyboardSelectors	<Dictionary>		The key is the character the user has typed.
											The value is the message selector to send.

By keeping the keys in a dictionary, we can install the keyboard hook, then dynamically modify the keys we trap and the actions we take in response.'!


!ListDemo methodsFor: 'initialize'!

initialize
	pets := SelectionInList new.
	pets list: #('dog' 'cat' 'mouse' 'parrot') asList.
	self mySetUpKeyboardSelectors!

mySetUpKeyboardSelectors
	keyboardSelectors := Dictionary new.
	keyboardSelectors 
		at: Character del put: #deleteSelection;
		at: #Right put: #expandSelection! !

!ListDemo methodsFor: 'keyboard'!

deleteSelection
	| index |
	index := self pets selectionIndex.
	index > 0 ifTrue: [self pets list removeAtIndex: index]!

expandSelection
	self pets selectionIndex > 0
		ifTrue: [Dialog warn: 'Details of selection']!

myKeyboardHookBlock
	^
	[:event :controller | 
	| selector |
	selector := keyboardSelectors at: event keyValue ifAbsent: [nil].
	selector notNil ifTrue: [self perform: selector].
	event]! !

!ListDemo methodsFor: 'interface opening'!

postBuildWith: aBuilder 
	| listController |
	super postBuildWith: aBuilder.
	listController := (self builder componentAt: #petsLB) widget controller.

	"Set up a keyboard hook to intercept keystrokes"
	listController keyboardHook: self myKeyboardHookBlock.

	"Set up a block to be executed when user double clicks mouse"
	listController dispatcher doubleClick: [self blockDoubleClick].! !

!ListDemo methodsFor: 'double clicking'!

automaticDoubleClick
	Dialog warn: 'Automatic double click'!

blockDoubleClick
	Dialog warn: 'Block double click'.
	(self builder componentAt: #petsLB) 
		widget controller dispatcher doubleClickSelector: #selectorDoubleClick!

selectorDoubleClick
	Dialog warn: 'Selector double click'.
	(self builder componentAt: #petsLB) 
		widget controller dispatcher doubleClick: [self blockDoubleClick].! !

!ListDemo methodsFor: 'aspects'!

pets
	^pets! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ListDemo class
	instanceVariableNames: ''!


!ListDemo class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'List' 
			#min: #(#Point 224 194 ) 
			#max: #(#Point 224 194 ) 
			#bounds: #(#Rectangle 546 424 770 618 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#SequenceViewSpec 
					#layout: #(#LayoutFrame 10 0 80 0 -10 1 -10 1 ) 
					#name: #petsLB 
					#model: #pets 
					#callbacksSpec: 
					#(#UIEventCallbackSubSpec 
						#doubleClickSelector: #automaticDoubleClick ) ) 
				#(#NoteBookSpec 
					#layout: #(#Rectangle -254 -92 -104 8 ) ) 
				#(#LabelSpec 
					#layout: #(#Point 13 7 ) 
					#label: 'Double-click on items' ) 
				#(#LabelSpec 
					#layout: #(#Point 13 30 ) 
					#label: 'Select item then press:' ) 
				#(#LabelSpec 
					#layout: #(#Point 29 49 ) 
					#label: 'Delete or right-arrow' ) ) ) )! !

!ListDemo class methodsFor: 'examples'!

example
	"self example"
	self open! !

