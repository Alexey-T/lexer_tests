ApplicationModel subclass: #MenuDynamicCreate
	instanceVariableNames: 'menuButton inputField textEditor mode '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-WidgetExamples'!
MenuDynamicCreate comment:
'This class demonstrates dynamic building of a menu. Each time the user goes to select a menu item, the menu is rebuilt based on the value of some variable.
'!


!MenuDynamicCreate methodsFor: 'support'!

itemSelected
	Dialog warn: 'You selected an item'.! !

!MenuDynamicCreate methodsFor: 'initialize-release'!

initialize
	menuButton := nil asValue.
	inputField := String new asValue.
	textEditor := String new asValue.
	mode := #menuOne.!

preBuildWith: aBuilder 
	aBuilder menuAt: #menuButtonMenu put: [self selectMenu].
	aBuilder menuAt: #inputFieldMenu put: [self selectMenu].
	aBuilder menuAt: #textEditorMenu put: [self selectMenu]! !

!MenuDynamicCreate methodsFor: 'aspects'!

inputField
	^inputField!

menuButton
	^menuButton!

textEditor
	^textEditor! !

!MenuDynamicCreate methodsFor: 'menus'!

menuOne
	^MenuBuilder new
		add: 'Dog' -> #itemSelected;
		add: 'Cat' -> #itemSelected;
		add: 'Elephant' -> #itemSelected;
		add: 'Mouse' -> #itemSelected;
		menu!

menuTwo
	^MenuBuilder new
		add: 'One' -> #itemSelected;
		add: 'Two' -> #itemSelected;
		add: 'Three' -> #itemSelected;
		add: 'Four' -> #itemSelected;
		menu!

selectMenu
	mode == #menuOne
		ifTrue: 
			[mode := #menuTwo.
			^self menuOne]
		ifFalse: 
			[mode := #menuOne.
			^self menuTwo]! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MenuDynamicCreate class
	instanceVariableNames: ''!


!MenuDynamicCreate class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Menu Dynamic Create' 
			#min: #(#Point 252 259 ) 
			#max: #(#Point 252 259 ) 
			#bounds: #(#Rectangle 518 422 770 681 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#MenuButtonSpec 
					#layout: #(#Rectangle 25 28 177 57 ) 
					#name: #menuButtonMB 
					#model: #menuButton 
					#label: 'Select Here' 
					#menu: #menuButtonMenu ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 25 77 177 106 ) 
					#name: #inputFieldIF 
					#model: #inputField 
					#menu: #inputFieldMenu ) 
				#(#TextEditorSpec 
					#layout: #(#Rectangle 25 126 177 223 ) 
					#name: #textEditorTE 
					#model: #textEditor 
					#menu: #textEditorMenu ) ) ) )! !

!MenuDynamicCreate class methodsFor: 'examples'!

example
	"This example shows menus being dynamically created 
	when the user goes to select an item."

	"self example"

	self open! !

ApplicationModel subclass: #WidgetAttributes
	instanceVariableNames: 'inputOne inputTwo '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-WidgetExamples'!
WidgetAttributes comment:
'This example shows widgets being modified in various ways. We look at enabling, disabling, making invisible and visible, making input fields read/write or read only, selecting text in different ways, changing the text of labels and changing the labels of action buttons.
'!


!WidgetAttributes methodsFor: 'actions'!

changeButtonOne
	| component |
	component := self builder componentAt: #buttonOneAB.
	component isVisible
		ifFalse: 
			[component beVisible.
			component enable]
		ifTrue: [component isEnabled
				ifTrue: [component disable]
				ifFalse: [component beInvisible]]!

changeButtonTwo
	| component newLabelString |
	component := self builder componentAt: #buttonTwoAB.
	newLabelString := component widget label text asString = 'Yes'
				ifTrue: ['No']
				ifFalse: ['Yes'].
	component labelString: newLabelString!

changeInputOne
	| component controller |
	component := self builder componentAt: #inputOneIF.
	controller := component widget controller.
	component isVisible
		ifFalse: 
			[component beVisible.
			controller readOnly: false.
			component takeKeyboardFocus.
			controller selectAt: 1.
			(controller findAndSelect: 'There')
				ifFalse: [controller text size > 6
						ifTrue: [controller selectFrom: 2 to: 5]
						ifFalse: [controller selectFrom: 1 to: controller text size + 1]]]
		ifTrue: [controller readOnly
				ifTrue: [component beInvisible]
				ifFalse: [controller readOnly: true]]!

changeInputTwo
	| component controller allSelected selectionStartIndex |
	component := self builder componentAt: #inputTwoIF.
	controller := component widget controller.
	allSelected := controller selection = controller text.
	selectionStartIndex := controller selectionStopIndex.
	component takeKeyboardFocus.
	allSelected
		ifTrue: [controller selectAt: 1]
		ifFalse: [selectionStartIndex == 1
				ifTrue: [controller selectAt: controller text size + 1]]!

changeLabel
	| component  |
	component := self builder componentAt: #labelLB. 
	component widget label text asString = 'Label'
		ifTrue: [component labelString: 'Label Two']
		ifFalse: [component labelString: 'Label']!

pressButton
	Dialog warn: 'You pressed me'! !

!WidgetAttributes methodsFor: 'aspects'!

inputOne
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^inputOne isNil
		ifTrue:
			[inputOne := 'Hi There' asValue]
		ifFalse:
			[inputOne]!

inputTwo
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^inputTwo isNil
		ifTrue:
			[inputTwo := 'Hi There' asValue]
		ifFalse:
			[inputTwo]! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

WidgetAttributes class
	instanceVariableNames: ''!


!WidgetAttributes class methodsFor: 'examples'!

example
	"This example shows widgets being modified in various ways.
	For example, enabled, disabled, invisible, label changes"

	"self example"
	self open! !

!WidgetAttributes class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Widget Attributes' 
			#min: #(#Point 504 371 ) 
			#max: #(#Point 504 371 ) 
			#bounds: #(#Rectangle 345 151 849 522 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 27 26 116 59 ) 
					#name: #buttonOneAB 
					#model: #pressButton 
					#label: 'Press Me' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 27 91 116 124 ) 
					#name: #buttonTwoAB 
					#model: #pressButton 
					#label: 'Yes' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 186 26 258 59 ) 
					#model: #changeButtonOne 
					#label: 'Change' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 186 91 258 124 ) 
					#model: #changeButtonTwo 
					#label: 'Change' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 186 152 258 185 ) 
					#model: #changeInputOne 
					#label: 'Change' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 186 209 258 242 ) 
					#model: #changeInputTwo 
					#label: 'Change' 
					#defaultable: true ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 27 156 127 181 ) 
					#name: #inputOneIF 
					#model: #inputOne ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 27 213 127 238 ) 
					#name: #inputTwoIF 
					#model: #inputTwo ) 
				#(#LabelSpec 
					#layout: #(#Point 27 270 ) 
					#name: #labelLB 
					#label: 'Label' ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 186 266 258 299 ) 
					#model: #changeLabel 
					#label: 'Change' 
					#defaultable: true ) 
				#(#DividerSpec 
					#layout: #(#Rectangle 10 73 485 77 ) ) 
				#(#DividerSpec 
					#layout: #(#Rectangle 10 138 485 142 ) ) 
				#(#DividerSpec 
					#layout: #(#Rectangle 10 195 485 199 ) ) 
				#(#DividerSpec 
					#layout: #(#Rectangle 10 252 485 256 ) ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 387 321 487 358 ) 
					#model: #closeRequest 
					#label: 'Close' 
					#defaultable: true ) 
				#(#DividerSpec 
					#layout: #(#Rectangle 12 309 487 313 ) ) 
				#(#LabelSpec 
					#layout: #(#Point 280 30 ) 
					#label: 'Enabled/Disabled/Invisible' ) 
				#(#LabelSpec 
					#layout: #(#Point 280 95 ) 
					#label: 'Yes/No' ) 
				#(#LabelSpec 
					#layout: #(#Point 280 270 ) 
					#label: 'Label/Label Two' ) 
				#(#LabelSpec 
					#layout: #(#Point 280 156 ) 
					#label: 'RW/RO/Invisible' ) 
				#(#LabelSpec 
					#layout: #(#Point 280 213 ) 
					#label: 'Selected/Begin/End' ) ) ) )! !

ApplicationModel subclass: #WidgetText
	instanceVariableNames: 'inputOne inputTwo normalFont normalEmphasis inputThree '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-WidgetExamples'!
WidgetText comment:
'This shows examples of changing the fonts, colors, and emphasis of text on or in widgets. In particular, we look at action buttons, input fields, and labels.
'!


!WidgetText methodsFor: 'actions'!

buttonPressed
	Dialog warn: 'You pressed me'!

changeColor
	| components lookPrefs |
	components := Array
				with: (self builder componentAt: #buttonOneAB)
				with: (self builder componentAt: #inputOneIF)
				with: (self builder componentAt: #labelOneLB).
	components
		do: 
			[:each | 
			lookPrefs := each lookPreferences.
			lookPrefs foregroundColor = ColorValue red
				ifTrue: 
					[lookPrefs setForegroundColor: ColorValue green.
					lookPrefs setBackgroundColor: ColorValue red]
				ifFalse: 
					[lookPrefs setForegroundColor: ColorValue red.
					lookPrefs setBackgroundColor: ColorValue green].
			each lookPreferences: lookPrefs]!

changeEmphasis
	self changeEmphasisForButtonAndLabel.
	self changeEmphasisForInputField.
	self normalEmphasis: self normalEmphasis not!

changeEmphasisForButtonAndLabel
	| label text components |
	components := Array with: (self builder componentAt: #buttonThreeAB)
				with: (self builder componentAt: #labelThreeLB).
	components
		do: 
			[:each | 
			label := each widget label.
			text := label text.
			self normalEmphasis
				ifTrue: [label text: (text emphasizeAllWith: (Array
								with: #italic
								with: #large
								with: #color -> ColorValue pink))]
				ifFalse: [label text: (text emphasizeAllWith: #bold)].
			each invalidate]!

changeEmphasisForInputField
	| text widget |
	widget := (self builder componentAt: #inputThreeIF) widget.
	text := widget controller text.
	self normalEmphasis
		ifTrue: [widget editText: (text emphasizeAllWith: (Array
						with: #italic
						with: #large
						with: #color -> ColorValue pink))]
		ifFalse: [widget editText: (text emphasizeAllWith: #bold)].
	widget invalidate!

changeFont
	| widgets textAttributes |
	widgets := Array
				with: (self builder componentAt: #buttonTwoAB) widget
				with: (self builder componentAt: #labelTwoLB) widget
				with: (self builder componentAt: #inputTwoIF) widget.
	widgets
		do: 
			[:each | 
			self normalFont
				ifTrue: [textAttributes := TextAttributes styleNamed: #large]
				ifFalse: [textAttributes := TextAttributes styleNamed: #small].
			each textStyle: textAttributes.
			each invalidate].
	self normalFont: self normalFont not! !

!WidgetText methodsFor: 'aspects'!

inputOne
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^inputOne isNil
		ifTrue:
			[inputOne := 'Input One' asValue]
		ifFalse:
			[inputOne]!

inputThree
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^inputThree isNil
		ifTrue:
			[inputThree := 'Input Three' asValue]
		ifFalse:
			[inputThree]!

inputTwo
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^inputTwo isNil
		ifTrue:
			[inputTwo := 'Input Two' asValue]
		ifFalse:
			[inputTwo]! !

!WidgetText methodsFor: 'accessing'!

normalEmphasis
	^normalEmphasis isNil
		ifTrue: [normalEmphasis := true]
		ifFalse: [normalEmphasis]!

normalEmphasis: aBoolean
	normalEmphasis := aBoolean!

normalFont
	^normalFont isNil
		ifTrue: [normalFont := true]
		ifFalse: [normalFont]!

normalFont: aBoolean
	normalFont := aBoolean! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

WidgetText class
	instanceVariableNames: ''!


!WidgetText class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Widget Text' 
			#min: #(#Point 504 284 ) 
			#max: #(#Point 504 284 ) 
			#bounds: #(#Rectangle 475 462 979 746 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 391 233 491 270 ) 
					#model: #closeRequest 
					#label: 'Close' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 15 16 161 57 ) 
					#model: #changeColor 
					#label: 'Change color' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 180 16 326 57 ) 
					#model: #changeFont 
					#label: 'Change Font' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 15 79 104 111 ) 
					#name: #buttonOneAB 
					#model: #buttonPressed 
					#label: 'Button One' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 180 79 304 112 ) 
					#name: #buttonTwoAB 
					#model: #buttonPressed 
					#label: 'Button Two' 
					#defaultable: true ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 15 133 115 158 ) 
					#name: #inputOneIF 
					#model: #inputOne ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 180 134 280 159 ) 
					#name: #inputTwoIF 
					#model: #inputTwo ) 
				#(#LabelSpec 
					#layout: #(#Point 15 180 ) 
					#name: #labelOneLB 
					#label: 'Label One' ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 345 16 491 57 ) 
					#model: #changeEmphasis 
					#label: 'Change Emphasis' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 345 79 452 111 ) 
					#name: #buttonThreeAB 
					#model: #buttonPressed 
					#label: 'Button Three' 
					#defaultable: true ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 345 133 460 157 ) 
					#name: #inputThreeIF 
					#model: #inputThree ) 
				#(#LabelSpec 
					#layout: #(#Rectangle 345 179 462 204 ) 
					#name: #labelThreeLB 
					#label: 'Label Three' ) 
				#(#DividerSpec 
					#layout: #(#Rectangle 170 9 270 217 ) 
					#orientation: #vertical ) 
				#(#DividerSpec 
					#layout: #(#Rectangle 334 9 434 217 ) 
					#orientation: #vertical ) 
				#(#LabelSpec 
					#layout: #(#Rectangle 180 181 280 206 ) 
					#name: #labelTwoLB 
					#label: 'Label Two' ) ) ) )! !

!WidgetText class methodsFor: 'examples'!

example
	"This shows examples of changing the fonts, colors, 
	and emphasis of text on or in widgets"

	"self example"
	self open! !

ApplicationModel subclass: #MenuDynamicModify
	instanceVariableNames: 'menuButton menuButtonMenuVH inputField inputFieldMenuVH textEditor textEditorMenuVH mode '
	classVariableNames: 'MenuBarVH '
	poolDictionaries: ''
	category: 'STBE-WidgetExamples'!
MenuDynamicModify comment:
'This class demonstrates some of the ways to dynamically modify a menu. We cycle through:
	Creating a new menu
	Disabling a menu item
	Hiding a menu item
	Coloring a menu item
	Adding an item to the end of the menu
	Removing the item from the menu'!


!MenuDynamicModify methodsFor: 'support'!

changeMenu: aMenu item: aMenuItem 
	mode == #disabled ifTrue: [aMenuItem disable].
	mode == #invisible ifTrue: [aMenu hideItem: aMenuItem].
	mode == #addTo
		ifTrue: 
			[aMenu backgroundColor: ColorValue white.
			aMenu addItemLabel: '--Added--' value: #itemSelected].
	mode == #removeFrom ifTrue: 
		[aMenu removeItem: (aMenu menuItemLabeled: '--Added--')].
	mode == #colored
		ifTrue: 
			[aMenu unhideItem: aMenuItem.
			aMenuItem enable.
			aMenu backgroundColor: ColorValue green.
			(aMenu menuItemLabeled: 'Three')
				color: ColorValue red].!

changeMenuBar
	| subMenu |
	subMenu := (MenuBarVH value menuItemLabeled: 'Select') submenu.
	self changeMenu: subMenu item: (subMenu menuItemLabeled: 'One')!

changeMenuFor: aMenu
	self changeMenu: aMenu item: (aMenu menuItemLabeled: 'One')!

changeMode
	mode == #removeFrom ifTrue: [mode := nil].
	mode == #addTo ifTrue: [mode := #removeFrom].
	mode == #colored ifTrue: [mode := #addTo].
	mode == #invisible ifTrue: [mode := #colored].
	mode == #disabled ifTrue: [mode := #invisible].
	mode == #enableMenuTwo ifTrue: [mode := #disabled].
	mode == #enableMenuOne ifTrue: [mode := #enableMenuTwo].
	mode == nil ifTrue: [mode := #enableMenuOne].!

installNewMenu
	mode == #enableMenuOne
		ifTrue: 
			[menuButtonMenuVH value: self menuOne.
			inputFieldMenuVH value: self menuOne.
			textEditorMenuVH value: self menuOne.
			MenuBarVH value: self class menuOne].
	mode == #enableMenuTwo
		ifTrue: 
			[menuButtonMenuVH value: self menuTwo.
			inputFieldMenuVH value: self menuTwo.
			textEditorMenuVH value: self menuTwo.
			MenuBarVH value: self class menuTwo]!

itemSelected
	Dialog warn: 'You selected a menu item'! !

!MenuDynamicModify methodsFor: 'actions'!

change
	self changeMode.
	(mode == #enableMenuOne or: [mode == #enableMenuTwo])
		ifTrue: [self installNewMenu]
		ifFalse: 
			[self changeMenuBar.
			self changeMenuFor: (self builder menuAt: #menuButtonMenuVH) value.
			self changeMenuFor: (self builder menuAt: #inputFieldMenuVH) value.
			self changeMenuFor: (self builder menuAt: #textEditorMenuVH) value]! !

!MenuDynamicModify methodsFor: 'initialize-release'!

initialize
	menuButton := nil asValue.
	inputField := String new asValue.
	textEditor := String new asValue.
	menuButtonMenuVH := self menuOne asValue.
	inputFieldMenuVH := self menuOne asValue.
	textEditorMenuVH := self menuOne asValue.
	MenuBarVH := self class menuOne asValue.
	mode := #enableMenuOne.
	menuButton onChangeSend: #itemSelected to: self! !

!MenuDynamicModify methodsFor: 'aspects'!

inputField
	^inputField!

inputFieldMenuVH
	^inputFieldMenuVH!

menuButton
	^menuButton!

menuButtonMenuVH
	^menuButtonMenuVH!

textEditor
	^textEditor!

textEditorMenuVH
	^textEditorMenuVH! !

!MenuDynamicModify methodsFor: 'menus'!

menuOne
	^MenuBuilder new
		add: 'Dog' -> #itemSelected;
		add: 'Cat' -> #itemSelected;
		add: 'Elephant' -> #itemSelected;
		add: 'Mouse' -> #itemSelected;
		menu!

menuTwo
	^MenuBuilder new
		add: 'One' -> #itemSelected;
		add: 'Two' -> #itemSelected;
		add: 'Three' -> #itemSelected;
		add: 'Four' -> #itemSelected;
		menu! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MenuDynamicModify class
	instanceVariableNames: ''!


!MenuDynamicModify class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Menu Dynamic Modify' 
			#min: #(#Point 354 294 ) 
			#max: #(#Point 354 294 ) 
			#bounds: #(#Rectangle 538 336 892 630 ) 
			#flags: 4 
			#menu: #menuBar ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#MenuButtonSpec 
					#layout: #(#Rectangle 25 28 177 57 ) 
					#model: #menuButton 
					#label: 'Select Here' 
					#menu: #menuButtonMenuVH ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 25 77 177 106 ) 
					#model: #inputField 
					#menu: #inputFieldMenuVH ) 
				#(#TextEditorSpec 
					#layout: #(#Rectangle 25 126 177 223 ) 
					#model: #textEditor 
					#menu: #textEditorMenuVH ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 227 210 315 252 ) 
					#name: #changeAB 
					#model: #change 
					#label: 'Change' 
					#defaultable: true ) 
				#(#LabelSpec 
					#layout: #(#Point 212 30 ) 
					#label: 'Cycle between:' ) 
				#(#LabelSpec 
					#layout: #(#Point 227 56 ) 
					#label: 'New Menu' ) 
				#(#LabelSpec 
					#layout: #(#Point 227 79 ) 
					#label: 'Disabled' ) 
				#(#LabelSpec 
					#layout: #(#Point 227 102 ) 
					#label: 'Hidden' ) 
				#(#LabelSpec 
					#layout: #(#Point 227 125 ) 
					#label: 'Colored' ) 
				#(#LabelSpec 
					#layout: #(#Point 227 148 ) 
					#label: 'New Item' ) 
				#(#LabelSpec 
					#layout: #(#Point 227 171 ) 
					#label: 'Item Removed' ) ) ) )! !

!MenuDynamicModify class methodsFor: 'resources'!

menuBar
	^MenuBarVH!

menuOne
	| mb   |
	mb := MenuBuilder new.
	mb 
		beginSubMenuLabeled: 'Select'; 
		add: 'Dog' -> #itemSelected; 
		add: 'Cat' -> #itemSelected; 
		add: 'Elephant' -> #itemSelected;
		add: 'Mouse' -> #itemSelected;
		endSubMenu.
	^mb menu!

menuTwo
	| mb   |
	mb := MenuBuilder new.
	mb 
		beginSubMenuLabeled: 'Select'; 
		add: 'One' -> #itemSelected; 
		add: 'Two' -> #itemSelected; 
		add: 'Three' -> #itemSelected;
		add: 'Four' -> #itemSelected;
		endSubMenu.
	^mb menu! !

!MenuDynamicModify class methodsFor: 'examples'!

example
	"This example shows menus being modified dynamically"

	"self example"

	self open! !

