ApplicationModel subclass: #MyView
	instanceVariableNames: 'person '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyView class
	instanceVariableNames: ''!


!MyView class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'View' 
			#min: #(#Point 140 113 ) 
			#max: #(#Point 140 113 ) 
			#bounds: #(#Rectangle 726 566 866 679 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 27 0 -11 1 48 0 ) 
					#model: #name 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 76 0 -11 1 97 0 ) 
					#model: #age 
					#isReadOnly: true 
					#type: #number ) 
				#(#LabelSpec 
					#layout: #(#Point 9 4 ) 
					#label: 'Name' ) 
				#(#LabelSpec 
					#layout: #(#Point 9 51 ) 
					#label: 'Age' ) ) ) )! !

!MyView class methodsFor: 'interface opening'!

open: aModel
	^self openOn: (self new initialize: aModel)! !

MyView subclass: #MyView8
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyView8 methodsFor: 'aspects'!

age
	^(PluggableAdaptor on: person)
		getBlock: [:model | model age * 10]
		putBlock: [:model :aValue | ]
		updateBlock: [:model :aspect :parameter | aspect == #age]!

name
	^(PluggableAdaptor on: person)
		getBlock: [:model | model name asUppercase]
		putBlock: [:model :aValue | ]
		updateBlock: [:model :aspect :parameter | aspect == #name ]! !

!MyView8 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson.! !

MyView subclass: #MyView1
	instanceVariableNames: 'name age '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyView1 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson.
	person addDependent: self.
	name := String new asValue.
	age := 0 asValue.! !

!MyView1 methodsFor: 'updating'!

update: aSymbol with: aValue from: anObject 
	"We have to check what change happened so that we know what 
	action to take"

	aSymbol == #name ifTrue: [self name value: aValue].
	aSymbol == #age ifTrue: [self age value: aValue]! !

!MyView1 methodsFor: 'aspects'!

age
	^age!

name
	^name! !

MyView subclass: #MyView3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyView3 methodsFor: 'aspects'!

age
	^(AspectAdaptor subject: person sendsUpdates: true)
		forAspect: #age.!

name
	^(AspectAdaptor subject: person sendsUpdates: true)
		forAspect: #name.! !

!MyView3 methodsFor: 'initialize-release'!

initialize: aPerson
	person := aPerson! !

ApplicationModel subclass: #MyApp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyApp methodsFor: 'initialize-release'!

initialize: aNumber 
	| model window modelClass inputClass viewClass windowType |
	modelClass := Smalltalk at: ('MyPerson' , aNumber printString) asSymbol.
	inputClass := Smalltalk at: ('MyInput' , aNumber printString) asSymbol.
	viewClass := Smalltalk at: ('MyView' , aNumber printString) asSymbol.
	model := modelClass new.
	1 to: 4
		do: 
			[:index | 
			windowType := index == 4
						ifTrue: [inputClass]
						ifFalse: [viewClass].
			window := (windowType open: model) window.
			window application: self.
			window bePartner]! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyApp class
	instanceVariableNames: ''!


!MyApp class methodsFor: 'examples'!

exampleEight
	"self exampleEight"

	self newExample: 8!

exampleFive
	"self exampleFive"

	self newExample: 5!

exampleFour
	"self exampleFour"

	self newExample: 4!

exampleOne
	"self exampleOne"

	self newExample: 1!

exampleSeven
	"self exampleSeven"

	self newExample: 7!

exampleSix
	"self exampleSix"

	self newExample: 6!

exampleThree
	"self exampleThree"

	self newExample: 3!

exampleTwo
	"self exampleTwo"

	self newExample: 2! !

!MyApp class methodsFor: 'instance creation'!

newExample: aNumber
	^super new initialize: aNumber! !

MyView subclass: #MyView6
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyView6 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson asValue.! !

!MyView6 methodsFor: 'aspects'!

age
	^(AspectAdaptor subject: person sendsUpdates: true)
		forAspect: #age.!

name
	^(AspectAdaptor subject: person sendsUpdates: true)
		forAspect: #name.!

person
	^person! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyView6 class
	instanceVariableNames: ''!


!MyView6 class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'View' 
			#min: #(#Point 140 113 ) 
			#max: #(#Point 140 113 ) 
			#bounds: #(#Rectangle 664 454 804 567 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 27 0 -11 1 48 0 ) 
					#model: #'person name' 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 76 0 -11 1 97 0 ) 
					#model: #'person age' 
					#isReadOnly: true 
					#type: #number ) 
				#(#LabelSpec 
					#layout: #(#Point 9 4 ) 
					#label: 'Name' ) 
				#(#LabelSpec 
					#layout: #(#Point 9 51 ) 
					#label: 'Age' ) ) ) )! !

MyView subclass: #MyView4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyView4 methodsFor: 'aspects'!

age
	| adaptor |
	adaptor := AspectAdaptor subjectChannel: person sendsUpdates: true.
	adaptor
		accessWith: #yearsOld
		assignWith: #yearsOld:
		aspect: #age.
	^adaptor!

name
	| adaptor |
	adaptor := AspectAdaptor subjectChannel: person sendsUpdates: true.
	adaptor
		accessWith: #called
		assignWith: #called:
		aspect: #name.
	^adaptor! !

!MyView4 methodsFor: 'initialize-release'!

initialize: aPerson 
	"Note that in this case the model is contained in a ValueHolder and 
	we use subjectChannel rather than subject"

	person := aPerson asValue! !

MyView subclass: #MyView2
	instanceVariableNames: 'name age '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyView2 methodsFor: 'initialize-release'!

initialize: aPerson
	person := aPerson.
	person name onChangeSend: #nameChanged to: self.
	person age onChangeSend: #ageChanged to: self.
	name := String new asValue.
	age := 0 asValue.! !

!MyView2 methodsFor: 'changes'!

ageChanged
	self age value: person age value.!

nameChanged
	self name value: person name value.! !

!MyView2 methodsFor: 'aspects'!

age
	^age!

name
	^name! !

Model subclass: #MyPerson
	instanceVariableNames: 'name age '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


MyPerson subclass: #MyPerson3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyPerson3 methodsFor: 'accessing'!

age
	^age!

age: aValue 
	"We send changed: rather than changed:with: because the 
	mechanism sends a message back to us asking for the new value"

	age := aValue.
	self changed: #age!

name
	^name!

name: aValue 
	"We send changed: rather than changed:with: because the 
	mechanism sends a message back to us asking for the new value"

	name := aValue.
	self changed: #name! !

MyPerson subclass: #MyPerson6
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyPerson6 methodsFor: 'accessing'!

age
	^age!

age: aValue 
	age := aValue.
	Transcript cr; show: 'Age changed'.
	self changed: #age!

name
	^name!

name: aValue 
	name := aValue.
	Transcript cr; show: 'Name changed'.
	self changed: #name! !

MyPerson subclass: #MyPerson5
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyPerson5 methodsFor: 'accessing'!

age
	^age!

age: aValue
	age := aValue.
	self changed: #age!

name
	^name!

name: aValue
	name := aValue.
	self changed: #name! !

MyPerson subclass: #MyPerson7
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyPerson7 methodsFor: 'accessing'!

age
	^age!

age: aValue 
	age := aValue.
	Transcript cr; show: 'Age changed'.
	self changed: #age!

name
	^name!

name: aValue 
	name := aValue.
	Transcript cr; show: 'Name changed'.
	self changed: #name! !

MyPerson subclass: #MyPerson8
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyPerson8 methodsFor: 'accessing'!

age
	^age isNil
		ifTrue: [age := 0]
		ifFalse: [age]!

age: aNumber
	age := aNumber.
	self changed: #age!

name
	^name isNil
		ifTrue: [name := String new]
		ifFalse: [name]!

name: aString
	name := aString.
	self changed: #name! !

MyPerson subclass: #MyPerson1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyPerson1 methodsFor: 'accessing'!

age: aValue 
	"We send changed:with: so that the dependents of the model don't 
	have to ask the model for the new value"

	age := aValue.
	self changed: #age with: age!

name: aValue 
	"We send changed:with: so that the dependents of the model don't 
	have to ask the model for the new value"

	name := aValue.
	self changed: #name with: name! !

MyPerson subclass: #MyPerson4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyPerson4 methodsFor: 'accessing'!

called
	^name!

called: aValue
	name := aValue.
	self changed: #name!

yearsOld
	^age!

yearsOld: aValue
	age := aValue.
	self changed: #age! !

MyView subclass: #MyView5
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyView5 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson! !

!MyView5 methodsFor: 'aspects'!

age
	^(AspectAdaptor subject: person sendsUpdates: true)
		forAspect: #age.!

name
	^(AspectAdaptor subject: person sendsUpdates: true)
		forAspect: #name.! !

ApplicationModel subclass: #MyInput
	instanceVariableNames: 'person '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!
MyInput comment:
'Window registers itself as a dependent of Model by sending Model #addDependent:. Window''s fields are ValueHolders. Window sends #onChangeSend: to the ValueHolders.

When the value changes, the ValueHolder sends Window a message and Window sets Model''s value. Model sends #changed:with: to itself, which propagates out to its dependents as #update:with:from. 

Window gets this message and has to check to see what symbol it was sent. Window then does the appropriate thing.
'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyInput class
	instanceVariableNames: ''!


!MyInput class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Input' 
			#min: #(#Point 140 113 ) 
			#max: #(#Point 140 113 ) 
			#bounds: #(#Rectangle 369 417 509 530 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 27 0 -11 1 48 0 ) 
					#model: #name 
					#isReadOnly: false ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 76 0 -11 1 97 0 ) 
					#model: #age 
					#isReadOnly: false 
					#type: #number ) 
				#(#LabelSpec 
					#layout: #(#Point 9 4 ) 
					#label: 'Name' ) 
				#(#LabelSpec 
					#layout: #(#Point 9 51 ) 
					#label: 'Age' ) ) ) )! !

!MyInput class methodsFor: 'interface opening'!

open: aModel
	^self openOn: (self new initialize: aModel)! !

MyInput subclass: #MyInput2
	instanceVariableNames: 'name age '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!
MyInput2 comment:
'Window''s fields are ValueHolders. Window sends #onChangeSend: to the ValueHolders. Model''s fields are also ValueHolders, and Window also sends #onChangeSend: to Model''s fields.

When Window''s fields change, the ValueHolder sends Window a message and Window sets Model''s value. Model''s value then sends a message to its dependents (in this case Window which registered by sending #onChangeSend:). When Window receives the message, it displays its fields appropriately.
 '!


!MyInput2 methodsFor: 'aspects'!

age
	^age!

name
	^name! !

!MyInput2 methodsFor: 'initialize-release'!

initialize: aPerson
	person := aPerson.
	name := String new asValue.
	name onChangeSend: #nameChanged to: self.
	age := 0 asValue.
	age onChangeSend: #ageChanged to: self.! !

!MyInput2 methodsFor: 'changes'!

ageChanged
	person age value: self age value!

nameChanged
	person name value: self name value! !

MyInput subclass: #MyInput1
	instanceVariableNames: 'name age '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyInput1 methodsFor: 'changes'!

ageChanged
	person age: self age value!

nameChanged
	person name: self name value! !

!MyInput1 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson.
	name := String new asValue.
	name onChangeSend: #nameChanged to: self.
	age := 0 asValue.
	age onChangeSend: #ageChanged to: self! !

!MyInput1 methodsFor: 'aspects'!

age
	^age!

name
	^name! !

MyInput subclass: #MyInput3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!
MyInput3 comment:
'Window''s fields are AspectAdaptors with Model as their subject and Model''s instance variables as their aspects. When Window''s values change, the appropriate changes are sent to Model. Model sends itself a #changed: message, which is propagated out to the AspectAdaptors that registered as dependents.

The AspectAdaptors then update Window''s fields.'!


!MyInput3 methodsFor: 'aspects'!

age
	^(AspectAdaptor subject: person sendsUpdates: true)
		forAspect: #age.!

name
	^(AspectAdaptor subject: person sendsUpdates: true)
		forAspect: #name.! !

!MyInput3 methodsFor: 'initialize-release'!

initialize: aPerson
	person := aPerson.! !

MyInput subclass: #MyInput5
	instanceVariableNames: 'trigger '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!
MyInput5 comment:
'Uses BufferedValueHolders to do buffering. This means that no fields are updated until the trigger is set to true.'!


!MyInput5 methodsFor: 'aspects'!

age
	| adaptor |
	adaptor := AspectAdaptor subject: person sendsUpdates: true.
	adaptor forAspect: #age.
	^BufferedValueHolder subject: adaptor triggerChannel: trigger.!

name
	| adaptor |
	adaptor := AspectAdaptor subject: person sendsUpdates: true.
	adaptor forAspect: #name.
	^BufferedValueHolder subject: adaptor triggerChannel: trigger.! !

!MyInput5 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson.
	trigger := false asValue! !

!MyInput5 methodsFor: 'actions'!

accept
	trigger value: true! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyInput5 class
	instanceVariableNames: ''!


!MyInput5 class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Input' 
			#min: #(#Point 130 167 ) 
			#max: #(#Point 130 167 ) 
			#bounds: #(#Rectangle 738 432 868 599 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0 27 0 -10 1 48 0 ) 
					#model: #name 
					#isReadOnly: false ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0 84 0 -10 1 105 0 ) 
					#model: #age 
					#isReadOnly: false 
					#type: #number ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 30 0 -50 1 -30 1 -10 1 ) 
					#model: #accept 
					#label: 'Accept' 
					#defaultable: true ) 
				#(#LabelSpec 
					#layout: #(#Point 10 1 ) 
					#label: 'Name' ) 
				#(#LabelSpec 
					#layout: #(#Point 10 56 ) 
					#label: 'Age' ) ) ) )! !

MyInput subclass: #MyInput7
	instanceVariableNames: 'trigger '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyInput7 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson asValue.
	trigger := false asValue.! !

!MyInput7 methodsFor: 'actions'!

accept
	trigger value: true.! !

!MyInput7 methodsFor: 'aspects'!

person
	^person!

trigger
	^trigger! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyInput7 class
	instanceVariableNames: ''!


!MyInput7 class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Input' 
			#min: #(#Point 130 167 ) 
			#max: #(#Point 130 167 ) 
			#bounds: #(#Rectangle 738 432 868 599 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0 27 0 -10 1 48 0 ) 
					#model: #'person name | trigger' 
					#isReadOnly: false ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 10 0 84 0 -10 1 105 0 ) 
					#model: #'person age | trigger' 
					#isReadOnly: false 
					#type: #number ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 30 0 -50 1 -30 1 -10 1 ) 
					#model: #accept 
					#label: 'Accept' 
					#defaultable: true ) 
				#(#LabelSpec 
					#layout: #(#Point 10 1 ) 
					#label: 'Name' ) 
				#(#LabelSpec 
					#layout: #(#Point 10 56 ) 
					#label: 'Age' ) ) ) )! !

MyInput subclass: #MyInput6
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!
MyInput6 comment:
'Uses Aspect paths, which means that there is a very tight relationship between the model, view, and controller. Basically, we can''t get other views to update.'!


!MyInput6 methodsFor: 'aspects'!

person
	^person! !

!MyInput6 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson asValue.! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyInput6 class
	instanceVariableNames: ''!


!MyInput6 class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Input' 
			#min: #(#Point 140 113 ) 
			#max: #(#Point 140 113 ) 
			#bounds: #(#Rectangle 829 163 969 276 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 27 0 -11 1 48 0 ) 
					#model: #'person name' 
					#isReadOnly: false ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 76 0 -11 1 97 0 ) 
					#model: #'person age' 
					#isReadOnly: false 
					#type: #number ) 
				#(#LabelSpec 
					#layout: #(#Point 9 4 ) 
					#label: 'Name' ) 
				#(#LabelSpec 
					#layout: #(#Point 9 51 ) 
					#label: 'Age' ) ) ) )! !

MyInput subclass: #MyInput8
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!
MyInput8 comment:
'This example uses PluggableAdaptors. It sets up a PluggableAdaptor on the model''s value fields, which are ValueHolders. We specify three blocks for the adaptor: a get block, a put block (which manipulates the data before storing it), and an update block which tells whether to send updates out.'!


!MyInput8 methodsFor: 'aspects'!

age
	^(PluggableAdaptor on: person)
		getBlock: [:model | model age]
		putBlock: [:model :aValue | model age: aValue * 3]
		updateBlock: [:model :aspect :parameter | aspect == #age]!

name
	^(PluggableAdaptor on: person)
		getBlock: [:model | model name]
		putBlock: [:model :aValue | model name: aValue asLowercase]
		updateBlock: [:model :aspect :parameter | aspect == #name]! !

!MyInput8 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson.! !

MyInput subclass: #MyInput4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!
MyInput4 comment:
'Window''s fields are AspectAdaptors with Model as their subject and Model''s instance variables as their aspects. When Window''s values change, the appropriate changes are sent to Model. 

However, the 4 classes differ from the 3 classes in two respects. First, the model is stored in a ValueHolder in the Window''s instance variable. Second, the accessing methods for Model''s variables have different names from the variables.

Model sends itself a #changed: message, which is propagated out to the AspectAdaptors that registered as dependents.

The AspectAdaptors then update Window''s fields.
'!


!MyInput4 methodsFor: 'aspects'!

age
	| adaptor |
	adaptor := AspectAdaptor subjectChannel: person sendsUpdates: true.
	adaptor
		accessWith: #yearsOld
		assignWith: #yearsOld:
		aspect: #age.
	^adaptor!

name
	| adaptor |
	adaptor := AspectAdaptor subjectChannel: person sendsUpdates: true.
	adaptor
		accessWith: #called
		assignWith: #called:
		aspect: #name.
	^adaptor! !

!MyInput4 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson asValue! !

MyPerson subclass: #MyPerson2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyPerson2 methodsFor: 'accessing'!

age
	^age isNil
		ifTrue: [age := 0 asValue]
		ifFalse: [age]!

name
	^name isNil
		ifTrue: [name := String new asValue]
		ifFalse: [name]! !

MyView subclass: #MyView7
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-MvcDependencies'!


!MyView7 methodsFor: 'aspects'!

person
	^person! !

!MyView7 methodsFor: 'initialize-release'!

initialize: aPerson 
	person := aPerson asValue.! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyView7 class
	instanceVariableNames: ''!


!MyView7 class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'View' 
			#min: #(#Point 140 113 ) 
			#max: #(#Point 140 113 ) 
			#bounds: #(#Rectangle 661 444 801 557 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 27 0 -11 1 48 0 ) 
					#model: #'person name' 
					#isReadOnly: true ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 9 0 76 0 -11 1 97 0 ) 
					#model: #'person age' 
					#isReadOnly: true 
					#type: #number ) 
				#(#LabelSpec 
					#layout: #(#Point 9 4 ) 
					#label: 'Name' ) 
				#(#LabelSpec 
					#layout: #(#Point 9 51 ) 
					#label: 'Age' ) ) ) )! !

