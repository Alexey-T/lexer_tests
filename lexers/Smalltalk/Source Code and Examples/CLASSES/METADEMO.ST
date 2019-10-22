'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 4, 1996 at 5:12:01 pm'!

Object subclass: #MetaDemo
	instanceVariableNames: 'IV1 IV2 '
	classVariableNames: 'CV1 CV2 '
	poolDictionaries: ''
	category: 'STBE-MetaclassDemo'!
MetaDemo comment:
'Run the example code on the class side, looking at the output to the Transcript. Compare this with the code that generates the output. When the two inspectors come up, take a look at the values of the different variables.
'!


!MetaDemo methodsFor: 'methods'!

intanceMethodOne!

intanceMethodThree!

intanceMethodTwo! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MetaDemo class
	instanceVariableNames: 'CIV1 CIV2 '!


!MetaDemo class methodsFor: 'examples'!

example
	"self example"

	"Showing instance methods defined by the class and its metaclass"
	Transcript cr; show: self printString, ' methods:'.
	self selectors do: [ :each | Transcript crtab; show: each asString ].
	Transcript cr; show: self class printString, ' methods:'.
	self class selectors do: [ :each | Transcript crtab; show: each asString ].

	"Showing instance and class variables defined by the class and its metaclass"
	Transcript cr; show: self printString, ' instance variables:'.
	self instVarNames do: [ :each | Transcript crtab; show: each ].
	Transcript cr; show: self printString, ' class variables:'.
	self classVarNames do: [ :each | Transcript crtab; show: each ].
	Transcript cr; show: self printString, ' class instance variables:'.
	self class instVarNames do: [ :each | Transcript crtab; show: each ].

	self inspect.
	self class inspect! !

!MetaDemo class methodsFor: 'methods'!

classMethodOne!

classMethodTwo! !
