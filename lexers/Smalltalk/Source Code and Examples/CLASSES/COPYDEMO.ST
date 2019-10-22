Object subclass: #MyAddress
	instanceVariableNames: 'streetAddress city '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-CopyDemo'!
MyAddress comment:
'I am used in support of the copying demonstration with MyPerson'!


!MyAddress methodsFor: 'accessing'!

city
	^city!

city: aCity
	city := aCity!

streetAddress
	^streetAddress!

streetAddress: aString
	streetAddress := aString! !

Object subclass: #MyPerson
	instanceVariableNames: 'name address '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-CopyDemo'!
MyPerson comment:
'I exist to demonstrate the difference that implementing postCopy makes. When we copy a person and modify the address of the copied person, the original person also gets the change unless we implement postCopy.
'!


!MyPerson methodsFor: 'accessing'!

address
	^address!

address: anAddress
	address := anAddress!

name
	^name!

name: aName
	name := aName! !

!MyPerson methodsFor: 'copying'!

postCopy
	super postCopy.
	address := address copy! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyPerson class
	instanceVariableNames: ''!


!MyPerson class methodsFor: 'examples'!

exampleOne
	"In this example, we do a shallow copy.
	When personTwo's address changes, personOne also gets the change"
	
	"self exampleOne"

	| address personOne personTwo |
	(address := MyAddress new)
		streetAddress: '916 Smith Avenue';
		city: 'Boulder'.
	(personOne := MyPerson new)
		name: 'Alec Sharp';
		address: address.
	personTwo := personOne shallowCopy.
	personTwo address streetAddress: '221 Jones Court'.
	personOne address streetAddress inspect!

exampleTwo
	"In this example, we do a copy which invokes the postCopy we implement.
	When personTwo's address changes, personOne does NOT get the change"

	"self exampleTwo"

	| address personOne personTwo |
	(address := MyAddress new)
		streetAddress: '916 Smith Avenue';
		city: 'Boulder'.
	(personOne := MyPerson new)
		name: 'Alec Sharp';
		address: address.
	personTwo := personOne copy.
	personTwo address streetAddress: '221 Jones Court'.
	personOne address streetAddress inspect! !

