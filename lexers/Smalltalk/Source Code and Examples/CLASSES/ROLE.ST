Object subclass: #RolePlayer
	instanceVariableNames: 'role '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Roles'!
RolePlayer comment:
'I am an object that can take on various roles. When messages are sent to me I try to respond to them. If I am unable to respond, I assume the message is directed at the role I am currently playing and I ask the role to respond.

Instance Variables:

	role		<Role>		Actually a subclass of Role. If I don''t understand a message
						and the role is non nil, I ask the role to respond to the message.
'!


!RolePlayer methodsFor: 'roles'!

doesNotUnderstand: aMessage 
	role notNil ifTrue: 
		[^role 
			perform: aMessage selector 
			withArguments: aMessage arguments]! !

!RolePlayer methodsFor: 'accessing'!

role: aRole
	role := aRole! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

RolePlayer class
	instanceVariableNames: ''!


!RolePlayer class methodsFor: 'examples'!

example
	"self example"

	| person |
	person := self new.
	person role: ChauffeurRole new.
	person pickUpKids.
	person role: ShopperRole new.
	person buyGroceries.! !

Object subclass: #Role
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Roles'!
Role comment:
'I am the superclass of all roles.'!


Role subclass: #ShopperRole
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Roles'!
ShopperRole comment:
'I contain behavior specific to being a shopper.'!


!ShopperRole methodsFor: 'processing'!

buyGroceries
	Transcript cr; show: 'I am buying groceries'! !

Role subclass: #ChauffeurRole
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Roles'!
ChauffeurRole comment:
'I contain behavior specific to being a chauffeur.'!


!ChauffeurRole methodsFor: 'processing'!

pickUpKids
	Transcript cr; show: 'I am picking up the children'! !

