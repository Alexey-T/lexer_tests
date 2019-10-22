"This requires that the Tim Howard extensions to VisualWorks be already loaded. Otherwise you will get a message about nil superclasses"

DomainAdaptor subclass: #AsDomainAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Frameworks'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

AsDomainAdaptor class
	instanceVariableNames: ''!


!AsDomainAdaptor class methodsFor: 'interface opening'!

editAsDialog: aDomainObject 
	| instance |
	^(instance := self domain: aDomainObject) openAsDialog
		ifTrue: [instance domain]
		ifFalse: [nil]! !

ExtendedApplicationModel subclass: #AsExtendedApplicationModel
	instanceVariableNames: 'childApplications '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Frameworks'!


!AsExtendedApplicationModel methodsFor: 'closing'!

childApplicationsCanClose
	"We don't know if child windows will override changeRequest
	or requestForWindowClose, so we'll check both"

	| childrenThatCantClose |
	childrenThatCantClose := childApplications 
		select: [:each | each changeRequest == false 
					or: [ each requestForWindowClose == false]].
	^childrenThatCantClose isEmpty!

noticeOfWindowClose: aWindow 
	"Assume that if we have parent application, it's part of the framework"

	self parentApplication notNil 
		ifTrue: [self parentApplication removeChild: self].
	childApplications do: 
		[:each | "Don't send closeRequest to subcanvases"
			each builder window == self builder window 
				ifFalse: [each closeRequest]]!

requestForWindowClose
	^super requestForWindowClose  
		ifTrue: [self childApplicationsCanClose]
		ifFalse: [false]! !

!AsExtendedApplicationModel methodsFor: 'child support'!

addChild: anApplicationModel
	^childApplications add: anApplicationModel.!

removeChild: anApplicationModel
	^childApplications remove: anApplicationModel ifAbsent: [nil]! !

!AsExtendedApplicationModel methodsFor: 'accessing'!

parentApplication: anApplicationModel 
	"Assume that if we are given a parent, the parent is part of the framework"

	super parentApplication: anApplicationModel.
	anApplicationModel notNil 
		ifTrue: [anApplicationModel addChild: self]! !

!AsExtendedApplicationModel methodsFor: 'initialize-release'!

initialize
	super initialize.
	childApplications := OrderedCollection new.! !

DomainObject subclass: #AsDomainObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Frameworks'!


