Object subclass: #ImageCreator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ImageCreator'!
ImageCreator comment:
'I create a production image by filing in the application code, running MyStripper, then saving the image. Note that doing all this is automatic. You simply have to file me and MyStripper into the image, then save it, possible by doing ImageCreator saveImage. When you start up this new image, it will automatically create a production image.'!


!ImageCreator methodsFor: 'names'!

myApplication
	"Return the class that starts up the application"

	^MyApp!

myImageName
	"Return the name of the image to save"

	^'myapp' copy!

myInstallFile
	"Return the filename that files in the application code"

	^'install.st' asFilename!

myLauncherClass
	"We may be using a subclass of VisualLauncher as our Launcher"

	^VisualLauncher! !

!ImageCreator methodsFor: 'creating image'!

createImage
	ObjectMemory globalGarbageCollect.
	self myRemoveSelfAsDependent.
	self myCloseOtherWindows.
	self myFileInApplication.
	self myRunStripper.
	self myOpenApplication.
	self myCloseLauncher.
	self myInstallEmergencyHandler.
	self mySaveImageAndQuit!

myCloseLauncher
	"Close any open Launchers"

	self myLauncherClass allInstances 
		do: [:each | each builder window controller closeAndUnschedule].!

myCloseOtherWindows
	"Close any open windows that aren't Launchers"

	ScheduledControllers scheduledControllers 
		do: [:each | each model class ~~ self myLauncherClass 
			ifTrue: [each closeAndUnschedule]]!

myFileInApplication
	"File in the application code."

	self myInstallFile fileIn.!

myInstallEmergencyHandler
	"Install the application Emergency Handler."

	Exception emergencyHandler: 
		[:ex :context | self handleException: ex context: context]!

myOpenApplication
	"Open the application. We need to make sure that the user is not prompted for the window position. 
	When you accept this method, it will ask you to declare your application class. Make it Undeclared."

	| prompt |
	prompt := ScheduledWindow promptForOpen.
	ScheduledWindow promptForOpen: false.
	self myApplication open.
	ScheduledWindow promptForOpen: prompt!

myRemoveSelfAsDependent
	"Remove ourself as a dependent of ObjectMemory. 
	We don't want this code to run every time the application is started."

	(ObjectMemory dependents includes: self class)
		ifTrue: [ObjectMemory removeDependent: self class]!

myRunStripper
	"Run the stripper to remove any unnecessary system classes."

	MyStripper new 
		doRemoveOfSubsystems; 
		stripSystem.!

mySaveImageAndQuit
	"Do a final garbage collect then save the image and quit. 
	 Saving as a snapshot means that no change file is created.
	The loadPolicy parameter specifies that all objects in old space are loaded into perm space."

	ObjectMemory globalGarbageCollect.
	ObjectMemory
		snapshotAs: self myImageName
		thenQuit: true
		withLoadPolicy: 1! !

!ImageCreator methodsFor: 'exception handling'!

handleException: ex context: aContext
	"Do some error handling then quit"
	Dialog warn: 'Error: ', ex errorString, ' in ', aContext printString, '. About to quit'.
	ObjectMemory quit! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ImageCreator class
	instanceVariableNames: ''!


!ImageCreator class methodsFor: 'class initialization'!

initialize
	"Register ourselves to receive an update message when the image comes up."

	"self initialize"

	(ObjectMemory dependents includes: self)
		ifFalse: [ObjectMemory addDependent: self]!

update: anAspect with: arguments from: anObject 
	"Start the application when we are told the image has come up."

	(anObject == ObjectMemory and: [anAspect == #returnFromSnapshot])
		ifTrue: 
			[self new createImage]! !

!ImageCreator class methodsFor: 'saving image'!

saveImage
	"Save the image that will be used to create the production image.
	Do a final garbage collect then save the image and quit. 
	 Saving as a snapshot means that no change file is created.
	The loadPolicy parameter specifies that all objects in old space are loaded into perm space.

	Use this method to save the image without a change file"

	"self saveImage"

	ObjectMemory globalGarbageCollect.
	ObjectMemory
		snapshotAs: self myImageName
		thenQuit: true
		withLoadPolicy: 1! !

!ImageCreator class methodsFor: 'names'!

myImageName
	"Return the name of the image that will be used to create the deployment image"

	^'mkim' copy! !

Stripper subclass: #MyStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ImageCreator'!
MyStripper comment:
'I am a subclass of Stripper. I provide similar functionality without any Dialogs'!


!MyStripper methodsFor: 'initialize-release'!

initialize
	super initialize.
	doDbtools := true asValue.
	doPrinting := true asValue.
	doExtras := true asValue.! !

!MyStripper methodsFor: 'system stripping'!

stripSystem
	"Here's where everything happens."
	|     array listOfClasses numberOfClasses index |
	progress value: 0.
		"First, run user supplied code, if any"
	self preStrip.
		"Look to see if there are any tools open."
	self openViewTest
		ifFalse: [^self].
	Cursor wait show.
	array := self classesAndBytesLeft.
	array at: 1. array at: 2.
	progress value: 10.
	Transcript cr; show: 
		'Remove all classes from System Organizer'.
	self emptySystemOrganization.
	progress value: 15.
	Transcript cr; show: 
		'Remove the opcode pool dictionary'.
	self removeOpcodePool.
	Transcript cr; show: 
		'Remove to-be-removed pool dictionaries from Smalltalk'.
	self removeLocalPools.
	Transcript cr; show: 
		'Change Projects so they do not use ChangeSets'.
	self projectsWithoutChangeSets. "Must be done before lockout"
	progress value: 20.
	doCompiler value ifTrue:
		[Transcript cr; show: 'Disable emergency evaluator'.
		self disableEmergEvaluator].
	Transcript cr; show: 
		'Add class-zapping messages to the right classes'.
	self installDisableMessages.
	Transcript cr; show:
		'Change Behavior to allow only DoIts'.
	self installBehaviorLockouts.
	Transcript cr; show: 
		'Install simple notifier'.
	self installNewNotifier.
	progress value: 30.
	listOfClasses := self listOfClasses.
	numberOfClasses := listOfClasses size.
	index := 0.
	listOfClasses do:
		[:class |  "Zap them"
			class disable.
			index := index + 1.
			progress value: 30 + (index * 50 // numberOfClasses)].
	progress value: 80.
	SourceFileManager default discardSources.	"Remove access to source files"
	Transcript cr; show: 
		'Remove all instance variable strings and class organizers'.
	self doOrgs.
	progress value: 85.
		"Let go of class name references"
	self removeRefsToClassNames.
		"Run user supplied code, if any"
	progress value: 90.
	self postStrip.
	progress value: 95.
	Transcript cr; show: 
			'Get rid of newly-unreferenced Symbols...'.
	Symbol initialize.
	array := self classesAndBytesLeft.
	progress value: 100.
	array at: 1. array at: 2.
	Cursor normal show! !

!MyStripper methodsFor: 'user preference'!

postStrip
	"Hook for users to insert code at the end of stripping."
	super postStrip!

preStrip
	"Hook for users to insert code at the start of stripping."
	super preStrip! !

!MyStripper methodsFor: 'actions'!

doRemoveOfSubsystems
	doDbtools value
		ifTrue: [self removeLensAndDBTools].
	doPrinting value
		ifTrue: [self removePrinting].
	Smalltalk organization removeEmptyCategories.
	Smalltalk allBehaviorsDo: [:cl | cl organization removeEmptyCategories].! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyStripper class
	instanceVariableNames: ''!


!MyStripper class methodsFor: 'user preferences'!

initExtras
	"The user may modify this list, but should remember to evaluate
		MyStripper initExtras
	and to set doExtras."
	ExtrasList := #(MappedCollection )! !
ImageCreator initialize!


