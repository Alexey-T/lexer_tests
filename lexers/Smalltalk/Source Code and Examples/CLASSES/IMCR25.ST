Object subclass: #ImageCreator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ImageCreator'!
ImageCreator comment:
'I create a production image by filing in the application code, then running MyImageMaker. Note that doing all this is automatic. You simply have to file me and MyImageMaker into the image, then saving it, possible by doing ImageCreator saveImage. When you start up this new image, it will automatically create a production image.'!


!ImageCreator methodsFor: 'names'!

myApplication
	"Return the class that starts up the application"

	^MyApplication!

myChoicesFile
	"Return the filename that contains the ImageMaker choices"

	^'choices.txt' asFilename!

myInstallFile
	"Return the filename that files in the application code"

	^'install.st' asFilename! !

!ImageCreator methodsFor: 'making image'!

createImage
	self myRemoveSelfAsDependent.
	self myInitializeImageMaker.
	self myFileInChoices.
	self myFileInApplication.
	self myOpenApplication.
	self myInstallEmergencyHandler.
	self myRunImageMaker!

myFileInApplication
	"File in the application code."

	self myInstallFile fileIn.!

myFileInChoices
	"File in the ImageMaker choices that we previously saved"

	self myChoicesFile fileIn!

myInitializeImageMaker
	"Create a new ImageMaker and make it the current one"

	MyImageMaker current: MyImageMaker new!

myInstallEmergencyHandler
	"Install the application Emergency Handler."

"	Exception emergencyHandler: 
		[:ex :context | self handleException: ex context: context]"!

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

myRunImageMaker
	"Remove unneeded classes and save the new image"

	MyImageMaker current makeDeploymentImage! !

!ImageCreator methodsFor: 'exception handling'!

handleException: ex context: aContext
	"Do some error handling then quit"
	Dialog warn: 'Error <', ex errorString, '>. Quitting application'.
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

	You can use this method to save the image without a changes file."
	
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

ImageMaker subclass: #MyImageMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-ImageCreator'!
MyImageMaker comment:
'I am a subclass of ImageMaker and provide similar functionality without dialogs.'!


!MyImageMaker methodsFor: 'names'!

myBaseDirectory
	"The directory where the utils subdirectory resides"
	^'c:\visual' copy!

myImageName
	^'myapp' copy! !

!MyImageMaker methodsFor: 'override'!

getZoneDirectory
	| relDirectory |

	"Prompt for the name of the release directory, and test to see whether it
	contains the necessary subdirectory of zone scripts."

	relDirectory := self myBaseDirectory.
	relDirectory isEmpty ifTrue: [^nil].
	zoneDirectory := relDirectory.
	self class removeScriptDirPath do:
		[:step |
		zoneDirectory := zoneDirectory asFilename construct: step].
	(zoneDirectory definitelyExists and:
		[zoneDirectory isDirectory and:
			[(zoneDirectory construct: 'progtool.rm') isReadable]])
		ifFalse:
			[Dialog warn: zoneDirectory asString, ' does not contain the zone script files'.
			^nil].	
	^zoneDirectory!

makeImageFile

	| headless imagePrefix |
	headless := (self optionNamed: #headlessOption) elected.
	imagePrefix := self myImageName.
	self writeReportFile: imagePrefix.
	ObjectMemory permSaveAs: imagePrefix thenQuit: true.
	(builder notNil and:
			[builder window notNil])
		ifTrue:
			[builder window controller closeAndUnschedule].
	(self optionNamed: #dsetViewOption) optionallyPerformFor: self.
	self installAutoLoader.
	headless
		ifTrue: [HeadlessImage default isHeadless: true].
	ControlManager
		noWindowBlock: [:ctrlMgr | ObjectMemory quit].
	ObjectMemory
		globalCompactingGC;
		saveAs: imagePrefix thenQuit: true.!

refreshView!

warnUser
	^true! !
ImageCreator initialize!


