"install.st - Install the Application software
When. Who. Why
"

| sourceDir sysAddDir sysModDir localFile 
beforeClassCount beforeMethodCount afterClassCount afterMethodCount |

"Figure out the various directories. We allow development to be done on different types of platform"
Window platformName = 'X11'
	ifTrue: 
		[sourceDir := '/product/src/'.
		sysAddDir := '/visual/sys/additions/'.
		sysModDir := '/visual/sys/modifications/'.
		localFile := '/home/', (CEnvironment getenv: 'LOGNAME'), '/.vwlocal']
	ifFalse:
		[sourceDir := 'c:\alec\st\source\'.
		sysAddDir := 'c:\alec\st\sysadds\'.
		sysModDir := 'c:\alec\st\sysmods\'.
		localFile := 'c:\visual\local\local.st'].

"File In any required system additions and modifications that we find the the relevant directories"
(sysAddDir asFilename filesMatching: '*.st')
	do: [ :each | each asFilename fileIn].
(sysModDir asFilename filesMatching: '*.st')
	do: [ :each | each asFilename fileIn].

"File in any programmer-specific changes that we find"
(localFile notNil and: [localFile asFilename isReadable])
	ifTrue: [localFile asFilename fileIn]
	ifFalse: [Transcript cr; show: 'No local file'].

"Count the number of classes and methods in the image before filing in the application code"
beforeClassCount := Smalltalk classNames size.
beforeMethodCount := Smalltalk classNames 
	inject: 0 
	into: [:subtotal :each | | class| 
			class := Smalltalk at: each.
			subtotal + class selectors size + class class selectors size].

"Create any necessary pool dictionaries and add all the pool dictionary variables"
Smalltalk at: #MyPoolDictionary put: Dictionary new.
MyPoolDictionary at: #PoolVariableOne put: nil.
MyPoolDictionary at: #PoolVariableTwo put: nil.

"File in all the named categories"
#(	
	'CatgOne.st'
	'CatgTwo.st'
	'CatgThree.st'
) do: [ :filename | (sourceDir , filename) asFilename fileIn ].

"Count the number of classes and methods in the image after filing in the application code,
then report on the counts."
afterClassCount := Smalltalk classNames size.
afterMethodCount := Smalltalk classNames 
	inject: 0 
	into: [:subtotal :each | | class| 
			class := Smalltalk at: each.
			subtotal + class selectors size + class class selectors size].

Transcript 
	cr; nextPutAll: 'Before loading in:';
	crtab; nextPutAll: 'Class count = '; print: beforeClassCount;
	crtab; nextPutAll: 'Method count = '; print: beforeMethodCount;
	cr; nextPutAll: 'After loading in:';
	crtab; nextPutAll: 'Class count = '; print: afterClassCount;
	crtab; nextPutAll: 'Method count = '; print: afterMethodCount;
	cr; nextPutAll: 'New:';
	crtab; nextPutAll: 'Classes = '; print: (afterClassCount - beforeClassCount);
	crtab; nextPutAll: 'Methods = '; print: (afterMethodCount - beforeMethodCount);
	endEntry.

"Do any final system changes"
LookPreferences deleteForward: true			"Make the delete key delete forwards"
ChangeSet noChanges.						"Empty the change set"
