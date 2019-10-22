Object subclass: #TstResult
	instanceVariableNames: 'startTime stopTime count test failures errors '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!
TstResult comment:
'I keep track of the results of running tests. For detailed information on how I work, see Chapter 30, Testing.

Instance Variables:

	startTime 	<Time>					The start time for the series of tests.
	stopTime 	<Time>					The end time for the series of tests.
	count 		<SmallInteger>			The number of tests run.
	test 		<Class>				A test class.
	failures 		<OrderedCollection>		A collection of test failures.
										A failure is a test whose results did not match the expected results.
	errors		<OrderedCollection>		A collection of test errors.
										An error is a test in which an error occured.'!


!TstResult methodsFor: 'errors-failures'!

error: aString in: aTestCase
	errors add: aTestCase-> aString!

failure: aString in: aTestCase
	failures add: aTestCase-> aString! !

!TstResult methodsFor: 'running'!

start
	startTime := Timestamp now!

stop
	stopTime := Timestamp now! !

!TstResult methodsFor: 'accessing'!

incrementCount
	count := count + 1! !

!TstResult methodsFor: 'initialize release'!

initialize: aTest
	test := aTest.
	count := 0.
	failures := OrderedCollection new.
	errors := OrderedCollection new! !

!TstResult methodsFor: 'printing'!

myPrintCollection: aCollection label: aString on: aStream 
	aStream 
		cr; cr; 
		nextPutAll: aString;
		nextPutAll: ': ';
		print: aCollection size.
	aCollection
		do: 
			[:each | 
			aStream cr.
			each key printOn: aStream.
			each value notNil
				ifTrue: 
					[aStream nextPutAll: '  ('.
					aStream nextPutAll: each value.
					aStream nextPut: $)]]!

myPrintErrorsOn: aStream 
	errors isEmpty ifFalse: 
		[self
			myPrintCollection: errors
			label: 'Errors'
			on: aStream]!

myPrintFailuresOn: aStream 
	failures isEmpty ifFalse: 
		[self
			myPrintCollection: failures
			label: 'Failures'
			on: aStream]!

printOn: aStream 
	aStream nextPutAll: 'Name: '.
	test printOn: aStream.
	aStream 
		cr; nextPutAll: 'Tests run: '; print: count;
		cr; nextPutAll: 'Start time: '; print: startTime;
		cr; nextPutAll: 'Stop time: '; print: stopTime.
	self myPrintFailuresOn: aStream.
	self myPrintErrorsOn: aStream! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

TstResult class
	instanceVariableNames: ''!


!TstResult class methodsFor: 'instance creation'!

newFor: aTest
	^super new initialize: aTest! !

ApplicationModel subclass: #TstTestManager
	instanceVariableNames: 'mainTest subTestList individualTestList status doSetupCleanup '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!
TstTestManager comment:
'I manage hierarchies of TstTest classes, and run tests, both individual tests and hierarchies of tests. For detailed information on how I work, see Chapter 30, Testing.'!


!TstTestManager methodsFor: 'initialize-release'!

initialize
	mainTest := self rootTestClass asValue.
	status := String new asValue.
	doSetupCleanup := false asValue.
	subTestList := SelectionInList new.
	subTestList selectionIndexHolder onChangeSend: #changedSubTest to: self.
	individualTestList := MultiSelectionInList new.
	self myBuildLists.! !

!TstTestManager methodsFor: 'aspects'!

doSetupCleanup
	^doSetupCleanup!

individualTestList
	^individualTestList!

mainTest
	^mainTest!

status
	^status!

subTestList
	^subTestList! !

!TstTestManager methodsFor: 'support'!

myBuildLists
	| test |
	test := self mainTest value.
	self subTestList list: test subclasses asSortedCollection.
	self individualTestList list: test individualTestNames!

myTestResultFor: aTest
	^TstResult newFor: aTest!

showSuperclass
	"Display the superclass of the previously displayed class and all 
	its sub-tests and individual tests. Don't do anything if we are already 
	at the root class."

	| mainTestClass |
	mainTestClass := self mainTest value.
	mainTestClass == self rootTestClass
		ifFalse: 
			[self mainTest value: mainTestClass superclass.
			self myBuildLists]! !

!TstTestManager methodsFor: 'selections'!

clearAll
	self individualTestList clearAll.!

selectAll
	self individualTestList selectAll.! !

!TstTestManager methodsFor: 'change messages'!

changedSubTest
	"If a test class has been selected, make it the primary test class."

	status value: String new.
	self subTestList selection notNil
		ifTrue: 
			[self mainTest value: self subTestList selection.
			self myBuildLists]! !

!TstTestManager methodsFor: 'running tests'!

myRunSelectedCases: aCollection forClass: aClass 
	self myDoSetupFor: aClass.
	aCollection do: [:each | each run].
	self myDoCleanupFor: aClass!

myRunTestsForClass: aClass result: aTestResult 
	self myDoSetupFor: aClass.
	aClass individualTests
		do: [:each | each run: aTestResult].
	aClass subclasses do: 
		[:each | self myRunTestsForClass: each result: aTestResult].
	self myDoCleanupFor: aClass!

runSelected
	| testClass testCollection |
	self individualTestList selections isEmpty
		ifTrue: 
			[status value: 'No tests to run'.
			^self].
	status value: 'Running....'.
	testClass := self mainTest value.
	testCollection := self individualTestList selections collect: [:each | testClass newTest: each].
	self doSetupCleanup value ifTrue: [self myDoHierarchySetupFor: testClass].
	self myRunSelectedCases: testCollection forClass: testClass.
	self doSetupCleanup value ifTrue: [self myDoHierarchyCleanupFor: testClass].
	status value: 'Done'!

runTests
	| testClass  testResult |
	status value: 'Running....'.
	testClass := self mainTest value.
	testResult := self myTestResultFor: testClass.

	self myDoHierarchySetupFor: testClass.
	testResult start.
	self myRunTestsForClass: testClass result: testResult.
	testResult stop.
	self myDoHierarchyCleanupFor: testClass.

	status value: 'Done'.
	testResult inspect.! !

!TstTestManager methodsFor: 'setup-cleanup'!

myDoCleanupFor: aClass 
	(aClass class includesSelector: #cleanUp)
		ifTrue: [aClass cleanUp]!

myDoHierarchyCleanupFor: aClass 
	aClass testSuperclasses
		do: [:each | self myDoCleanupFor: each]!

myDoHierarchySetupFor: aClass 
	aClass testSuperclasses
		reverseDo: [:each | self myDoSetupFor: each]!

myDoSetupFor: aClass 
	(aClass class includesSelector: #setUp)
		ifTrue: [aClass setUp]! !

!TstTestManager methodsFor: 'accessing'!

rootTestClass
	^TstTest! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

TstTestManager class
	instanceVariableNames: ''!


!TstTestManager class methodsFor: 'interface specs'!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Test Manager' 
			#min: #(#Point 50 50 ) 
			#max: #(#Point 1128 900 ) 
			#bounds: #(#Rectangle 546 296 941 685 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#SequenceViewSpec 
					#layout: #(#LayoutFrame 0 0 200 0 0 0.5 0 1 ) 
					#name: #individualTestList 
					#model: #individualTestList 
					#menu: #individualTestMenu 
					#multipleSelections: true ) 
				#(#SequenceViewSpec 
					#layout: #(#LayoutFrame 0 0.5 40 0 0 1 0 1 ) 
					#model: #subTestList 
					#tabable: false ) 
				#(#LabelSpec 
					#layout: #(#Rectangle 7 173 129 195 ) 
					#label: 'Individual Tests' ) 
				#(#LabelSpec 
					#layout: #(#LayoutFrame 5 0.5 15 0 120 0.5 38 0 ) 
					#label: 'Sub-Tests' ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 8 15 176 39 ) 
					#model: #mainTest 
					#menu: #mainTestMenu 
					#isReadOnly: true 
					#type: #object ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 6 43 87 77 ) 
					#model: #runTests 
					#label: 'Run Tests' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 88 43 178 77 ) 
					#model: #showSuperclass 
					#label: 'Up a Level' 
					#defaultable: true ) 
				#(#ActionButtonSpec 
					#layout: #(#Rectangle 127 167 176 195 ) 
					#model: #runSelected 
					#tabable: false 
					#label: 'Run' 
					#defaultable: true ) 
				#(#InputFieldSpec 
					#layout: #(#Rectangle 60 87 172 111 ) 
					#flags: 0 
					#model: #status 
					#tabable: false 
					#isReadOnly: true ) 
				#(#LabelSpec 
					#layout: #(#Point 8 88 ) 
					#label: 'Status:' ) 
				#(#DividerSpec 
					#layout: #(#Rectangle 10 119 173 123 ) ) 
				#(#CheckBoxSpec 
					#layout: #(#Point 11 136 ) 
					#model: #doSetupCleanup 
					#label: 'Run setup/cleanup' ) ) ) )! !

!TstTestManager class methodsFor: 'resources'!

individualTestMenu
	"UIMenuEditor new openOnClass: self andSelector: #individualTestMenu"

	<resource: #menu>
	^#(#Menu #(
		#(#MenuItem #label: 'select all' ) 
		#(#MenuItem #label: 'clear all' ) 
		#(#MenuItem #label: 'run selected' ) ) 
		#(2 1 ) 
		#(#selectAll #clearAll #runSelected ) ) decodeAsLiteralArray!

mainTestMenu
	"UIMenuEditor new openOnClass: self andSelector: #mainTestMenu"

	<resource: #menu>
	^#(#Menu #(
			#(#MenuItem 
				#label: 'run tests' ) 
			#(#MenuItem 
				#label: 'up a level' ) ) #(2 ) #(#runTests #showSuperclass ) ) decodeAsLiteralArray! !

!TstTestManager class methodsFor: 'examples'!

example
	"self example"

	self open! !

Object subclass: #TstTest
	instanceVariableNames: 'selector '
	classVariableNames: 'TestFailedSignal '
	poolDictionaries: ''
	category: 'STBE-Testing'!
TstTest comment:
'I am the superclass for all test classes. I know how to create and run tests. For detailed information on how I work, see Chapter 30, Testing.

Instance Variables:

	selector				<Symbol>	The message selector of the individual test to run

Class Variables:

	TestFailedSignal	<Signal>	The signal used to raise and trap test errors

Class Instance Variables:

	DisplayName		<String>	The string used when displaying this class'!


!TstTest methodsFor: 'running'!

performTest
	| testProcess timeoutProcess sharedQueue exceptionOrNil |
	self class showContext: thisContext selector: selector.
	sharedQueue := SharedQueue new.
	testProcess := 
		[self errorSignal 
			handle: [:ex | sharedQueue nextPut: ex]
			do: 
				[self perform: selector.
				sharedQueue nextPut: nil]] fork.
	timeoutProcess := 
		[(Delay forSeconds: self timeoutValue) wait.
		sharedQueue nextPut: (self errorSignal newException errorString: 'TIMEOUT')] fork.
	exceptionOrNil := sharedQueue next.
	testProcess terminate.
	timeoutProcess terminate.
	exceptionOrNil notNil ifTrue: 
		[exceptionOrNil propagateFrom: thisContext]!

performTest1
	self class showContext: thisContext selector: selector.
	self perform: selector!

performTest2
	| testProcess timeoutProcess sharedQueue exceptionOrNil |
	self class showContext: thisContext selector: selector.
	sharedQueue := SharedQueue new.
	testProcess := 
		[self errorSignal 
			handle: [:ex | sharedQueue nextPut: ex]
			do: 
				[self perform: selector.
				sharedQueue nextPut: nil]] fork.
	timeoutProcess := 
		[(Delay forSeconds: self timeoutValue) wait.
		sharedQueue nextPut: (self errorSignal newException errorString: 'TIMEOUT')] fork.
	exceptionOrNil := sharedQueue next.
	testProcess terminate.
	timeoutProcess terminate.
	exceptionOrNil notNil ifTrue: 
		[exceptionOrNil propagateFrom: thisContext]!

run
	self performTest!

run: aTestResult 
	self errorSignal 
		handle: [:ex | aTestResult error: ex localErrorString in: self]
		do: [self class testFailedSignal 
				handle: [:ex | aTestResult failure: ex localErrorString in: self]
				do: 
					[aTestResult incrementCount.
					self performTest]]! !

!TstTest methodsFor: 'initialize release'!

initialize: aSelector
	selector := aSelector! !

!TstTest methodsFor: 'accessing'!

timeoutValue
	"Subclasses can override if they want a different value"
	^3! !

!TstTest methodsFor: 'evaluating'!

expect: aBlock
	self expect: aBlock string: nil!

expect: aBlock string: aStringOrNil
	aBlock value ifFalse: 
		[self class testFailedSignal raiseErrorString: aStringOrNil]!

reject: aBlock
	self reject: aBlock string: nil!

reject: aBlock string: aStringOrNil 
	aBlock value ifTrue: 
		[self class testFailedSignal raiseErrorString: aStringOrNil]! !

!TstTest methodsFor: 'printing'!

printOn: aStream
	aStream 
		print: self class;
		nextPutAll: '>>';
		print: selector! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

TstTest class
	instanceVariableNames: 'DisplayName '!


!TstTest class methodsFor: 'instance creation'!

newTest: aSelector
	^self new initialize: aSelector! !

!TstTest class methodsFor: 'initialize '!

initialize
	"self initialize"

	DisplayName := 'All Tests'.
	TestFailedSignal := 
		(self errorSignal newSignal) 
			notifierString: 'Test failed: '; 
			nameClass: self message: #testFailedSignal;
			yourself! !

!TstTest class methodsFor: 'test support'!

individualTestNames
	"Return a sorted collection of individual test case names for this class."

	^(self organization listAtCategoryNamed: self testProtocol) asSortedCollection!

individualTests
	"Return a sorted collection of individual test cases for this class"

	^self individualTestNames
		collect: [:each | self newTest: each]!

rootTestClass
	^TstTest!

showContext: aContext
	self showContext: aContext selector: nil!

showContext: aContext selector: aSelector 
	"Uncomment the next line to get a printout of the contexts we go through"

	aSelector isNil
		ifTrue: [Transcript cr; show: aContext printString]
		ifFalse: [Transcript cr; show: aContext printString , ':' , aSelector printString]!

testProtocol
	^#tests!

testSuperclasses
	"Returns a collection of the superclasses up to the root test class"	
	| collection class |
	collection := OrderedCollection new.
	class := self.
	[class == self rootTestClass]
		whileFalse: 
			[class := class superclass.
			collection add: class].
	^collection! !

!TstTest class methodsFor: 'setup-cleanup'!

cleanUp
	"Do whatever is needed to clean up after the test. At any level in the 
	hierarchy, the cleanUp method will be invoked to clean up after all 
	the lower-level tests have been run. Subclasses should define their 
	own cleanUp method to clean up after all the tests at their level 
	and below have been run."

	self showContext: thisContext!

setUp
	"Do whatever is needed to get ready for the test to run."

	self showContext: thisContext! !

!TstTest class methodsFor: 'comparing'!

<= anObject
	^self displayString <= anObject displayString! !

!TstTest class methodsFor: 'signals'!

testFailedSignal
	^TestFailedSignal! !

!TstTest class methodsFor: 'printing'!

displayString
	^DisplayName notNil
		ifTrue: [DisplayName]
		ifFalse: [self name]! !

TstTest subclass: #MyTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!


!MyTests methodsFor: 'tests'!

testThat
	self expect: [#this == #that] string: 'this == that'!

testThis
	self reject: [#this == #that] string: 'this == that'! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyTests class
	instanceVariableNames: ''!


!MyTests class methodsFor: 'setup-cleanup'!

cleanUp
	super showContext: thisContext!

setUp
	super showContext: thisContext! !

!MyTests class methodsFor: 'class initialization'!

initialize
	"self initialize"
	super initialize.
	DisplayName := 'Personal Tests'! !

MyTests subclass: #MyTestOne
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!


!MyTestOne methodsFor: 'tests'!

testOne
	self expect: [1 == 1]!

testThree
	self expect: [1 == 3]!

testTwo
	self reject: [1 == 2]! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyTestOne class
	instanceVariableNames: ''!


!MyTestOne class methodsFor: 'setup-cleanup'!

cleanUp
	super showContext: thisContext!

setUp
	super showContext: thisContext! !

!MyTestOne class methodsFor: 'class initialization'!

initialize
	"self initialize"
	super initialize.
	DisplayName := 'Class One Tests'! !

MyTests subclass: #MyTestTwo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!


!MyTestTwo methodsFor: 'tests'!

testOne
	self expect: ['abc' == 'abc'] string: 'strings =='!

testThree
	self reject: ['abc' == #abc] string: 'strings == symbols'!

testTwo
	self reject: ['abc' == 'abc'] string: 'strings =='! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyTestTwo class
	instanceVariableNames: ''!


!MyTestTwo class methodsFor: 'setup-cleanup'!

cleanUp
	super showContext: thisContext!

setUp
	super showContext: thisContext! !

!MyTestTwo class methodsFor: 'class initialization'!

initialize
	"self initialize"
	super initialize.
	DisplayName := 'Class Two Tests'! !

TstTest subclass: #ProductTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ProductTests class
	instanceVariableNames: ''!


!ProductTests class methodsFor: 'class initialization'!

initialize
	"self initialize"
	super initialize.
	DisplayName := 'Product Tests'! !

ProductTests subclass: #SystemTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

SystemTests class
	instanceVariableNames: ''!


!SystemTests class methodsFor: 'setup-cleanup'!

cleanUp
	super showContext: thisContext!

setUp
	super showContext: thisContext! !

!SystemTests class methodsFor: 'class initialization'!

initialize
	"self initialize"
	super initialize.
	DisplayName := 'System Tests'! !

ProductTests subclass: #ComponentTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ComponentTests class
	instanceVariableNames: ''!


!ComponentTests class methodsFor: 'setup-cleanup'!

cleanUp
	super showContext: thisContext!

setUp
	super showContext: thisContext! !

!ComponentTests class methodsFor: 'class initialization'!

initialize
	"self initialize"
	super initialize.
	DisplayName := 'Component Tests'! !

TstTest subclass: #SupportClassTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

SupportClassTests class
	instanceVariableNames: ''!


!SupportClassTests class methodsFor: 'setup-cleanup'!

cleanUp
	super showContext: thisContext!

setUp
	super showContext: thisContext! !

!SupportClassTests class methodsFor: 'class initialization'!

initialize
	"self initialize"
	super initialize.
	DisplayName := 'Support Class Tests'! !

MyTests subclass: #MyTestThree
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-Testing'!


!MyTestThree methodsFor: 'tests'!

msgNotUnderstood
	self foo.!

unequalSymbolsOne
	self expect: [#tiger == #cat] string: 'tiger == cat'!

unequalSymbolsTwo
	(Delay forSeconds: 10) wait.
	self reject: [#tiger == #lion] string: 'tiger == lion'! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyTestThree class
	instanceVariableNames: ''!


!MyTestThree class methodsFor: 'setup-cleanup'!

cleanUp
	super showContext: thisContext!

setUp
	super showContext: thisContext! !

!MyTestThree class methodsFor: 'class initialization'!

initialize
	"self initialize"
	super initialize.
	DisplayName := 'Class Three Tests'! !
TstTest initialize!

MyTests initialize!

MyTestOne initialize!

MyTestTwo initialize!

ProductTests initialize!

SystemTests initialize!

ComponentTests initialize!

SupportClassTests initialize!

MyTestThree initialize!


