Object subclass: #MyStockWatcher
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyStockWatcher class
	instanceVariableNames: ''!


!MyStockWatcher class methodsFor: 'instance creation'!

watchStock: aStock
	^super new initialize: aStock! !

MyStockWatcher subclass: #MyStockWatcher2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!


!MyStockWatcher2 methodsFor: 'initialize-release'!

initialize: aStock 
	aStock
		expressInterestIn: #price
		for: self
		sendBack: #priceChanged:by:! !

!MyStockWatcher2 methodsFor: 'updating'!

priceChanged: oldPrice by: aStock 
	Transcript 
		cr; print: thisContext; tab; 
		nextPutAll: 'Price changed from: '; print: oldPrice; 
		nextPutAll: ' to '; print: aStock price; flush! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyStockWatcher2 class
	instanceVariableNames: ''!


!MyStockWatcher2 class methodsFor: 'example'!

example
	"self example"

	| stock |
	stock := MyStock2 new.
	self watchStock: stock.
	stock traded: 200 price: 20.
	stock traded: 100 price: 30.! !

MyStockWatcher subclass: #MyStockWatcher3
	instanceVariableNames: 'stock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!


!MyStockWatcher3 methodsFor: 'updating'!

priceChanged
	Transcript 
		cr; print: thisContext; tab;
		nextPutAll: 'New price='; print: stock price value;
		flush!

priceChanged: aPlaceHolder by: aStock
	Transcript 
		cr; print: thisContext; tab;
		nextPutAll: 'New price='; print: aStock value;
		flush! !

!MyStockWatcher3 methodsFor: 'initialize-release'!

initialize: aStock 
	stock := aStock.
	stock price onChangeSend: #priceChanged to: self.
	aStock price onChangeSend: #priceChanged:by: to: self! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyStockWatcher3 class
	instanceVariableNames: ''!


!MyStockWatcher3 class methodsFor: 'example'!

example
	"self example"

	| stock |
	stock := MyStock3 new.
	self watchStock: stock.
	stock traded: 200 price: 20.
	stock traded: 100 price: 30.! !

Model subclass: #MyStock
	instanceVariableNames: 'price trade totalTraded '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!


!MyStock methodsFor: 'accessing'!

price
	^price isNil
		ifTrue: [price := 0]
		ifFalse: [price]!

price: aPrice 
	| oldPrice |
	oldPrice := self price.
	price := aPrice.
	self changed: #price with: oldPrice!

totalTraded
	^totalTraded isNil
		ifTrue: [totalTraded := 0]
		ifFalse: [totalTraded]!

totalTraded: aCount 
	| oldTotalTraded |
	oldTotalTraded := self totalTraded.
	totalTraded := aCount.
	self changed: #totalTraded with: oldTotalTraded!

trade
	^trade isNil
		ifTrue: [trade := 0]
		ifFalse: [trade]!

trade: aCount 
	| oldTrade |
	oldTrade := self trade.
	trade := aCount.
	self changed: #trade with: oldTrade! !

!MyStock methodsFor: 'trading'!

traded: aCount price: aPrice 
	self price: aPrice.
	self trade: aCount.
	self totalTraded: self totalTraded + aCount! !

MyStock subclass: #MyStock1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!


MyStockWatcher subclass: #MyStockWatcher1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!


!MyStockWatcher1 methodsFor: 'initialize-release'!

initialize: aStock 
	aStock addDependent: self! !

!MyStockWatcher1 methodsFor: 'updating'!

update: aSymbol with: oldPrice from: aStock 
	Transcript cr; print: thisContext; tab; print: aSymbol; tab.
	aSymbol == #price ifTrue: 
		[Transcript 
			nextPutAll: 'Price changed from: '; print: oldPrice; 
			nextPutAll: ' to '; print: aStock price].
	Transcript flush.! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyStockWatcher1 class
	instanceVariableNames: ''!


!MyStockWatcher1 class methodsFor: 'example'!

example
	"self example"

	| stock |
	stock := MyStock1 new.
	self watchStock: stock.
	stock traded: 200 price: 20.
	stock traded: 100 price: 30.! !

MyStock subclass: #MyStock2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!
MyStock2 comment:
'| parent |
parent := MyParent3 new.
MyDependent3 newWithParent: parent.
parent string: ''kkk''.
parent number: 44.
parent string: ''mmmm''.
parent number: 55.

In this example we use a different dependency mechanism. We register as a dependent of the parent, but we ask the parent to do the filtering, sending us a specific message for each message we are interested in.

Note that the message we want sent is something like:
	stringChanged:by:
When we specify a message selector with no arguments, we will be sent just the message.
A one argument selector will give us the new value (assuming the parent send changed:with:, otherwise nil).
A two argument selector will give us the new value and the object it came from.

(See update:with:from: in DependencyTransformer)'!


MyStock subclass: #MyStock3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!
MyStock3 comment:
'| parent |
parent := MyParent2 new.
MyDependent2 newWithParent: parent.
parent string: ''aaa''.
parent number: 22.
parent string: ''bbbb''.
parent number: 33.

In this example we use a more sophisticated dependency mechanism. In this case, rather than registering as a dependent of the whole parent, we do separate registrations for each field we are interested in getting change information about.

The fields have to be ValueHolders, and we register an interest in a ValueHolder by sending the onChangeSend:to: message to it. We specify the message that the ValueHolder should send us when the object changes. Now we don''t have to check for specific symbols; instead we just write methods for the messages we specified.'!


!MyStock3 methodsFor: 'accessing'!

price
	"Create a ValueHolder first time"

	^price isNil
		ifTrue: [price := 0 asValue]
		ifFalse: [price]!

price: aPrice 
	"The self changed is done by the ValueHolder"

	self price value: aPrice!

totalTraded
	"Create a ValueHolder first time"

	^totalTraded isNil
		ifTrue: [totalTraded := 0 asValue]
		ifFalse: [totalTraded]!

totalTraded: aCount 
	"The self changed is done by the ValueHolder"

	self totalTraded value: aCount!

trade
	"Create a ValueHolder first time"

	^trade isNil
		ifTrue: [trade := 0 asValue]
		ifFalse: [trade]!

trade: aTrade 
	"The self changed is done by the ValueHolder"

	self trade value: aTrade! !

!MyStock3 methodsFor: 'trading'!

traded: aCount price: aPrice 
	self price: aPrice.
	self trade: aCount.
	self totalTraded: self totalTraded value + aCount! !

Object subclass: #MyStockExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'STBE-DependencyExamples'!


!MyStockExample methodsFor: 'examples'!

exampleOne
	| stock |
	Transcript cr; show: '------------- Doing Example One ----------------------'.
	stock := MyStock1 new.
	MyStockWatcher1 watchStock: stock.
	stock traded: 200 price: 20.
	stock traded: 100 price: 30!

exampleThree
	| stock |
	Transcript cr; show: '------------- Doing Example Three ----------------------'.
	stock := MyStock3 new.
	MyStockWatcher3 watchStock: stock.
	stock traded: 200 price: 20.
	stock traded: 100 price: 30!

exampleTwo
	| stock |
	Transcript cr; show: '------------- Doing Example Two ----------------------'.
	stock := MyStock2 new.
	MyStockWatcher2 watchStock: stock.
	stock traded: 200 price: 20.
	stock traded: 100 price: 30! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyStockExample class
	instanceVariableNames: ''!


!MyStockExample class methodsFor: 'examples'!

example
	"self example"

	| instance |
	instance := self new.
	instance exampleOne.
	instance exampleTwo.
	instance exampleThree! !

