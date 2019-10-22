 "Examples for Chapter 11, Collections.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

| orderedCollection |
orderedCollection := OrderedCollection new.
orderedCollection add: 'xyz'; add: 'abc'.
orderedCollection first inspect

| orderedCollection |
orderedCollection := OrderedCollection new.
orderedCollection add: #here.
orderedCollection remove: #here.
orderedCollection remove: #there ifAbsent: [nil].
orderedCollection inspect.

| array |
array := #(5 4 3 2 1).
SequenceableCollectionSorter sort: array.
array inspect.

| array |
array := Array new: 10.
array at: 3 put: #hiThere.
(array at: 3) inspect.


| dictionary1 dictionary2 dictionary3 |
dictionary1 := Dictionary new.
dictionary1 at: 'abc' put: 'def'.
dictionary2 := Dictionary new.
dictionary2 add: 'abc' -> 'def'.
dictionary3 := Dictionary new.
dictionary3 add: (Association key: 'abc' value: 'def').
dictionary1 inspect.
dictionary2 inspect.
dictionary3 inspect.

| set |
set := Set new add: 7; add: 8; add: 7; yourself.
set size inspect

| collection includesDuplicates |
collection := #(1 2 3 4 3 2 1).
includesDuplicates := collection size ~= collection asSet size.
includesDuplicates inspect.

| duplicateList collection |
duplicateList := List new.
collection := #(1 2 3 4 3 2 1).
collection asBag valuesAndCountsDo:
	[:each :count | count > 1 ifTrue: [duplicateList add: each]].
duplicateList inspect.

[| orderedCollection |
orderedCollection := OrderedCollection new.
1 to: 100 do: [:index | orderedCollection add: index]] speed.

[| orderedCollection |
orderedCollection:= OrderedCollection new: 100.
1 to: 100 do: [:index | orderedCollection add: index]] speed.

| collection |
collection := OrderedCollection with: 'how' with: 'are' with: 'you'.
collection inspect.

| collection |
collection := List withAll: #('how' 'are' 'you' 'today' 'Alec?').
collection inspect.

| collection |
collection := OrderedCollection new.
collection
	add: 1;
	add: 2.
collection inspect.

| collection |
(collection := OrderedCollection new)
	add: 1;
	add: 2.
collection inspect.

| collection |
collection := OrderedCollection new
	add: 1;
	add: 2;
	yourself.
collection inspect.

| collection |
collection:= OrderedCollection withAll: #(1 2 3 4 5 6).
collection copy do: [ :each | each even ifTrue: [collection remove: each]].
collection inspect.

| collection newCollection |
collection:= OrderedCollection withAll: #(1 2 3 4 5 6).
newCollection := collection select: [:each | each odd].
newCollection inspect.

| collection newCollection |
collection:= OrderedCollection withAll: #(1 2 3 4 5 6).
newCollection := collection reject: [:each | each even].
newCollection inspect.

#(1 2 3 4 5) do: [ :each | Transcript cr; show: each printString].

| squareCollection |
squareCollection := List new.
#(1 2 3 4 5) do: [ :each | squareCollection add: each * each].
squareCollection inspect.

| squareCollection |
squareCollection:= #(1 2 3 4 5) collect: [ :each | each * each].
squareCollection inspect.

| oddCollection |
oddCollection := #(1 2 3 4 5) select: [ :each | each odd].
oddCollection inspect.

| evenCollection |
evenCollection := #(1 2 3 4 5) reject: [ :each | each odd].
evenCollection inspect.

| number |
number := (1 to: 5) detect: [ :each | each > 3].
number inspect.

| number |
number := (1 to: 5) detect: [ :each | each > 30] ifNone: [99].
number inspect.

| total |
total := (1 to: 5) inject: 0 into: [ :subtotal :each | subtotal + each].
total inspect.

| maxValue |
maxValue := (1 to: 5) inject: 0 into: [ :max :each | max max: each].
maxValue inspect.

| answer |
answer := (1 to: 5) inject: 1 into: [ :factorial :each | factorial * each ].
answer inspect.

| sentence |
sentence := #('Now' 'is' 'the' 'time' 'for' 'all' 'good' 'men') 
	inject: String new 
	into: [ :string :each | string, each, ' '].
sentence inspect.

| answer |
answer := (#(3 $x 'hello' #mySymbol) 
	inject: String new writeStream 
	into: [ :stream :each | 
		stream 
			print: each class; 
			nextPutAll: ' value '; 
			print: each; 
			cr; yourself]) contents.
answer inspect.

| collection |
collection := OrderedCollection new.
collection add: 'abc'; add: 'def'; add: 'xyz'.
(collection includes: 'def') inspect.
(collection contains: [:each | each includes: $y] ) inspect.

| list |
list := List new.
list add: 'xyz'; add: 'abc'; add: 'mno'.
list asSortedCollection inspect.

