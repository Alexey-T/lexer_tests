 "Examples for Chapter 13, Streams.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

| writeStream readStream |
writeStream := 'myfile' asFilename writeStream.
writeStream nextPutAll: 'Here I am, writing to my file'.
writeStream cr; nextPutAll: 'Here is line two'.
writeStream close.
readStream := 'myfile' asFilename readStream.
Transcript cr; show: (readStream upTo: Character cr).
Transcript cr; show: readStream upToEnd.
readStream close.

| string |
string := 'There are ' , 4 printString, ' apples in ', 1 printString, ' basket.',
			(String with: Character cr), 4 printString, ' of you can have ', 1 printString.
string inspect.

| stream |
stream := (String new: 100) writeStream.
stream
	nextPutAll: 'There are '; print: 4;
	nextPutAll: ' apples in '; print: 1;
	nextPutAll: ' basket.';
	cr; print: 4;
	nextPutAll: ' of you can have '; print: 1.
stream contents inspect.

| stream |
stream := ReadWriteStream on: (Array new).
stream nextPut: 2.
stream nextPut: 'How are you'.
stream nextPutAll: 'Alec'.
stream nextPutAll: #('Fine' 'thanks').
stream nextPut: OrderedCollection new.
stream reset.
[stream atEnd]
	whileFalse: 
		[Transcript cr; show: stream next displayString]
