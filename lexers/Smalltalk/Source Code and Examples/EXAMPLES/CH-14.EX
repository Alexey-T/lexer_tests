 "Examples for Chapter 14, Files.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

Filename named: 'c:\baseDir\file.ext'.
'c:\baseDir\file.ext' asFilename.

| dirName fileName |
	dirName := 'c:\baseDir\subDir' asFilename.
fileName := dirName construct: 'myFile.ext'.
fileName inspect.

('c:\baseDir\subDir', (String with: Filename separator), 'myFile.ext') asFilename.

| filePath fileName |
filePath := '\baseDir\subDir1\subDir2\file'.
fileName := filePath asFilename.
fileName inspect.

| directoryPath directoryName |
directoryPath := 'c:\baseDir\subDir1\subDir2'.
directoryName := directoryPath asFilename.
directoryName inspect.

| fileName |
fileName := 'c:\baseDir\subDir\myFile.ext' asFilename.
fileName directory inspect.
fileName head inspect.
fileName tail inspect.

| filePath |
filePath := 'c:\baseDir\subDir1\fileName.ext'.
(Filename components: filePath) inspect.
(Filename breakup: filePath) inspect.
(Filename splitExtension: filePath) inspect.
(Filename splitPath: filePath) inspect.

| writeStream |
writeStream := 'c:\temp\myfile' asFilename writeStream.
[writeStream 
	nextPutAll: 'Some data';
	cr; nextPutAll: 'More data']
		valueNowOrOnUnwindDo: [writeStream close]

(ExternalStream classPool at: #OpenStreams) copy inspect.

(ExternalStream classPool at: #OpenStreams) copy do: 
	[:each | each name = 'someFilePath' ifTrue: [each close]].

| stream |
stream := 'c:\temp\binfile' asFilename readWriteStream.
stream binary.
[stream nextStringPut: 'Hi Alec'.
stream nextLongPut: 999999.
stream nextWordPut: 33.
stream reset.
Transcript cr; show: 'String = ', stream nextString.
Transcript cr; show: 'Long = ', stream nextLong printString.
Transcript cr; show: 'Word = ', stream nextWord printString]
	valueNowOrOnUnwindDo: [stream close].

Filename isCaseSensitive.
Filename currentDirectory.
Filename defaultDirectory.
Filename volumes.
Filename filesMatching: '*.im'.

| dirName |
	dirName := Filename defaultDirectory.
dirName directoryContents inspect.
(dirName filesMatching: '*.im') inspect.

"Before doing any of the following, make sure that the directory and the file both exist."
| dirName fileName |
dirName := 'c:\temp' asFilename.
fileName := dirName construct: 'myfile'.
fileName exists.	
dirName definitelyExists.
fileName fileSize.
dirName fileSize.
fileName isReadable.
dirName isWritable.
fileName canBeWritten.
dirName canBeWritten.
fileName isDirectory.
dirName isDirectory.
fileName isAbsolute.	
dirName isRelative.
