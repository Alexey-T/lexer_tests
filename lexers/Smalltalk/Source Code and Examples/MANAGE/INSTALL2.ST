"install2.st - Install the software
When. Who. Why.
"

| context baseDir |

"Figure out the directory this is being filed in from"
context := thisContext.
[context receiver isKindOf: Stream]
	whileFalse: 
		[context := context sender].
baseDir := context receiver name asFilename head asFilename.
sourceDir := baseDir construct: 'source'.

(Dialog confirm: 'Ok to file in application from directory ', baseDir asString, '?')
	ifFalse: 
		[Dialog warn: 'Please file this in from the directory containing the application files'.
		^self].

Dialog warn: 'Okay, filing in'.
"File in the application code here"

"Empty the change set"
ChangeSet noChanges.				
