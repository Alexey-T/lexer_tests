 "Examples for Chapter 16, Processes.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

[Transcript cr; show: 'Hello World'] fork.

[10 timesRepeat: [Transcript show: '1']] fork.
[10 timesRepeat: [Transcript show: '2']] fork.

[10 timesRepeat: [Transcript show: '1'. (Delay forMilliseconds: 1) wait]] fork.
[10 timesRepeat: [Transcript show: '2'. (Delay forMilliseconds: 1) wait]] fork.

[10 timesRepeat: [Transcript show: '1'. Processor yield]] fork.
[10 timesRepeat: [Transcript show: '2'. Processor yield]] fork.

[10 timesRepeat: [Transcript show: '1']] forkAt: 50.
(Delay forMilliseconds: 1) wait.
[10 timesRepeat: [Transcript show: '2']] forkAt: 51.

[10 timesRepeat: [Transcript show: '1']] forkAt: 49.
(Delay forMilliseconds: 1) wait.
[10 timesRepeat: [Transcript show: '2']] forkAt: 51.

[10 timesRepeat: [Transcript show: '1'. Processor yield]] forkAt: 50.
(Delay forMilliseconds: 1) wait.
[10 timesRepeat: [Transcript show: '2'. Processor yield]] forkAt: 51.

| sharedQueue readProcess |
sharedQueue := SharedQueue new.
readProcess := [[Transcript show: ' R', sharedQueue next printString] repeat] fork.
[1 to: 5 do: [:index | 
	Transcript show: ' W', index printString.
	sharedQueue nextPut: index.
	Processor yield]] fork.
(Delay forSeconds: 5) wait.
readProcess terminate.

| process |
process := [[Transcript cr; show: 'Hi there'. (Delay forSeconds: 1) wait] repeat] fork. 
(Delay forSeconds: 5) wait.
process terminate.
Transcript cr; show: 'All Done'.

| process |
process := [Transcript cr; show: 'Done'] newProcess.
(Delay forSeconds: 3) wait.
process resume

| process |
process := 
	[1 to: 10 do: [:index |
		Transcript cr; show: Time now printString.
		index = 5 ifTrue: [Processor activeProcess suspend]]] fork.
(Delay forSeconds: 5) wait.
process resume.

