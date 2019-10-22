 "Examples for Chapter 17, Coordinating and Sequencing Events.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

| semaphore |
semaphore := Semaphore new.
[Transcript cr; show: 'About to wait'.
semaphore wait.
Transcript cr; show: 'Semaphore signaled'] fork.
[(Delay forSeconds: 5) wait.
semaphore signal] fork.

| accessProtect |
accessProtect := Semaphore forMutualExclusion.
[accessProtect critical:
	[Transcript cr; show: 'Process1 ', Time now printString.
	(Delay forSeconds: 5) wait]] fork.
[accessProtect critical:
	[Transcript cr; show: 'Process2 ', Time now printString]] fork.
