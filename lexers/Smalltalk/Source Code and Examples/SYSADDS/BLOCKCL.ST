'From VisualWorks(R), Release 2.5 of September 26, 1995 on February 2, 1996 at 6:17:52 am'!



!BlockClosure methodsFor: '(additions)'!

forkAt: priority withName: aString
	"Create and schedule a process running the code in the receiver.
	The priority of the process is the argument, priority.
	Answer the new process."

	| forkedProcess |
	self newMethod: 'When. Who. Why'.
	forkedProcess := self newProcessWithName: aString.
	forkedProcess priority: priority.
	forkedProcess resume.
	^forkedProcess!

forkWithName: aString
	"Create and schedule a process running the code in the receiver.
	Answer the new process."

	| forkedProcess |
	self newMethod: 'When. Who. Why'.
	forkedProcess := self newProcessWithName: aString.
	forkedProcess resume.
	^forkedProcess!

newProcessWithName: aString
	"Answer a new process running the code in the receiver.
	The process is not scheduled."

	| process |
	self newMethod: 'When. Who. Why'.
	process := MyProcess
		forContext: 
			[Process terminateSignal
				handle: [:ex | ex return]
				do: self.
			Processor activeProcess finished]
				newContext
		priority: Processor activePriority.
	process processName: aString.
	^process! !
