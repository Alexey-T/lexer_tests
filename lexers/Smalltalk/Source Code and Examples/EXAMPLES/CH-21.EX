 "Examples for Chapter 21, Debugging.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

self halt: 'My string to display'.
self notify: 'Count is now over 100'.
self error: 'Should not get here'.

NotifierView shortStackFor: thisContext ofSize: 99.

Screen default ringBell.

OrderedCollection allInstances inspect.

(self allOwnersWeakly: true) inspect.

