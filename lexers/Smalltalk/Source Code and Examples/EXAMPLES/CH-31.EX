 "Examples for Chapter 31, Customizing Your Environment.
Copyright © 1996, Alec Sharp

To try the examples, highlight the appropriate code and select either
'do it', 'inspect it', or 'print it' from the operate menu, as appropriate.

This code should NOT be filed in.

------------------------------------------------------------------------------------------------------ "

LookPreferences deleteForward: true.

(ParagraphEditor classPool at: #Keyboard)
	bindValue: #cutKey: to: (TextConstants at: #Ctrlx).

"Sets the interrupt key to the default, ctrl-C"
InputState interruptKeyValue: (TextConstants at: #Ctrlc).

