--Eric Etheridge
--2009 05 18
--ExREtoFAconvertRE.hs

--stage 1: first input string to RE (3 versions across 3 modules with 2 submodules)
--This is the most complicated stage because it deals with the interface between human meaning and data arrangement.
--There are three methods of conversion implemented in order to show the basics and then show monads.
--Check each file for details.


module ExREtoFAconvertRE (
		--version 1, non-monadic
	convertStringToRE1,

		--version 2, monadic (has a submodule for the monad)
	convertStringToRE2,

		--version 3, monadic streamlined (has a submodule for the monad)
	convertStringToRE3
	)
	where

import ExREtoFAconvertREv1
import ExREtoFAconvertREv2
import ExREtoFAconvertREv3
