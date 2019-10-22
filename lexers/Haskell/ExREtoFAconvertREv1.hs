--Eric Etheridge
--2009 05 18
--ExREtoFAconvertREv1.hs

--stage 1: first input string to RE (3 versions across 3 modules with 2 submodules)
--version 1.
--This is what you get when you write everything as a function without caring that you are passing a state forward.
--The code is fairly clear, but has a lot of repetition.


module ExREtoFAconvertREv1 (
		--version 1, non-monadic
	convertStringToRE1
	)
	where


--import Debug.Trace
import ExREtoFAtypes
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Char (isDigit, isLower, isUpper)
import Control.Monad (when)


{-#### Conversion from RE String to Regular Expression, Version #1: Non-Monadic ####-}

--Data.Maybe: fromJust, isJust
--This function will return Nothing if the string is not acceptable.
convertStringToRE1 :: String -> Maybe MyRegExp
		--It is an error to not parse all the input.
		--For instance, this happens when a right paren has no matching left paren.
convertStringToRE1 str = if isJust mayConvertSRE && restCs == "" then Just reSeq else Nothing
	where
	mayConvertSRE = convertSREouter1 str []
		--This will not be evaluated above unless 'isJust mayConvertSRE' returns True, so no error occurs.
	(reSeq, restCs) = fromJust mayConvertSRE



--Data.Maybe: fromJust, isJust
--This function takes an RE sequence as an input so that it can appropriate combine new and old expressions.
convertSREouter1 :: String -> MyRegExp -> Maybe (MyRegExp, String)

	--The constructed expression should be returned when the end of the string is reached.
convertSREouter1 "" re = Just (re, "")

--A character may fall into one of many categories.
--We will use guards for the simple categories and patterns for those which require special processing.

--SPECIAL HANDLING:

--Square blocks are parsed and added to the list.
convertSREouter1 ( '[':cs ) re
	--The result is a MatchOne or a MatchNone value, unless the parse failed on the string.
	--The characters used to create the matchSome item must not be converted again.
	= if isJust mayConvertSquare then convertSREouter1 restCs (re ++ [matchSome]) else Nothing
		--uses 'mayConvertSquare' below
		where
		mayConvertSquare = convertSquareBlock1 cs
		--This will not be evaluated above unless 'isJust mayConvertSquare' returns True, so no error occurs.
		(matchSome, restCs) = fromJust mayConvertSquare

--Open parens create a sequence that is affected by later modifiers.
convertSREouter1 ( '(':cs ) re
	--The current sequence is not.
	--Therefore we create a new sequence.
	--This is a recursive call, and is the reason that the function also returns a string.
	--Once the recursive call is done, there must be a right paren as the next input character.
	--REsubExp is used so that the post modifiers (*, ?, {}, etc.) can be implemented simply,
	--rather than concatenating the two expressions.
	--We need the right paren to not be read again, so we pass 'tail restCs'.
	= if isJust mayConvertOuter && nextRightParen
		then convertSREouter1 (tail restCs) (re ++ [REsubExp newSeq]) else Nothing
		where
		mayConvertOuter = convertSREouter1 cs []
			--This will not be evaluated above unless 'isJust mayConvertOuter' returns True, so no error occurs.
		(newSeq, restCs) = fromJust mayConvertOuter
			--This avoids an error in the case of an empty 'restCs' because (&&) is short-circuit.
			--The parentheses here are not necessary but add clarity.
		nextRightParen = (restCs /= "") && (head restCs == ')')


--A number operator fails if there is no prior item.
convertSREouter1 ( '{':cs ) re
	--Otherwise it duplicates the item m times then adds the item with a "or empty" property n-m times.
	--It is legal for 'm' to be zero, so we have to use 'init' rather than trying to optimize.
	--The characters used to create the {m,n} item must not be converted again.
	= if re == [] then Nothing else convertSREouter1 restCs
		(init re ++ replicate m lastItem ++ replicate (n-m) (REGrpOrEmpty lastItem))
		where
		mayConvertNumbers = convertNumberBlock1 cs
		--This will not be evaluated above unless 'isJust mayConvertNumbers' returns True, so no error occurs.
		(m, n, restCs) = fromJust mayConvertNumbers
		--This is used in several places above, so calculating it once avoids a little work.
		lastItem = last re

--The pipe operator behaves differently from others in that it affects the entire prior sequence.
convertSREouter1 ( '|':cs ) re
	--I.e. aaa|bbb matches aaa or bbb, not aaabb or aabbb.
	--The remaining input is used to create a new RE sequence.
	--Both prior and new must be non-empty.
	--The next sequence may not parse the entire string, for instance if '|' is used inside parentheses.
	--Thus the returned part of the string must be passed back along with the constructed REGrpOr.
	= if (re /= []) && (isJust mayConvertOuter) && (newSeq /= [])
		then Just ([REGrpOr re newSeq], restCs) else Nothing
		where
		mayConvertOuter = convertSREouter1 cs []
		--This will not be evaluated above unless 'isJust mayConvertOuter' returns True, so no error occurs.
		(newSeq, restCs) = fromJust mayConvertOuter


--A '\' requires a following character.  That character is treated as a literal no matter what it is.
convertSREouter1 ( '\\':cs ) re
	--Exceptions: '\t' (none other currently)
	--That character must not be read twice.
	= if (cs /= []) then convertSREouter1 (tail cs) (re ++ [Literal nextChar]) else Nothing
		where
		nextChar = if head cs == 't' then '\t' else head cs

--SIMPLE CASES:
convertSREouter1 (c:cs) re
	--If the character is not a special character, it is a literal and should be added to the current sequence.
	| not (c `elem` specialREouterChars)	= convertSREouter1 cs (re ++ [Literal c])

	--A '.' outside of a [ ] block indicates that a wild card value.
	| c == '.'	= convertSREouter1 cs (re ++ [Wildcard])


	--For the above to work, a right paren must not be removed from the input when encountered.
	--Instead, we return the input with the right paren and the newly constructed RE.
	--If the right paren has no match, we will return to 'convertStringToRE1' and fail on a non-empty return.
	| c == ')'	= Just (re, (c:cs))

	--A star operator fails if there is no prior item.
	--Otherwise it makes the previous item have a "star" property.
	| c == '*'	= if re == [] then Nothing else convertSREouter1 cs (init re ++ [REGrpStar (last re)])

	--A plus operator fails if there is no prior item.
	--Otherwise it copies the prior item with a "star" property.
	| c == '+'	= if re == [] then Nothing else convertSREouter1 cs (re ++ [REGrpStar (last re)])

	--A question operator fails if there is no prior item.
	--Otherwise it makes the previous item have a "or empty" property.
	| c == '?'	= if re == [] then Nothing else convertSREouter1 cs (init re ++ [REGrpOrEmpty (last re)])

	--A failed parse includes running into a closing character with no earlier open character.
	| otherwise	= Nothing


--Data.Maybe: fromJust, isJust
--Data.Char: isLower, isUpper, isDigit
--Data.List: nub
--This function also matches and removes the closing ']'.
--Duplicates in the list are removed at the end.
convertSquareBlock1 :: String -> Maybe (MyRegExpItem, String)
convertSquareBlock1 cs = if (cs /= []) && isJust maySBhelp then Just (matchMethod (nub charsInRange), restCs) else Nothing
	where
		--'matchMethod' is a variable for a data constructor.  That's allowed.
		--This will not be evaluated above unless 'cs /= []' returns True, so no error occurs.
		--This is a good example of combining work for the same condition using tuples.
	(matchMethod, workCs) = if head cs == '^' then (MatchNone, tail cs) else (MatchOne, cs)
		--We must not consider a leading '^' to be a character or part of a range.
		--'workCs', calculated above, does not include a leading '^' if present.
	maySBhelp = convertSBhelp workCs []
		--This will not be evaluated above unless 'isJust maySBhelp' returns True, so no error occurs.
	(charsInRange, restCs) = fromJust maySBhelp

	--We need to keep track of the characters so far allowed or disallowed, so we pass forward a [Char].
	--There's no reason to bother about the order of the found characters.
	convertSBhelp :: String -> [Char] -> Maybe ([Char], String)
		--When there is more than one character left, we may have a range or a literal.
		--This function is a good study of logical cases.
		--None are left out, but that is not easy to verify by visual inspection.
	convertSBhelp (x:y:zs) foundChars
			--A dash cannot appear without being escaped as '\-'.
		| x == '-'	= Nothing
			--If the next character is a close bracket, then the range is done.
		| x == ']'	= Just (foundChars, y:zs)
			--A '\' is used to indicate a '-' literal.  This is not checked, a simplification.
			--I assume here that no literals using '\' can also be part of a range.
			--(i.e. '\t' will not need to be handled.)
		| x == '\\'	= convertSBhelp zs (y:foundChars)
			--When the first character is not a control, it is a literal.
			--If the second character is not a '-', then x it is a lone literal.
		| y /= '-'	= convertSBhelp (y:zs) (x:foundChars)
			--It is not a range start if the third character is ']'.
		| (zs /= []) && (head zs == ']')	= convertSBhelp (y:zs) (x:foundChars)
			--Otherwise, it is a range start.
			--There must be a third character.
			--Duplicate characters are not prevented here.
		| otherwise	= if (zs == []) then Nothing else
				--The (..) operator is an inclusive range operator.
				--The first character in zs is read, so it must not be read again.
			if okayRange then convertSBhelp (tail zs) (foundChars ++ [x..z]) else Nothing
			where
				--Avoid extra writing and calculation
			z = head zs
				--A range is only legal if it starts and stops on the same kind of character.
				--This is not technically true but will prevent odd behavior.
				--These functions come from Data.Char.
			okayRange = isLower x && isLower z || isUpper x && isUpper z || isDigit x && isDigit z

		--When there is exactly one character left, it must be a ']'.
	convertSBhelp [x] foundChars	= if x == ']' then Just (foundChars, "") else Nothing
		--When there are no characters left, no ending ']' was found, so the parse fails.
	convertSBhelp [] _	= Nothing


--Data.Char: isDigit
--Data.List: span
--Spaces are not allowed.
--n need not be greater than m, and we use replicate above which accepts that appropriately.
--This function also matches and removes the closing '}'.
convertNumberBlock1 :: String -> Maybe (Int, Int, String)
convertNumberBlock1 cs = if digitsM /= "" && theComma == "," && digitsN /= "" && theClose == "}"
	then Just (read digitsM, read digitsN, afterClose)
	else Nothing
	where
	(digitsM, afterDigitsM) = span isDigit cs
	(theComma, afterComma) = span (== ',') afterDigitsM
	(digitsN, afterDigitsN) = span isDigit afterComma
	(theClose, afterClose) = span (== '}') afterDigitsN




