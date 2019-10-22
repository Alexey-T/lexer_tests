--Eric Etheridge
--2009 05 18
--ExREtoFAconvertREv2.hs

--stage 1: first input string to RE (3 versions across 3 modules with 2 submodules)
--version 2, using a submodule.
--Only a few simple access functions are written for the monad which carries the state,
--so some extra code is needed in a few functions here.


module ExREtoFAconvertREv2 (
		--version 2, monadic
	convertStringToRE2
	)
	where


--import Debug.Trace
import ExREtoFAtypes
import ExREtoFAconvertREv2monad
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Char (isDigit, isLower, isUpper)
import Control.Monad (when)


{-#### Conversion from RE String to Regular Expression, Version #2: Monadic ####-}


--This function will return Nothing if the string is not acceptable.
--This function does not take apart a monadic state,
--and would normally be in a separate file from the monad type and accessor functions.
convertStringToRE2 :: String -> Maybe MyRegExp
		--It is an error to not parse all the input.
		--For instance, this happens when a right paren has no matching left paren.
convertStringToRE2 str = if restCs == ""
	then
--	trace ("convertStringToRE2: mayReSeq: " ++ (show mayReSeq))
	mayReSeq
	else
--	trace "convertStringToRE2: restCs /= \"\""
	Nothing
	where
	--The composed monad action is invoked here.
	--Note that the return values are in the opposite order from convertStringToRE1.
	--This is a more natural order given the monad type.
	--An additional change is that the whole tuple is always returned here.
	(restCs, mayReSeq) = runRE2monadReturnState str (convertSREouter2 [])


--Data.Maybe: fromJust
--This function takes an RE sequence as an input so that it can appropriate combine new and old expressions.
--This function must compose a REmaker2 monad action,
--so either accessor functions for the monad must exist, or it needs the RE2 constructor.
--I chose the first method, making this function only an indirect accessor of the monad.
--This function would normally be outside the scope of the constructor 'RE2'.
convertSREouter2 :: MyRegExp -> REmaker2 (Maybe MyRegExp)
convertSREouter2 re = 
--	trace ("convertSREouter2: re: " ++ (show re)) $
	do
	--For the actions to work, a right paren must not be removed from the input when encountered.
	--Instead, we return the input with the right paren and the newly constructed RE.
	--If the right paren has no match, we will return to 'convertStringToRE2' and fail on a non-empty return.
	nextOne <- peekChars2 1
	if nextOne == [')']
		then return (Just re)
		else do
		mayC <- mayGetChar2
		if mayC == Nothing
			--The constructed expression is finished when the end of the string is reached.
			then return (Just re)
			--Otherwise, we call a helper function which pattern matches on the character.
			else convertOuter2help (fromJust mayC) re


--A character may fall into one of many categories.
--We will use guards for the simple categories and patterns for those which require special processing.
--This function would normally be outside the scope of the constructor 'RE2'.
convertOuter2help :: Char -> MyRegExp -> REmaker2 (Maybe MyRegExp)

--SPECIAL HANDLING:

--Square blocks are parsed and added to the list.
	--The result is a MatchOne or a MatchNone value, unless the parse failed on the string.
	--The characters used to create the matchSome item must not be converted again.
convertOuter2help '[' re = do
	mayMatchSome <- convertSquareBlock2
	--If the square block failed, we fail.
	if mayMatchSome == Nothing
		--Each result of the conditional must be a monadic action,
		--so this is 'return Nothing' rather than 'Nothing',
		--and so on for all the further instances below.
		then return Nothing
		--Otherwise, we add the MatchSome and continue.
		else convertSREouter2 (re ++ [fromJust mayMatchSome])

--Open parens create a sequence that is affected by later modifiers.
	--The current sequence is not.
	--Therefore we create a new sequence.
	--This is a recursive call, and is the reason that the function also returns a string.
	--Once the recursive call is done, there must be a right paren as the next input character.
	--REsubExp is used so that the post modifiers (*, ?, {}, etc.) can be implemented simply,
	--rather than concatenating the two expressions.
	--We need the right paren to not be read again, so we pass 'tail restCs'.
convertOuter2help '(' re = do
	--First we get the sub expression.
	maySubSeq <- convertSREouter2 []
	--If the sub expression failed, we fail.
	if maySubSeq == Nothing
		then return Nothing
		else do
		--We need to get rid of the right paren after the above expression.
		mayRightParen <- mayGetChar2
		--Anything other than a right paren is an error, and we fail.
		if mayRightParen /= Just ')'
			then return Nothing
			--Otherwise, we add the sub expression and continue.
			else convertSREouter2 (re ++ [REsubExp (fromJust maySubSeq)])


--A number operator fails if there is no prior item.
	--Otherwise it duplicates the item m times then adds the item with a "or empty" property n-m times.
	--It is legal for 'm' to be zero, so we have to use 'init' rather than trying to optimize.
	--The characters used to create the {m,n} item must not be converted again.
convertOuter2help '{' re = if re == [] then return Nothing else do
	mayNums <- convertNumberBlock2
	--If the number block failed, we fail.
	if mayNums == Nothing
		then return Nothing
		else do
		--Otherwise, we add the expression copies and continue.  This takes some arrangement.
			--Here we use a 'let' in a 'do block' twice.
			--The first time we assign an expression to a pattern, to get at its pieces.
		let Just (m, n) = mayNums
		--This is used in two places below, so calculating it once avoids a little work.
		let lastItem = last re
		--This is the final recursive call.
		convertSREouter2 (init re ++ replicate m lastItem ++ replicate (n-m) (REGrpOrEmpty lastItem))


--The pipe operator behaves differently from others in that it affects the entire prior sequence.
	--I.e. aaa|bbb matches aaa or bbb, not aaabb or aabbb.
	--The remaining input is used to create a new RE sequence.
	--Both prior and new must be non-empty.
	--The next sequence may not parse the entire string, for instance if '|' is used inside parentheses.
	--Thus the returned part of the string must be passed back along with the constructed REGrpOr.
	--There is no recursive call since the next expression should have consumed as much input as possible.
convertOuter2help '|' re = if re == [] then return Nothing else do
	--Second, we get the next expression.
	mayNextSeq <- convertSREouter2 []
	--If the next expression failed, or if it contains no actions, we fail.
	if mayNextSeq == Nothing || fromJust mayNextSeq == []
		then return Nothing
		--Otherwise, we add the next expression and return the constructed choice.
		else return (Just [REGrpOr re (fromJust mayNextSeq)])


--A '\' requires a following character.  That character is treated as a literal no matter what it is.
	--Exceptions: '\t' (none other currently)
	--That character must not be read twice.
convertOuter2help '\\' re = do
	--First, we get the next character.
	mayNextC <- mayGetChar2
	--If there was no next character, we fail.
	if mayNextC == Nothing
		then return Nothing
		else do
		--Otherwise, we add the literal and continue.
		let Just c = mayNextC
		let nextChar = if c == 't' then '\t' else c
		convertSREouter2 (re ++ [Literal nextChar])


--SIMPLE CASES:
convertOuter2help c re
	--If the character is not a special character, it is a literal and should be added to the current sequence.
	| not (c `elem` specialREouterChars)	= convertSREouter2 (re ++ [Literal c])

	--A '.' outside of a [ ] block indicates that a wild card value.
	| c == '.'	= convertSREouter2 (re ++ [Wildcard])


	--Right parens are handled above in 'convertSREouter2'.

	--A star operator fails if there is no prior item.
	--Otherwise it makes the previous item have a "star" property.
	| c == '*'	= if re == [] then return Nothing else convertSREouter2 (init re ++ [REGrpStar (last re)])

	--A plus operator fails if there is no prior item.
	--Otherwise it copies the prior item with a "star" property.
	| c == '+'	= if re == [] then return Nothing else convertSREouter2 (re ++ [REGrpStar (last re)])

	--A question operator fails if there is no prior item.
	--Otherwise it makes the previous item have a "or empty" property.
	| c == '?'	= if re == [] then return Nothing else convertSREouter2 (init re ++ [REGrpOrEmpty (last re)])

	--A failed parse includes running into a closing character with no earlier open character.
	| otherwise	= return Nothing


--Control.Monad: when
--Data.Char: isLower, isUpper, isDigit
--Data.List: nub
--This function also matches and removes the closing ']'.
--Duplicates in the list are removed at the end.
convertSquareBlock2 :: REmaker2 (Maybe MyRegExpItem)
convertSquareBlock2 = do
	nextOne <- peekChars2 1
		--'matchMethod' is a variable for a data constructor.  That's allowed.
	let matchMethod = if nextOne == ['^'] then MatchNone else MatchOne
		--We must not consider a leading '^' to be a character or part of a range.
		--Here we get the '^' character and ignore it if necessary.
	when (nextOne == ['^']) (do {dummy <- mayGetChar2; return ()})
	mayCharsInRange <- convertSBrange []
	--If the range block failed, we fail.
	if mayCharsInRange == Nothing
		then return Nothing
		--Otherwise, we return the appropriate match method for distinct characters.
		else return (Just (matchMethod (nub (fromJust mayCharsInRange))))

--We need to keep track of the characters so far allowed or disallowed, so we pass forward a [Char].
--There's no reason to bother about the order of the found characters.
convertSBrange :: [Char] -> REmaker2 (Maybe [Char])
convertSBrange foundChars = do
	nextThree <- peekChars2 3
	case nextThree of
		--When there is exactly one character left, it must be a ']'.
		[x]	-> do
			dummyX <- mayGetChar2	--remove x
			return (if x == ']' then Just foundChars else Nothing)
		--When there are no characters left, no ending ']' was found, so the parse fails.
		[]	-> return Nothing
		--Otherwise there are at least two characters, which has several possibilities.
		_	-> convertSBhelp nextThree foundChars

	where
	convertSBhelp :: [Char] -> [Char] -> REmaker2 (Maybe [Char])
	--When there is more than one character left, we may have a range or a literal.
	--This function is a good study of logical cases.
	--None are left out, but that is not easy to verify by visual inspection.
	convertSBhelp (x:y:zs) foundChars
		--A dash cannot appear without being escaped as '\-'.
		| x == '-'	= return Nothing

		--If the next character is a close bracket, then the range is done.
		| x == ']'	= do
			dummyX <- mayGetChar2	--remove x
			return (Just foundChars)

		--A '\' is used to indicate a '-' literal.  This is not checked, a simplification.
		--I assume here that no literals using '\' can also be part of a range.
		--(i.e. '\t' then '-' will not need to be handled.)
		| x == '\\'	= do
			dummyX <- mayGetChar2	--remove x
			dummyY <- mayGetChar2	--remove y
			convertSBrange (y:foundChars)

		--When the first character is not a control, it is a literal.
		--If the second character is not a '-', then x it is a lone literal.
		| y /= '-'	= do
			dummyX <- mayGetChar2	--remove x
			convertSBrange (x:foundChars)

		--Beyond here, y == '-'.
		--No matter what, x is not a range start if the third character is ']'.
		--In this case, x is a lone literal.
		| (zs /= []) && (head zs == ']')	= do
			dummyX <- mayGetChar2	--remove x
			convertSBrange (x:foundChars)

		--Otherwise, it is a range start.
		--There must be a third character.
		--Duplicate characters are not prevented here.
		| otherwise	= if (zs == []) || not okayRange
			then return Nothing
				--The (..) operator is an inclusive range operator.
				--The first character in zs is read, so it must not be read again.
			else do
				dummyX <- mayGetChar2	--remove x
				dummyY <- mayGetChar2	--remove y
				dummyZ <- mayGetChar2	--remove z
				convertSBrange (foundChars ++ [x..z])
			where
				--Avoid extra writing and calculation
			z = head zs
				--A range is only legal if it starts and stops on the same kind of character.
				--This is not technically true but will prevent odd behavior.
				--These functions come from Data.Char.
			okayRange = isLower x && isLower z || isUpper x && isUpper z || isDigit x && isDigit z


--Data.Char: isDigit
--Data.List: span
--Spaces are not allowed.
--n need not be greater than m, and we use replicate above which accepts that appropriately.
--This function also matches and removes the closing '}'.
convertNumberBlock2 :: REmaker2 (Maybe (Int, Int))
convertNumberBlock2 = do
	--Note that no string is passed from function to function.
	--This is simpler and much less prone to errors.
	digitsM <- spanREmaker2 isDigit
	theComma <- spanREmaker2 (== ',')
	digitsN <- spanREmaker2 isDigit
	theClose <- spanREmaker2 (== '}')
	if digitsM /= "" && theComma == "," && digitsN /= "" && theClose == "}"
		then return (Just (read digitsM, read digitsN))
		else return Nothing

	where
	--Unfortunately, without direct access to the state,
	--the original simple code cannot be written without a new function.
	--This function would be much simpler as a priveleged function, able to access the state.
	spanREmaker2 :: (Char -> Bool) -> REmaker2 [Char]
	spanREmaker2 checkFunc = spanHelp ""
		where
		spanHelp already = do
			nextOne <- peekChars2 1
			if nextOne == [] || not (checkFunc (head nextOne))
				then return already
				else do
				dummy <- mayGetChar2
				spanHelp (already ++ [head nextOne])



