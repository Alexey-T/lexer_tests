--Eric Etheridge
--2009 05 18
--ExREtoFAconvertREv3.hs

--stage 1: first input string to RE (3 versions across 3 modules with 2 submodules)
--version 3, using a submodule.
--Some extra useful access functions are written for the monad which carries the state,
--so the extra code in version 2 is avoided here.


module ExREtoFAconvertREv3 (
		--version 3, monadic streamlined
	convertStringToRE3
	)
	where


--import Debug.Trace
import ExREtoFAtypes
import ExREtoFAconvertREv3monad
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Char (isDigit, isLower, isUpper)
import Control.Monad (when)


{-#### Conversion from RE String to Regular Expression, Version #3: MonadPlus Was Unnecessary ####-}

--This version does not use Monadplus to simplify the code for failed parses.
--The complexity of the extra state information was more than what was saved by using 'mzero'.
--Instead, we'll add a function that lets us return Nothing more easily.
--We will also add two other functions that streamline other common monad accesses.


--This function does not use internals but simplifies a lot of code.
--It is used to reach error states more easily.
failNothing :: Maybe a -> REmaker3 (Maybe b) -> REmaker3 (Maybe b)
failNothing Nothing _ = return Nothing
failNothing _ action = action



--This function will return Nothing if the string is not acceptable.
--This function does not take apart a monadic state,
--and would normally be in a separate file from the monad type and accessor functions.
convertStringToRE3 :: String -> Maybe MyRegExp
		--It is an error to not parse all the input.
		--For instance, this happens when a right paren has no matching left paren.
convertStringToRE3 str = if restCs == "" then mayReSeq else Nothing
	where
	--The composed monad action is invoked here.
	--Note that the return values are in the opposite order from convertStringToRE1.
	--This is a more natural order given the monad type.
	--An additional change is that the whole tuple is always returned here.
	(restCs, mayReSeq) = runRE3monadReturnState str (convertSREouter3 [])


--Data.Maybe: fromJust
--This function takes an RE sequence as an input so that it can appropriate combine new and old expressions.
--This function must compose a REmaker3 monad action,
--so either accessor functions for the monad must exist, or it needs the RE3 constructor.
--I chose the first method, making this function only an indirect accessor of the monad.
--This function would normally be outside the scope of the constructor 'RE3'.
convertSREouter3 :: MyRegExp -> REmaker3 (Maybe MyRegExp)
convertSREouter3 re = 
--	trace ("convertSREouter3: re: " ++ (show re)) $
	do
	--For the actions to work, a right paren must not be removed from the input when encountered.
	--Instead, we return the input with the right paren and the newly constructed RE.
	--If the right paren has no match, we will return to 'convertStringToRE3' and fail on a non-empty return.
	nextOne <- peekChars3 1
	if nextOne == [')']
		then return (Just re)
		else do
		mayC <- mayGetChar3
		if mayC == Nothing
			--The constructed expression is finished when the end of the string is reached.
			then return (Just re)
			--Otherwise, we call a helper function which pattern matches on the character.
			else convertOuter3help (fromJust mayC) re


--A character may fall into one of many categories.
--We will use guards for the simple categories and patterns for those which require special processing.
--This function would normally be outside the scope of the constructor 'RE3'.
convertOuter3help :: Char -> MyRegExp -> REmaker3 (Maybe MyRegExp)

--SPECIAL HANDLING:

--Square blocks are parsed and added to the list.
	--The result is a MatchOne or a MatchNone value, unless the parse failed on the string.
	--The characters used to create the matchSome item must not be converted again.
convertOuter3help '[' re = do
	mayMatchSome <- convertSquareBlock3
	--If the square block failed, we fail.
	failNothing mayMatchSome $ convertSREouter3 (re ++ [fromJust mayMatchSome])

--Open parens create a sequence that is affected by later modifiers.
	--The current sequence is not.
	--Therefore we create a new sequence.
	--This is a recursive call, and is the reason that the function also returns a string.
	--Once the recursive call is done, there must be a right paren as the next input character.
	--REsubExp is used so that the post modifiers (*, ?, {}, etc.) can be implemented simply,
	--rather than concatenating the two expressions.
	--We need the right paren to not be read again, so we pass 'tail restCs'.
convertOuter3help '(' re = do
	--First we get the sub expression.
	maySubSeq <- convertSREouter3 []
	--If the sub expression failed, we fail.
	failNothing maySubSeq $ do
		--We need to get rid of the right paren after the above expression.
		mayRightParen <- mayGetChar3
		--Anything other than a right paren is an error, and we fail.
		if mayRightParen /= Just ')'
			then return Nothing
			--Otherwise, we add the sub expression and continue.
			else convertSREouter3 (re ++ [REsubExp (fromJust maySubSeq)])


--A number operator fails if there is no prior item.
	--Otherwise it duplicates the item m times then adds the item with a "or empty" property n-m times.
	--It is legal for 'm' to be zero, so we have to use 'init' rather than trying to optimize.
	--The characters used to create the {m,n} item must not be converted again.
convertOuter3help '{' re = if re == [] then return Nothing else do
	mayNums <- convertNumberBlock3
	--If the number block failed, we fail.
	failNothing mayNums $ do
		--Otherwise, we add the expression copies and continue.  This takes some arrangement.
			--Here we use a 'let' in a 'do block' twice.
			--The first time we assign an expression to a pattern, to get at its pieces.
		let Just (m, n) = mayNums
		--This is used in two places below, so calculating it once avoids a little work.
		let lastItem = last re
		--This is the final recursive call.
		convertSREouter3 (init re ++ replicate m lastItem ++ replicate (n-m) (REGrpOrEmpty lastItem))


--The pipe operator behaves differently from others in that it affects the entire prior sequence.
	--I.e. aaa|bbb matches aaa or bbb, not aaabb or aabbb.
	--The remaining input is used to create a new RE sequence.
	--Both prior and new must be non-empty.
	--The next sequence may not parse the entire string, for instance if '|' is used inside parentheses.
	--Thus the returned part of the string must be passed back along with the constructed REGrpOr.
	--There is no recursive call since the next expression should have consumed as much input as possible.
convertOuter3help '|' re = if re == [] then return Nothing else do
	--Second, we get the next expression.
	mayNextSeq <- convertSREouter3 []
	--If the next expression failed, or if it contains no actions, we fail.
	failNothing mayNextSeq $ if fromJust mayNextSeq == []
		then return Nothing
		--Otherwise, we add the next expression and return the constructed choice.
		else return (Just [REGrpOr re (fromJust mayNextSeq)])


--A '\' requires a following character.  That character is treated as a literal no matter what it is.
	--Exceptions: '\t' (none other currently)
	--That character must not be read twice.
convertOuter3help '\\' re = do
	--First, we get the next character.
	mayNextC <- mayGetChar3
	--If there was no next character, we fail.
	failNothing mayNextC $ do
		--Otherwise, we add the literal and continue.
		let Just c = mayNextC
		let nextChar = if c == 't' then '\t' else c
		convertSREouter3 (re ++ [Literal nextChar])


--SIMPLE CASES:
convertOuter3help c re
	--If the character is not a special character, it is a literal and should be added to the current sequence.
	| not (c `elem` specialREouterChars)	= convertSREouter3 (re ++ [Literal c])

	--A '.' outside of a [ ] block indicates that a wild card value.
	| c == '.'	= convertSREouter3 (re ++ [Wildcard])


	--Right parens are handled above in 'convertSREouter3'.


	--A star operator fails if there is no prior item.
	--Otherwise it makes the previous item have a "star" property.
	| c == '*'	= if re == [] then return Nothing else convertSREouter3 (init re ++ [REGrpStar (last re)])

	--A plus operator fails if there is no prior item.
	--Otherwise it copies the prior item with a "star" property.
	| c == '+'	= if re == [] then return Nothing else convertSREouter3 (re ++ [REGrpStar (last re)])

	--A question operator fails if there is no prior item.
	--Otherwise it makes the previous item have a "or empty" property.
	| c == '?'	= if re == [] then return Nothing else convertSREouter3 (init re ++ [REGrpOrEmpty (last re)])

	--A failed parse includes running into a closing character with no earlier open character.
	| otherwise	= return Nothing


--Control.Monad: when
--Data.Char: isLower, isUpper, isDigit
--Data.List: nub
--This function also matches and removes the closing ']'.
--Duplicates in the list are removed at the end.
convertSquareBlock3 :: REmaker3 (Maybe MyRegExpItem)
convertSquareBlock3 = do
	nextOne <- peekChars3 1
		--'matchMethod' is a variable for a data constructor.  That's allowed.
	let matchMethod = if nextOne == ['^'] then MatchNone else MatchOne
		--We must not consider a leading '^' to be a character or part of a range.
		--Here we get the '^' character and ignore it if necessary.
	when (nextOne == ['^']) (remChars3 1)
	mayCharsInRange <- convertSBrange []
	--If the range block failed, we fail.
	failNothing mayCharsInRange $ return (Just (matchMethod (nub (fromJust mayCharsInRange))))


--We need to keep track of the characters so far allowed or disallowed, so we pass forward a [Char].
--There's no reason to bother about the order of the found characters.
convertSBrange :: [Char] -> REmaker3 (Maybe [Char])
convertSBrange foundChars = do
	nextThree <- peekChars3 3
	case nextThree of
		--When there is exactly one character left, it must be a ']'.
		[x]	-> do
			remChars3 1	--remove x
			return (if x == ']' then Just foundChars else Nothing)
		--When there are no characters left, no ending ']' was found, so the parse fails.
		[]	-> return Nothing
		--Otherwise there are at least two characters, which has several possibilities.
		_	-> convertSBhelp nextThree foundChars

	where
	convertSBhelp :: [Char] -> [Char] -> REmaker3 (Maybe [Char])
	--When there is more than one character left, we may have a range or a literal.
	--This function is a good study of logical cases.
	--None are left out, but that is not easy to verify by visual inspection.
	convertSBhelp (x:y:zs) foundChars
		--A dash cannot appear without being escaped as '\-'.
		| x == '-'	= return Nothing

		--If the next character is a close bracket, then the range is done.
		| x == ']'	= do
			remChars3 1	--remove x
			return (Just foundChars)

		--A '\' is used to indicate a '-' literal.  This is not checked, a simplification.
		--I assume here that no literals using '\' can also be part of a range.
		--(i.e. '\t' then '-' will not need to be handled.)
		| x == '\\'	= do
			remChars3 2	--remove x, y
			convertSBrange (y:foundChars)

		--When the first character is not a control, it is a literal.
		--If the second character is not a '-', then x it is a lone literal.
		| y /= '-'	= do
			remChars3 1	--remove x
			convertSBrange (x:foundChars)

		--Beyond here, y == '-'.
		--No matter what, x is not a range start if the third character is ']'.
		--In this case, x is a lone literal.
		| (zs /= []) && (head zs == ']')	= do
			remChars3 1	--remove x
			convertSBrange (x:foundChars)

		--Otherwise, it is a range start.
		--There must be a third character.
		--Duplicate characters are not prevented here.
		| otherwise	= if (zs == []) || not okayRange
			then return Nothing
				--The (..) operator is an inclusive range operator.
				--The first character in zs is read, so it must not be read again.
			else do
				remChars3 3	--remove x, y, z
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
convertNumberBlock3 :: REmaker3 (Maybe (Int, Int))
convertNumberBlock3 = do
	--Note that no string is passed from function to function.
	--This is simpler and much less prone to errors.
	digitsM <- spanREmaker3 isDigit
	theComma <- spanREmaker3 (== ',')
	digitsN <- spanREmaker3 isDigit
	theClose <- spanREmaker3 (== '}')
	if digitsM /= "" && theComma == "," && digitsN /= "" && theClose == "}"
		then return (Just (read digitsM, read digitsN))
		else return Nothing

