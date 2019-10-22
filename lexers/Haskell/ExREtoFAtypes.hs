--Eric Etheridge
--2009 05 18
--ExREtoFAtypes.hs


--This file is imported by each of the other source files for this example.

module ExREtoFAtypes (
		--constants
	specialREouterChars,
		--RE types
	MyRegExp, MyRegExpItem(..),

		--FA types
	MyFAInputs, MyFA, MyDFA, MyFALabels,

		--validation functions
	validInputString, validFA, validDFA,

		--inspection functions
	findCharsReferenced, findEpsClosure
	)
	where


--It's a good idea to list a few specific functions that you are importing
--in case the libaries change and you need to track new locations of functions.
import Data.List (nub, sort, (\\), union)
import Data.Maybe (catMaybes, isNothing)


{-#### Regular Expression Reference ####-}

{-
POSIX minus \#, \n, ^, and * (non-regular and line-referring symbols):
., [], [^], (), +, *, |, {m,n}, ?

. outside of [] matches anything
. in [] matches .
[ ] and [^ ] include literals, literal dash literal, and \- which represents - (dash)
use ord functions to find all intermediate characters of ranges
{m,n} bounds the number of repetitions of the previous object:
x{m,n} == x written m times then (x or empty) written n minus m times
x? == (x or empty)
x|y can be extended: x|y|z, note aaa|bbb is aaa or bbb, not aaabb or aabbb
\t and blank must be included, and \
-}


{-#### Constants ####-}

--'^' is only special in a [] block, at the very front.
--Also, we are not allowing ], }, or ) without matching open character, to prevent typos in the RE string.
specialREouterChars = "[]\\.{}?|()+*"	--'\\' == the \ character


{-#### Regular Expression Types ####-}

type MyRegExp = [MyRegExpItem]
data MyRegExpItem = Literal Char | Wildcard | MatchOne [Char] | MatchNone [Char]
	| REGrpStar MyRegExpItem | REGrpOrEmpty MyRegExpItem
	| REsubExp MyRegExp | REGrpOr MyRegExp MyRegExp
	deriving (Eq, Show)
--convert +, {m,n} to REGrpMany (*) and REGrpOrEmpty (x or empty) before storing



{-#### Finite Automata Types ####-}

--Simple list for indicating valid inputs.
type MyFAInputs = [ Char ]

--Finite Automata for recognizing Strings.
--An arc on Nothing represents an epsilon transition (as opposed to some sort of wildcard).
--Each item in the out list represents a node, numbered by position, zero-indexed.
--The bool represents whether the state is an accepting state
--State 0 starts.
--The arc list is written as an association list, a list of two-tuples of (key, value) pairs.
type MyFA = [ ([ (Maybe Char, Int) ], Bool) ]

--We have to include what inputs are valid in a DFA because DFAs are defined over a given alphabet.
--NFAs are also defined over a given alphabet, but since it is acceptable to have no arcs for an input,
--that alphabet does not need to be known to have a valid NFA.
type MyDFA = ( MyFAInputs, MyFA )

--Separate, actually unrelated list for keeping labels if necessary.
--This type exists mostly to make clear that the main FA type doesn't include labels.
type MyFALabels = [ String ]



{-#### Validating FAs ####-}

--Data.List: nub, \\
--This function returns true iff an input string is valid for a given list of valid inputs.
--An input string is valid if it contains no characters not in the list of valid characters.
--(\\) removes at most one element from its left argument for each element in its right argument,
--so we need to find the unique characters of the input string.
--If all of these unique characters are in the valid list, then the difference is empty and the string is valid.
validInputString :: MyFAInputs -> String -> Bool
validInputString validInputs inputString = nub inputString \\ validInputs == []


--This function returns True iff a prospective FA has arcs which all point to existing states in the FA's list,
--and the FA has at least one state (the start state is currently hardcoded as state 0).
validFA :: MyFA -> Bool
	--This checks for an FA with no states, which is not allowed since there must be a start state.
validFA [] = False
	--This is a double mapping, which uses 'and' across all the target states.
	--If any are outside the allowed range, the function returns False.
	--'and []' == True, and no arcs would be acceptable, so this acts appropriately in that case.
validFA fa@(_:_) = and $ map (and . map (\(_, x) -> 0 <= x && x < length fa) . fst) fa


--Data.List: sort, nub
--Data.Maybe: catMaybes
--This function calls validFA first.
--This function returns True iff a prospective DFA has exactly one arc from each state for each valid input.
--Also, no epsilon transitions are allowed.
validDFA :: MyDFA -> Bool
	--We need to check that each arc list is valid, so we call functions with boolean return values on each.
	--'and $ map' could also be done with a fold, but this is clear enough.
	--Yes, this is a lambda function calling named helper functions.  That's allowed.
	--(&&) is short-circuit, so if validFA fails the rest of the definition will be ignored.
	--'and []' == True, which but an FA with no arcs is not valid, so that will still fail appropriately.
validDFA ( validInputs, dfa ) = (validFA dfa) && (and validArcs)
	where
	validArcs = map (\(arcs, _) -> (correctInputs arcs) && (noEps arcs)) dfa
		--We compare based on a list of sorted, distinct inputs.
	sortedUniqueInputs = sort $ nub $ validInputs
		--The first function checked on each arc list checks for exactly one arc for each input.
		--In English, this reads, "The valid inputs equal the inputs for sorted unique non-epsilon arcs."
		--The target state of those arcs is not important, so 'map fst' is used to remove them.
		--For new users of Haskell, remember that this "removal" does not actually affect 'arcs' or the 'dfa'.
		--Here we do NOT use 'nub', because duplication is a failure.
		--'catMaybes' is a function from Data.Maybe that returns the values from the Just entries in a list.
	correctInputs arcs = sortedUniqueInputs == (sort $ catMaybes $ map fst arcs)
		--The second function checked on each arc list checks for zero epsilon transitions.
		--This is a function defined without listing all of its parameters.
		--INCOMPLETE COMMENT: WHAT IS THIS CALLED?
		--Again, there are other ways to write 'not $ or $ map isNothing', but this is clearest.
		--In English, it reads, "There are not any values which are equal to Nothing."
	noEps xs = not $ or $ map (isNothing . fst) xs



{-#### Inspecting REs ####-}

--Data.List: nub
--This function calculates all of the characters specifically referenced by a regular expression.
--This can be used to determine the valid characters, but users should probably be given a choice.
findCharsReferenced :: MyRegExp -> MyFAInputs
findCharsReferenced re = sort $ nub $ findCharHelp [] re

findCharHelp :: MyFAInputs -> MyRegExp -> MyFAInputs
findCharHelp already (exp:exps) = findCharHelp (newChars exp ++ already) exps
	where
	newChars (Literal c) = [c]

	--Wildcards do not specifically refer to any character.
	newChars Wildcard = []

	newChars (MatchOne cs) = cs

	--MatchNone expressions do refer to characters.
	--Wildcards in the RE could match them.
	newChars (MatchNone cs) = cs

	newChars (REGrpStar item) = findCharHelp [] [item]
	newChars (REGrpOrEmpty item) = findCharHelp [] [item]
	newChars (REsubExp re) = findCharHelp [] re
	newChars (REGrpOr re1 re2) = (findCharHelp [] re1) ++ (findCharHelp [] re2)

findCharHelp already [] = already





{-#### Inspecting FAs ####-}


--Data.List: nub, \\, union
--This function finds the epsilon closure of a given set of states.
--It returns a sorted list of uniques.
--The epsilon closure is the set of reachable states from any given states using only epsilon arcs, i.e. no input.
findEpsClosure :: MyFA -> [Int] -> [Int]
		--'nub' is used here to avoid work, and the final result is sorted.
		--The original set must be included in the output, so 'xs' is the initial value of 'already'.
findEpsClosure theFA xs = sort $ findHelper (nub xs) xs
	where
		--This helper function is used to avoid passing around the original definition of 'theFA'.
		--In this function, any newly found node is added to the list of seen nodes.
		--Any newly found node not in the list of original nodes is added to that list (with 'union').
	findHelper :: [Int] -> [Int] -> [Int]
				--This is a tail-recursive call, easy for a compiler to optimize.
	findHelper (x:xs) already = findHelper xsWithNew (notAlready ++ already)
		where
			--First, we get the list of arcs for the given state.
			--The !! operator here will generate an error if state x is not listed (i.e. violates length).
		(arcList, _) = theFA !! x
			--Second, we get the list of distinct target states for epsilon arcs.
			--'nub' returns uniques only.
		epsArcs = nub $ [targetState | (mayChar, targetState) <- arcList, mayChar == Nothing]
			--Third, we compare this list with those already accumulated, finding the new states.
			--'(\\)' is "set minus", removing the first each (which is why nub was needed).
		notAlready = epsArcs \\ already
			--Fourth, we add the newly found state to the list of states to process, using a "set union".
			--There may be eps arcs to states in xs, so ++ is insufficient.
			--This could be ++, since the set difference above would remove duplicates,
			--However, not using 'union' here could result in infinitely long calculations.
			--(Consider two states in a set with epsilon arcs to each other.  Legal in an NFA.)
		xsWithNew = xs `union` notAlready

		--The base case of the helper function returns all those states accumulated.
	findHelper [] already = already
