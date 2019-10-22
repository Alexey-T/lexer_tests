--Eric Etheridge
--2009 05 18
--ExREtoFAexecution.hs

--stage 4: running RE, NFA, and DFA on further input strings (3 operations, 2 versions each)
--This is actually the simplest stage, as demonstrated by the brevity of the code in each method.
--The fact that it takes more work to run an RE than a DFA or an NFA is one of the reasons that conversion is usually performed.
--This file implements ALL of them in order to show the difference.


module ExREtoFAexecution (
		--version 1, w/ single stack
	runRE1, runDFA1, runNFA1,

		--version 2, w/ state sets
	runRE2, runDFA2, runNFA2

	)
	where

import ExREtoFAtypes
import Data.List (lookup)
import Data.Maybe (fromJust)


{-#### Executing FAs ####-}

--This returns Nothing on unacceptable input and Just a bool for accept or failure on valid input.
runDFA1, runDFA2 :: String -> MyDFA -> Maybe Bool
runNFA1, runNFA2 :: String -> MyFA -> Maybe Bool

-- #1: single state tracking with branching
--Data.List: lookup
--Data.Maybe: fromJust

		--If the inputs are acceptable,
runDFA1 str (validInputs, dfa) = if validInputString validInputs str && validDFA (validInputs, dfa)
		--call the helper on the dfa starting from the hardcoded start state and the beginning of the input string,
	then Just (runDFA1helper str 0)
		--otherwise return the error condition.
	else Nothing

	where
		--validDFA calls validFA which assures that target and start states are legal indices,
		--so state lookups will not error.
	runDFA1helper :: String -> Int -> Bool
		--If the input string is done, the current state indicates whether or not we should accept.
		--Its stored Bool is the DFA's result.
	runDFA1helper [] state = snd (dfa !! state)
		--Since the DFA is valid and the input is valid, the lookup will not error.
		--This uses the 'lookup' function which takes advantage of the structure of the data type.
		--There is no branching because this is a DFA.
	runDFA1helper (c:cs) state = runDFA1helper cs  $ fromJust (lookup (Just c) (fst (dfa !! state)))


--The NFA execution definitions are written to aid debugging.
--They are not intended for speed,
--but rather to allow isolation of errors in either the conversion code or execution of REs and DFAs.


--This NFA attempt uses branching execution.
--A further attempt will track the current set of states.
--This uses 'findEpsClosure' from the previous file.
--This version is susceptible to infinite loops.
		--If the inputs are acceptable,
runNFA1 str nfa = if validFA nfa
		--call the helper on the nfa starting from the hardcoded start state and the beginning of the input string,
	then Just (runNFA1helper str 0)
		--otherwise return the error condition.
	else Nothing

	where
		--validFA assures that target and start states are legal indices,
		--so state lookups will not error.
		--However, lookups on the characters may give any number of results,
		--and in additional there are epsilon arcs.
	runNFA1helper :: String -> Int -> Bool
		--If the input string is done, the current state indicates whether or not we should accept,
		--but there may be epsilon arcs.
		--If any stored Bool of the states in the epsilon closure is True, the NFA's result is True.
	runNFA1helper [] state = or $ map (snd . (nfa !!)) (findEpsClosure nfa [state])
		--When we are given a state and an input character,
		--the next state of the NFA can be any target state on an arc for that character
		--from any state in the epsilon closure of the given state.
		--If any of these branches result in success, the NFA is successful.
		--Therefore, we 'or' the results of those executions.
		--A lookup in a given state may give any number of matching arcs, zero or more.
		--Here we pass 'runNFA1helper' partially applied to 'cs' as a parameter to 'map'.
	runNFA1helper (c:cs) state = or (map (runNFA1helper cs) charTargets)
		where
			--We need the arc lists of states in the epsilon closure.
		epsStateLists = findEpsClosure nfa [state]
			--We need, for a state, the list of target states for arcs matching the input character.
		matchingArcs s = map snd (filter (\(mayC, _) -> Just c == mayC) (fst (nfa !! s)))
			--The total list is a concatenation of the lists of all matching arcs from eps-reachable states.
		charTargets = concat (map matchingArcs epsStateLists)


-- #2: tracking a set of current states
--No DFA version is needed.  There is only one state at a time in a DFA.
--This is why NFAs are usually converted to DFAs before running them on input.

runDFA2 = runDFA1


--The NFA starts with the start state's epsilon closure.
runNFA2 str nfa = if validFA nfa
		--call the helper on the nfa starting from the hardcoded start state and the beginning of the input string,
	then Just (runNFA2helper str [0])
		--otherwise return the error condition.
	else Nothing

	where
		--validFA assures that target and start states are legal indices,
		--so state lookups will not error.
		--However, lookups on the characters may give any number of results,
		--and in additional there are epsilon arcs.
	runNFA2helper :: String -> [Int] -> Bool
		--If the input string is done, the current state indicates whether or not we should accept,
		--but there may be epsilon arcs.
		--If any stored Bool of the states in the epsilon closure is True, the NFA's result is True.
	runNFA2helper [] states = or $ map (snd . (nfa !!)) (findEpsClosure nfa states)
		--When we are given a state and an input character,
		--the next state set of the NFA is the union of target states on arcs for that character
		--from any state in the epsilon closure of the given state.
		--If iteration on this set with the remaining inputs is successful, the NFA is successful.
		--A lookup in a given state may give any number of matching arcs, zero or more.
		--Here we pass 'runNFA2helper' partially applied to 'cs' as a parameter to 'map'.
	runNFA2helper (c:cs) states = runNFA2helper cs charTargets
		where
			--We need the arc lists of states in the epsilon closure of the set.
		epsStateLists = findEpsClosure nfa states
			--We need, for a state, the list of target states for arcs matching the input character.
		matchingArcs s = map snd (filter (\(mayC, _) -> Just c == mayC) (fst (nfa !! s)))
			--The total list is a concatenation of the lists of all matching arcs from eps-reachable states.
			--We don't bother finding uniques because this is only an input to findEpsClosure, which does.
		charTargets = concat (map matchingArcs epsStateLists)




{-#### Executing REs ####-}


--The versions of runRE do a pattern match on the next expression.
--Defining the behavior individually for each constructor makes the code very clear.
--Not much speed will be sacrificed, if any.
runRE1, runRE2 :: String -> MyRegExp -> Bool

-- #1: single state tracking with branching

runRE1 = runRE1help 20 20

--The helper also carries a current and max number of iterations with consuming a digit.
runRE1help :: Int -> Int -> String -> MyRegExp -> Bool

--When too many iterations have occured without consuming a digit, the match is considered to fail.
--This avoids the infinite recursions listed below.
--This match will never be reached if the max is a negative number.
runRE1help 0 _ _ _ = False

--A literal must be matched by an input literal.
--This consumes a digit, so the count is reset.
runRE1help n maxN (x:xs) (Literal c:exps) = (x == c) && runRE1help maxN maxN xs exps
runRE1help n maxN [] (Literal _:_) = False

--A wild card must be matched by some input literal, but it doesn't matter what.
--This consumes a digit, so the count is reset.
runRE1help n maxN (_:xs) (Wildcard:exps) = runRE1help maxN maxN xs exps
runRE1help n maxN [] (Wildcard:_) = False

--A MatchOne must be matched by an input literal from its list.
--This consumes a digit, so the count is reset.
runRE1help n maxN (x:xs) (MatchOne chars:exps) = (x `elem` chars) && runRE1help maxN maxN xs exps
runRE1help n maxN [] (MatchOne _:_) = False

--A MatchNone must be matched by an input literal not from its list.
--This consumes a digit, so the count is reset.
runRE1help n maxN (x:xs) (MatchNone chars:exps) = not (x `elem` chars) && runRE1help maxN maxN xs exps
runRE1help n maxN [] (MatchNone _:_) = False

--A star must be matched by some number of iterations of the contained sub item, then the remaining RE.
--To handle the case where their are no more copies of the star item,
--the string is matched against the remaining RE.
--To handle the cases where there are one or more copies of the star item,
--the string is matched against the sub item followed by the star item and the remaining RE.
--An empty input can only match zero copies of the star item,
--and this case is probably needed to prevent infinite recursion.
--In the first case, swapping the order of the two recursive steps causes double layered stars to go infinitely.
--I.e. if you swap the two recursions, "a**" will go forever on input "a".
--These do not consume a digit, so the count is decremented.
runRE1help n maxN (x:xs) (REGrpStar item:exps) =
	(runRE1help (n-1) maxN (x:xs) (item:REGrpStar item:exps)) || (runRE1help (n-1) maxN (x:xs) exps)
runRE1help n maxN [] (REGrpStar _:exps) = runRE1help (n-1) maxN [] exps

--An "or empty" must be matched by zero or one iterations of the contained sub item, then the remaining RE.
--This is very similar to the star case except that there is no recursion on the star item.
--To handle the case where their are no more copies of the sub item,
--the string is matched against the remaining RE.
--To handle the cases where there is one copy of the sub item,
--the string is matched against the sub item followed by the remaining RE.
--An empty input can only match zero copies of the sub item,
--but this case is probably not needed to prevent infinite recursion.
--These do not consume a digit, so the count is decremented.
runRE1help n maxN (x:xs) (REGrpOrEmpty item:exps) =
	(runRE1help (n-1) maxN (x:xs) exps) || (runRE1help (n-1) maxN (x:xs) (item:exps))
runRE1help n maxN [] (REGrpOrEmpty _:exps) = runRE1help (n-1) maxN [] exps

--A sub expression must be matched by the input, then the remaining RE must be matched.
--No case for zero-length input is needed.
--This does not consume a digit, so the count is decremented.
runRE1help n maxN theStr (REsubExp subRE:exps) = runRE1help (n-1) maxN theStr (subRE ++ exps)

--For an "or" expression, the first sub RE then the remaining RE must match the input,
--or the second sub RE then the remaining RE must match the input.
--No case for zero-length input is needed.
--Nothing using this design can prevent (a*|b*)* from recursing infinitely on at least "a" or "b".
--In this arrangement it goes infinitely on "b".
--These do not consume a digit, so the count is decremented.
runRE1help n maxN theStr (REGrpOr subRE1 subRE2:exps) =
	(runRE1help (n-1) maxN theStr (subRE1 ++ exps)) || (runRE1help (n-1) maxN theStr (subRE2 ++ exps))

--If there is some data left in the input and there are no items left in the RE,
--then the string did not match the RE.
runRE1help _ _ (_:_) [] = False

--If there is no data left in the input and there are no items left in the RE,
--then the string matched the RE.
runRE1help _ _ [] [] = True


-- #2: tracking a set of current states

--This version of RE execution operates on the possible states simultaneously.
--Unlike FAs, the potential states are not easily categorized by the amount of input consumed.
--Therefore, each state includes the remaining input not consumed already by that progression.
runRE2 theStr exps = runRE2help [(20, 20, theStr, exps)]

--The helper maps a conversion onto the list of states and then accumulates the results.
--This function must check for success, to avoid awkwardness of returning it from the conversion.
--Success occurs when a state reaches [] for input and [] for the remaining RE.
--Total failure occurs when there is no state left.
--The function is written to take advantage of the short-circuit implementation of || and &&.
--Without that short-circuit behavior, this could be infinitely recursive or simply slow to finish.
runRE2help :: [(Int, Int, String, MyRegExp)] -> Bool
	--The result is True if any state has reached success,
runRE2help states = (filter (\(_, _, xs, ys) -> xs == [] && ys == []) states /= []) ||
	--otherwise, the result is false if there are no more states,
	(	(states /= []) &&
	--and when there are states, we iterate.
		runRE2help (concat (map (uncurry4 runRE2convert) states))  )


--Instead of rewriting a lot of code so that runRE2convert can operate on tuples,
--we write runRE2convert as runRE1help, then write a custom uncurrying function.
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d


--The conversion function returns the possible states from the given states.
--It is similar to runRE1help, but [] is returned when no state is allowed instead of False.
--The same infinite recursion is possible as above, so the same control is needed.
--An interesting point about this function is that it could be converted into runRE1help with a simple wrapper.
--The only addition would be noticing success.
runRE2convert :: Int -> Int -> String -> MyRegExp -> [(Int, Int, String, MyRegExp)]

--When too many iterations have occured without consuming a digit, the match is considered to fail.
--This avoids the infinite recursions listed below.
--This match will never be reached if the max is a negative number.
runRE2convert 0 _ _ _ = []

--A literal must be matched by an input literal.
--This consumes a digit, so the count is reset.
runRE2convert n maxN (x:xs) (Literal c:exps) = if (x == c) then [(maxN, maxN, xs, exps)] else []
runRE2convert n maxN [] (Literal _:_) = []

--A wild card must be matched by some input literal, but it doesn't matter what.
--This consumes a digit, so the count is reset.
runRE2convert n maxN (_:xs) (Wildcard:exps) = [(maxN, maxN, xs, exps)]
runRE2convert n maxN [] (Wildcard:_) = []

--A MatchOne must be matched by an input literal from its list.
--This consumes a digit, so the count is reset.
runRE2convert n maxN (x:xs) (MatchOne chars:exps) = if (x `elem` chars) then [(maxN, maxN, xs, exps)] else []
runRE2convert n maxN [] (MatchOne _:_) = []

--A MatchNone must be matched by an input literal not from its list.
--This consumes a digit, so the count is reset.
runRE2convert n maxN (x:xs) (MatchNone chars:exps) = if not (x `elem` chars) then [(maxN, maxN, xs, exps)] else []
runRE2convert n maxN [] (MatchNone _:_) = []

--A star must be matched by some number of iterations of the contained sub item, then the remaining RE.
--To handle the case where their are no more copies of the star item,
--the string is matched against the remaining RE.
--To handle the cases where there are one or more copies of the star item,
--the string is matched against the sub item followed by the star item and the remaining RE.
--An empty input can only match zero copies of the star item,
--and this case is probably needed to prevent infinite recursion.
--In the first case, swapping the order of the two recursive steps causes double layered stars to go infinitely.
--I.e. if you swap the two recursions, "a**" will go forever on input "a".
--These do not consume a digit, so the count is decremented.
runRE2convert n maxN (x:xs) (REGrpStar item:exps) =
	[( (n-1), maxN, (x:xs), (item:REGrpStar item:exps) ), ( (n-1), maxN, (x:xs), exps )]
runRE2convert n maxN [] (REGrpStar _:exps) = [( (n-1), maxN, [], exps )]

--An "or empty" must be matched by zero or one iterations of the contained sub item, then the remaining RE.
--This is very similar to the star case except that there is no recursion on the star item.
--To handle the case where their are no more copies of the sub item,
--the string is matched against the remaining RE.
--To handle the cases where there is one copy of the sub item,
--the string is matched against the sub item followed by the remaining RE.
--An empty input can only match zero copies of the sub item,
--but this case is probably not needed to prevent infinite recursion.
--These do not consume a digit, so the count is decremented.
runRE2convert n maxN (x:xs) (REGrpOrEmpty item:exps) =
	[( (n-1), maxN, (x:xs), exps ), ( (n-1), maxN, (x:xs), (item:exps) )]
runRE2convert n maxN [] (REGrpOrEmpty _:exps) = [( (n-1), maxN, [], exps )]

--A sub expression must be matched by the input, then the remaining RE must be matched.
--No case for zero-length input is needed.
--This does not consume a digit, so the count is decremented.
runRE2convert n maxN theStr (REsubExp subRE:exps) = [( (n-1), maxN, theStr, (subRE ++ exps) )]

--For an "or" expression, the first sub RE then the remaining RE must match the input,
--or the second sub RE then the remaining RE must match the input.
--No case for zero-length input is needed.
--Nothing using this design can prevent (a*|b*)* from recursing infinitely on at least "a" or "b".
--In this arrangement it goes infinitely on "b".
--These do not consume a digit, so the count is decremented.
runRE2convert n maxN theStr (REGrpOr subRE1 subRE2:exps) =
	[( (n-1), maxN, theStr, (subRE1 ++ exps) ), ( (n-1), maxN, theStr, (subRE2 ++ exps) )]

--If there is some data left in the input and there are no items left in the RE,
--then the string did not match the RE.
runRE2convert _ _ (_:_) [] = []

--If there is no data left in the input and there are no items left in the RE,
--then runRE2help will detect that success has occured, so this pattern is unnecessary.
--runRE2convert [] [] = True

