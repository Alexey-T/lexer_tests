--Eric Etheridge
--2009 05 18
--ExREtoFAconvertNFA.hs

--stage 2: RE to NFA (1 version completed)
--The first version below, as indicated, is incomplete.
--The second version requires knowledge of the valid characters but is much more streamlined.


module ExREtoFAconvertNFA (convertREtoNFA) where


--import Debug.Trace
import ExREtoFAtypes
import Data.List ((\\))


{-#### Conversion from Regular Expression to NFA ####-}

--The simple principle for conversion is that each item in a sequence must be matched.


convertREtoNFA = convertREtoNFAv2

--UNFINISHED AND IMPROPERLY DESIGNED, DO NOT USE THIS VERSION
--LEFT AS AN EXAMPLE
convertREtoNFAv1 :: MyFAInputs -> MyRegExp -> MyFA
	--The initial version of the start state has no arcs and does not accept.
convertREtoNFAv1 validChars re = convertRNhelpv1 validChars re [( [], False )]

--This is written as a fairly imperative style.
--A good first try for code design, but 
convertRNhelpv1 :: MyFAInputs -> MyRegExp -> MyFA -> MyFA
convertRNhelpv1 validChars ((Literal c):es) nfa = convertRNhelpv1 validChars es $
		addArcs (newState - 1) newState [Just c] $ addState newState nfa
	where
	newState = length nfa
convertRNhelpv1 validChars (Wildcard:es) nfa = convertRNhelpv1 validChars es $
		addArcs (newState - 1) newState (map Just validChars) $ addState newState nfa
	where
	newState = length nfa
--	and so on... but we don't bother completing this imperative styled function
convertRNhelpv1 validChars (e:es) nfa
	| otherwise = error "stub, use convertREtoNFAv2"
--convertRNhelpv1 [] nfa = --The last state would need to be altered at this point so that it accepts.

--The fact that this function ignores a parameter completely is a hint to the other problems with this design.
addState :: Int -> MyFA -> MyFA
addState _ fa = fa ++ [ ([], False) ]

--This could be rewritten as a fold, but that wouldn't get rid of the imperative nature of 'update'.
addArcs :: Int -> Int -> [Maybe Char] -> MyFA -> MyFA
addArcs orig dest (c:cs) fa = addArcs orig dest cs (updateFA orig dest c fa)
addArcs _ _ [] fa = fa

--This is extremely imperative and should be done better.
--If you ever find yourself writing a list update function in Haskell,
--you're probably doing something wrong.
--This function and the above version are left as an example of undesirable Haskell coding.
updateFA :: Int -> Int -> Maybe Char -> MyFA -> MyFA
updateFA orig dest c fa = h ++ [((c, dest):arcs, acceptance)] ++ tail t
	where
	(h, t) = splitAt orig fa
	(arcs, acceptance) = head t



--Data.List: \\
--This is a much more recursive, functional definition of the conversion.
--We need to know the allowed characters.
convertREtoNFAv2 :: MyFAInputs -> MyRegExp -> MyFA
convertREtoNFAv2 validChars = convertRNhelpv2 [] 0

	where
	--This function is a local function to avoid passing 'validChars' repeatedly.
	--To more easily handle loops, we pass an arc list which is used at the last state in a sequence.
	--We also need an Int to know the number of the state to create.
	convertRNhelpv2 :: [ (Maybe Char, Int) ] -> Int -> MyRegExp -> MyFA

	--An input creates a state which does not accept and has an arc to its next state.
		--A parameter-carrying version that would be tail recursive might increase efficiency.
	convertRNhelpv2 finalArcs newStateNum (exp:exps) =
			theNewStates ++
			(convertRNhelpv2 finalArcs (newStateNum + length theNewStates) exps)
		where
		theNewStates = newStates exp

		--A literal creates a non-accepting state with a single arc to the next state.
		newStates (Literal c) =
			[ ([(Just c, newStateNum + 1)], False) ]

		--A "wildcard" creates a non-accepting state with arcs to the next state on all valid inputs.
		newStates Wildcard =
			[ (map (\c -> (Just c, newStateNum + 1)) validChars, False) ]

		--A MatchOne creates a non-accepting state with arcs to the next state on all given chars.
		newStates (MatchOne cs) =
			[ (map (\c -> (Just c, newStateNum + 1)) cs, False) ]

		--A MatchNone creates a non-accepting state with arcs to the next state on all valid chars not given.
		--THIS ASSUMES THAT 'validChars' HAS DISTINCT ENTRIES.
		newStates (MatchNone cs) =
			[ (map (\c -> (Just c, newStateNum + 1)) (validChars \\ cs), False) ]

		--A "star" expression contains a sub item.
		--A start state is made to control access to the item.
		--The FA for the sub item is calculated, starting at the next state number + 1.
		--The start either moves by epsilon into the sub FA or jumps by epsilon past the sub FA.
		--That sub FA terminates by an epsilon transition to the start state.
		--The new states added are the start state and the sub FA.
		newStates (REGrpStar item) =
			([(Nothing, newStateNum + 1), (Nothing, newStateNum + 1 + length subFA)], False) :
			subFA
			where
			subFA = convertRNhelpv2 [(Nothing, newStateNum)] (newStateNum+1) [item]

		--An "or empty" expression contains a sub item.
		--A start state is made to control access to the item.
		--An end state is used to exit the "or else" section.
		--The FA for the sub item is calculated, starting at the next state number + 2.
		--The start either moves by epsilon into the sub FA or moves by epsilon to the end state.
		--The end state jumps by epsilon past the sub FA.
		--That sub FA terminates by an epsilon transition to the end state.
		--The new states added are the start state, end state, and the sub FA.
		newStates (REGrpOrEmpty item) =
			([(Nothing, newStateNum + 2), (Nothing, newStateNum + 1)], False) : 
			([(Nothing, newStateNum + 2 + length subFA)], False) :
			subFA
			where
			subFA = convertRNhelpv2 [(Nothing, newStateNum + 1)] (newStateNum+2) [item]

		--A sub expression contains an RE on its own.
		--A start state is made to control access to the RE.
		--An end state is used to exit the expression.
		--The FA for the sub item is calculated, starting at the next state number + 2.
		--The start either moves by epsilon into the sub FA.
		--The end state jumps by epsilon past the sub FA.
		--That sub FA terminates by an epsilon transition to the end state.
		--The new states added are the start state, end state, and the sub FA.
		newStates (REsubExp re) = 
			([(Nothing, newStateNum + 2)], False) : 
			([(Nothing, newStateNum + 2 + length subFA)], False) :
			subFA
			where
			subFA = convertRNhelpv2 [(Nothing, newStateNum + 1)] (newStateNum+2) re

		--An "or re1 re2" expression acts like an "or empty" expression above.
		--A start state is made to control access to the REs.
		--An end state is used to exit the "or".
		--The FA for the first RE is calculated, starting at the next state number + 2.
		--The FA for the second RE is calculated, starting after first FA ends.
		--The start either moves by epsilon into the first FA or moves by epsilon to the second FA.
		--The end state jumps by epsilon past both FAs.
		--Both sub FAs terminate by an epsilon transition to the end state.
		--The new states added are the start state, end state, and both FAs.
		newStates (REGrpOr re1 re2) =
			([(Nothing, newStateNum + 2), (Nothing, newStateNum + 2 + length subFA1)], False) : 
			([(Nothing, newStateNum + 2 + length subFA1 + length subFA2)], False) :
			subFA1
			++ subFA2
			where
			subFA1 = convertRNhelpv2 [(Nothing, newStateNum + 1)] (newStateNum+2) re1
			subFA2 = convertRNhelpv2 [(Nothing, newStateNum + 1)] (newStateNum+2+length subFA1) re2

		--The end of the inputs creates a state which accepts if it has no back arcs.
		--It uses the passed forward final arcs as its arcs.
	convertRNhelpv2 finalArcs _ [] = [( finalArcs, finalArcs == [] )]

