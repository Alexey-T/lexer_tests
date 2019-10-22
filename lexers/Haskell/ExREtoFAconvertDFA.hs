--Eric Etheridge
--2009 05 18
--ExREtoFAconvertDFA.hs

--stage 3: NFA to DFA (1 version)
--This is a functional implementation of the normally highly imperative description for converting an NFA to a DFA.
--There are only two complex functions, the set-to-state recursive conversion and the check for adding a trash state.
--Normally I would write this much more concisely, but writing the intentions of each action made the code clear.


module ExREtoFAconvertDFA (convertNFAtoDFA) where


--import Debug.Trace
import ExREtoFAtypes
import Data.List ((\\), sort, nub, elemIndex, sortBy, groupBy)
import Data.Maybe (fromJust)


{-#### Conversion from NFA to DFA ####-}

{-
Imperative description:
1. create start node w/ label of NFA start state + epsilon closure - it accepts if any states in the set accept
2. get list of all non-epsilon arcs from the states in that set, group by character
3. for each character, identify the set of end states + epsilon closure of arcs from start set
4. if that set is not yet used as a label, add it - it accepts if any states in the set accept
5. add an arc from the start state to the new state on the given Char
6. if there are not arcs for all valid inputs, create trash state if necessary and arc on remaining inputs
7. repeat on any newly added state
-}

{-
Functional modifications:
1. pass forward a [ [ Int ] ] to indicate a set of NFA states represented by each 0-indexed state
2. pass forward a MyFA to represent the constucted DFA
3. a final pass determines what would be the state number of potential trash state and adds it and arcs to it if needed
-}

convertNFAtoDFA :: MyFAInputs -> MyFA -> MyDFA
convertNFAtoDFA cs [] = (cs, [])
convertNFAtoDFA validInputs nfa = completedDFA
	where
		--First we need the epsilon closure from the start state.
		--We sort this and remove duplicates so that comparisons are easy later.
	startEps = findEpsClosure nfa [0]
		--Second, we perform the lengthy conversion process starting with the start state.
	convertedFA = convertNDhelp nfa [] [startEps]
		--Third, we add a trash state if needed.
	completedDFA = maybeAddTrashStateAndArcs validInputs convertedFA


--from Data.List import: sortBy, groupBy, \\, nub, ?sort
--Technically, this function could be included as a secondary function in the above definition, but it's very large.
--The only reasons for including it would be to avoid passing the nfa forward and to prevent outside calls,
--but neither of those are very compelling reasons.
--This function creates one new state's record in the DFA list if necessary.
--When there are no new states to create, this function returns the constructed DFA.
--It could be modified to trace or return the list of sets of NFA states for testing/debugging purposes.
convertNDhelp :: MyFA -> MyFA -> [ [ Int ] ] -> MyFA
convertNDhelp nfa currentDFA dfaStateSets =
		--Termination occurs when their are no new sets of NFA states to convert to DFA states.
--	trace ("top convertNDhelp, currentDFA: " ++ show currentDFA) $
--	trace ("top convertNDhelp, dfaStateSets: " ++ show dfaStateSets) $
	if numDFAstates == numDFAsets then currentDFA else 
		--Otherwise, the definition is a recursive call with a new state and possibly more state sets.
	convertNDhelp nfa (currentDFA ++ [newState]) updatedDFAstateSets

	where
		--These are recalculated each time, wasting a little effort but making the code clearer.
		--They are used to determine when the function should return.
	numDFAstates = length currentDFA
	numDFAsets = length dfaStateSets

		--The next state to create is the first set beyond the current DFA.
	newStateNum = numDFAstates

		--We need the set of NFA states which the new DFA state will represent.
	newStateSet = dfaStateSets !! newStateNum

		--Our final goal for the new state is the following definition.
		--The values of newStateArcs and newStateAccept are defined below.
	newState = (newStateArcs, newStateAccept)

	--Calculating newStateAccept:
		--A DFA state must accept if it represents any NFA states which accept.
		--The following line is complicated.
		--It uses the ($), (.), and (!!) functions as well as 'or', 'map', and 'snd'.
		--The end calculation is T/F of whether any state in 'newStateSet' has 'True' as its second value.
	newStateAccept = or $ map (snd . (nfa !!)) newStateSet	--if any reachable NFA state accepts, the DFA state accepts

	--Calculating newStateSetArcs, step by step:
		--We need the arcs from all the states in the NFA set.
	nfaArcsWithEps = (concat (map (fst. (nfa !!)) newStateSet))
		--Of these, we need only the arcs which are not eps transitions from the source.
		--These are included already.
	nfaArcs = filter (\(x,_) -> x /= Nothing) nfaArcsWithEps
		--We need to separate the arcs for each input.
		--This may give us repeated target sets for multiple inputs and/or repeated state destinations.
	nfaCharTargets = groupBy (\(Just x, _) (Just y, _) -> x == y) $
			sortBy  (\(Just x, _) (Just y, _) -> x `compare` y) nfaArcs
		--Then we need to identify the input and target set for each group of arcs.
		--From these, we need to add all the epsilon closures from the destinations.
		--Again, we sort and remove duplicates from each list of target nfa states so that comparisons are easy.
		--Side note: I think this is the first time I've used an '@' pattern in a lambda function.
	nfaTargetSets = map (\arcs@((Just c, _):_) -> (Just c, findEpsClosure nfa $ map snd arcs)) nfaCharTargets
		--Now we have the target set for each character, which we use to make a larger DFA list of sets.
		--We use 'nub' to avoid adding the same set twice.
		--The sets in the current state sets are not new, so we do not include them.
		--The result may be the empty list.
	newDFAsets = (nub $ map snd nfaTargetSets) \\ dfaStateSets
		--Here we create the updated list of DFA states based on the new sets.
	updatedDFAstateSets = dfaStateSets ++ newDFAsets
		--Then we determine the DFA state to which each character arcs.
		--(In some ways, this is a recalculation, but it avoids awkward attempts to track the above calculation.)
		--We need to find the position of each set in the updated list, but 'elemIndex' isn't written in that order.
		--We use 'flip' to make 'elemIndex' and a target list work in a mapping.
	destStates = map (fromJust . flip elemIndex updatedDFAstateSets . snd) nfaTargetSets
		--Finally, we can define the arcs from the new state.
		--This does not include arcs to the trash state if any will be needed.
	newStateArcs = zip (map fst nfaTargetSets) destStates


--This takes the constructed FA and the list of valid characters.
--If any existing state is missing an arc on a valid input, a new trash state is needed.
--All states with arcs missing for valid inputs need arcs added on those to the trash state.
--Since the result is a correct DFA, we return that.
maybeAddTrashStateAndArcs :: MyFAInputs -> MyFA -> MyDFA
maybeAddTrashStateAndArcs validInputs dfaNoTrash =
--	trace ("top maybeAddTrashStateAndArcs, dfaNoTrash: " ++ show dfaNoTrash) $
	if not trashStateNeeded then (validInputs, dfaNoTrash)
		else (validInputs, dfaWithTrashArcs ++ [trashState])
	where
		--Find unique inputs (probably unnecessary) and sort them to make comparison easy.
	compInputs = sort $ nub validInputs
		--We are ignoring the idea of arcs listed for invalid inputs, so inequality implies missing inputs.
		--This requires sorting the characters in the arc lists.  We assume uniques in the DFA.
	trashStateNeeded = or $ map ((/= compInputs) . sort . map (fromJust . fst) . fst) dfaNoTrash

	--The remaining computations will only occur if a trash state is needed.
		--We need to know what state number the trash state will be.
	trashStateNumber = length dfaNoTrash
		--We need the trash state to loop on all valid inputs.
	trashStateArcs = map (\c -> (Just c, trashStateNumber)) validInputs
		--The trash state does not accept.
	trashState = (trashStateArcs, False)
		--The original dfa states need an arc to the trash state for all valid inputs.
	dfaWithTrashArcs = map addTrashArcs dfaNoTrash
		--We add the arcs to trash to each state.
		--This function is almost short enough to be a lambda expression, but this is clearer.
	addTrashArcs (arcs, acceptance) = (arcs ++ theseTrashArcs, acceptance)
		where
			--There must be a trash arc for any valid input for which no arc exists.
		unlistedInputs = compInputs \\ map (fromJust . fst) arcs
		theseTrashArcs = map (\c -> (Just c, trashStateNumber)) unlistedInputs

