--Eric Etheridge
--2009 05 18
--Example2CNF.hs

--solving 2CNF, also called 2SAT
--Usage:
--This file can be used in GHCi or can be compiled with GHC and run.

--test at a prompt:
--cnf2Solver1 cnfA
--examples cnfA through cnfF are provided

--Most information taken from this page:
--http://en.wikipedia.org/wiki/2SAT



import Data.Maybe (Maybe(..), fromJust)


--I use a 'data' definition for the type mostly to show it as an example.
--Usually, only larger types, or those which would not always need all members,
--would be written as 'data' types.
--Here the clauses use a bool for each given variable number.
-- 'False' indicates that the variable is negated in the clause.
--Variables are in the range 0 to 'maxVar' inclusive.
type VarCNF = (Bool, Int)
data CNF2 = CNF2 { maxVar :: Int, clauses :: [( VarCNF, VarCNF )] }

--The other reason to use 'data' types is to distinguish between members of identical type.
--The classic example is dates, stored by three integers:
--type DayOfYear = (Int, Int, Int)
--Is it (day, month, year), as in the UK?
--Is it (month, day, year), as in the US?
--Is it (year, month, day), to make sorting easy?
--There is no way to be sure from the type signature, so a data constructor is used.
--data DayOfYear = DOY { day, month, year :: Int }


-- #### Clauses to test and Main Function ####

	--	(A or not B) and (not A or B) --> A, B both false or True
clauseA = [((True, 0), (False, 1)), ((False, 0), (True, 1))]
cnfA = (CNF2 {maxVar = 1, clauses = clauseA})

	--	(A or not A) --> A can be false or true
clauseB = [((True, 0), (False, 0))]
cnfB = (CNF2 {maxVar = 0, clauses = clauseB})

	--	(A or A) --> A must true
clauseC = [((True, 0), (True, 0))]
cnfC = (CNF2 {maxVar = 0, clauses = clauseC})

	--	(A or A) and (not A or not A) --> no solution
clauseD = [((True, 0), (True, 0)), ((False, 0), (False, 0))]
cnfD = (CNF2 {maxVar = 0, clauses = clauseD})

	--	(A or not B) and (not A or B) and (not A or not B) --> A, B both false
clauseE = [((True, 0), (False, 1)), ((False, 0), (True, 1)), ((False, 0), (False, 1))]
cnfE = (CNF2 {maxVar = 1, clauses = clauseE})

	--	(A or not B) and (not B or C) and (A or not C) and (not A or not C) --> A true, B, C both false or all false
clauseF = [((True, 0), (False, 1)), ((False, 1), (True, 2)), ((False, 0), (False, 2)), ((False, 0), (False, 2))]
cnfF = (CNF2 {maxVar = 2, clauses = clauseF})


main = do
	putStrLn $ show $ cnf2Solver1 cnfA
	putStrLn $ show $ cnf2Solver1 cnfB
	putStrLn $ show $ cnf2Solver1 cnfC
	putStrLn $ show $ cnf2Solver1 cnfD
	putStrLn $ show $ cnf2Solver1 cnfE
	putStrLn $ show $ cnf2Solver1 cnfF
	--try your own clauses



-- #### Solution Versions ####

--The solution returns Nothing if no solution is possible,
--or Just a list of the True variables in a valid solution.
--This solution method finds all strongly connected components of the implication graph.
--If and only if none contain a variable and its negation, then a solution exists.
--A solution is formed by following the implications in the graph.
--The different implementations use varying data types to store the implication graph.
--Returns Just a valid variable assignment in order, or Nothing.
cnf2Solver1 :: CNF2 -> Maybe [Bool]



-- #### Version #1: Graph is a Doubly Nested List ####

type AssignedList1 = [Maybe Bool]	--assigned to True or False or not yet assigned
type ImpGraph1 = [( [VarCNF], [VarCNF] )]	--the assignments implied by (False, True) for a variable

cnf2Solver1 (CNF2 {maxVar = m, clauses = cs}) =
		--Only start recursion with valid initial data.
	if m < 0 then Nothing else cnfRecurse1 [Nothing | x <- [0..m]]
	where
	--This is variable in scope for this function so that it is computed only once.
	--It stores the list of assignments cnfs implied by the assignment of True and False for each variable.
	--This uses a complicated set of nested list comprehensions but its intention should be clear from the definition.
	impGraph1 :: ImpGraph1
	impGraph1 = map (\[fs, ts] -> (fs, ts)) $ [ [	--convert the two lists into a tuple for (false, true) for that variable i
		[ bVar | ((tfA, a), bVar) <- cs, tfA /= tf && a == i ] ++	--get the list of right-direction implications for that tf and i
		[ aVar | (aVar, (tfB, b)) <- cs, tfB /= tf && b == i ]	--add it to the list of left-direction implications for that tf and i
			| tf <- [False, True] ] | i <- [0..m] ]	--create false and true lists for each variable, calling them tf and i

	--This returns a list of assignment cnfs implied by a given assignment.
	--It uses impGraph1 as a global variable (in this scope).
	--That means that the graph doesn't need to be passed recursively.
	impliedByCNF :: VarCNF -> [VarCNF]
	impliedByCNF (tf, x) = (if tf then snd else fst) (impGraph1 !! x)

	--This updates a list of assigned variables with an assignment cnf.
	updateWithCNF :: VarCNF -> AssignedList1 -> AssignedList1
	updateWithCNF (tf, x) assigned = bef ++ (Just tf : tail aft)
		where
		(bef, aft) = splitAt x assigned

	--This function updates a list of assignments with all the transitive implications of a choice.
	setImplicationsAssuming1 :: VarCNF -> AssignedList1 -> Maybe AssignedList1
	setImplicationsAssuming1 (origTF, origX) = setHelper [(origTF, origX)]
		where
		--This function updates the list of assignments with the implications of a choice and recurses on new implications.
		setHelper :: [VarCNF] -> AssignedList1 -> Maybe AssignedList1
		setHelper (cnf@(tf,x):more) assigned =
				if lookup == Nothing	--if the variable is unassigned, assign it and add its implications to the todo-list
				then setHelper (more ++ impliedByCNF cnf) (updateWithCNF cnf assigned) 
				else if tf /= fromJust lookup		--if the variable is already assigned, check for a contradiction
				then Nothing		--if there is a contradiction, return Nothing
				else setHelper more assigned		--otherwise, don't redo that assignment
			where lookup = assigned !! x
		setHelper [] assigned = Just assigned

	--This recursively tries each way of assigning open variables.
	cnfRecurse1 :: AssignedList1 -> Maybe [Bool]
	cnfRecurse1 assigned =
			--If no more values are left unassigned, we are left with a good solution
		if firstUnassigned == [] then Just (map fromJust assigned)
			--else if picking the first assigned as True gives us a partial solution, we recurse with it only
		else if setChoiceTrue /= Nothing then cnfRecurse1 (fromJust setChoiceTrue)
			--else if picking the first assigned as False gives us a partial solution, we recurse with it only
		else if setChoiceFalse /= Nothing then cnfRecurse1 (fromJust setChoiceFalse)
			--otherwise no solution is possible
		else Nothing
		where
			--Get [(Nothing, variable number)] concerning the first unassigned variable
		firstUnassigned :: [(Maybe Bool, Int)]
		firstUnassigned = take 1 $ dropWhile ((/= Nothing) . fst) (zip assigned [0..])
			--Assuming there is a variable unassigned, we will get that number
		unassignedChoice = snd (head firstUnassigned)	--this errors if evaluated inappropriately
			--with that number, we will assign everything transitively implied by assuming that variable is True
		setChoiceTrue = setImplicationsAssuming1 (True, unassignedChoice) assigned
			--with that number, we will assign everything transitively implied by assuming that variable is False
		setChoiceFalse = setImplicationsAssuming1 (False, unassignedChoice) assigned



--There are other ways to represent the implication graph and scan it.
--Here are some suggested versions.
--Their implementations are left as exercises to the reader.
--Each would need to change the implementation of setImplicationsAssuming#,
--but that would require changes to the following:
	--the type ImpGraph#
	--definition of impGraph# (its creation)
	--the helper function impliedByCNF
	--the helper function updateWithCNF


-- #### Version #2: No Graph, Scan Clauses as Needed ####


-- #### Version #3: Graph is a Doubly Nested Array ####


-- #### Version #4: Graph is an Array of Lists ####


-- #### Version #5: Graph is an Array of IntSets from Data.IntSet ####


-- #### Version #6: Graph Uses Builtin Data.Graph Functions and Types ####


