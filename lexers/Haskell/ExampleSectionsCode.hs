--Eric Etheridge
--2009 05 18
--ExampleSectionsCode.hs

--This file contains the simple examples from the first six sections that need to be compiled to be used because they can't be typed at a prompt.
--This file can be loaded at an interpretive prompt or included by another source file.
--A few of the functions in this file are commented out with the intention that someone using the tutorial will uncomment them as needed.

--There is a lot of code in this file not referred to by examples in ExampleSectionsTry.txt.


-- ## section1.html

--Calculating a fibonnaci number:
fib :: Int -> Int
fib n = fibGen 0 1 n
	
fibGen :: Int -> Int -> Int -> Int
fibGen a b n = case n of
	0 -> a
	n -> fibGen b (a + b) (n - 1)



--Calculating the fibonnaci sequence:
fibs :: [Int]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]



-- ## section2.html


--Taking a class member as a parameter:
doubleIt :: Num a => a -> a
doubleIt n = n + n


--The fibonacci sequence again, but polymorphic:
fibPoly :: (Num a, Num b) => a -> b
fibPoly n = fibGenPoly 0 1 n

fibGenPoly :: (Num a, Num b) => b -> b -> a -> b
fibGenPoly a b n = case n of
	0 -> a
	n -> fibGenPoly b (a + b) (n - 1)


--The map function example:
fooList :: [Int]
fooList = [3, 1, 5, 4]
	
bar :: Int -> Int
bar n = n - 2


--Partial application of map example:
subEachFromTen :: [Int] -> [Int]
subEachFromTen = map (10 -)



-- ## section3.html

--Recursive function example:
sumAll :: Num a => [a] -> a
sumAll (x:xs) = x + sumAll xs
sumAll [] = 0


--The fibonacci sequence again, but using pattern matching:
fibPat :: (Num a, Num b) => a -> b
fibPat n = fibGenPat 0 1 n
	
fibGenPat :: (Num a, Num b) => b -> b -> a -> b
fibGenPat a _ 0 = a
fibGenPat a b n = fibGenPat b (a + b) (n - 1)


--An example to show a time, using guards:
showTime :: Int -> Int -> String
showTime hours minutes
	| hours == 0	= "12" ++ ":" ++ showMin ++ " am"
	| hours <= 11	= (show hours) ++ ":" ++ showMin ++ " am"
	| hours == 12	= (show hours) ++ ":" ++ showMin ++ " pm"
	| otherwise 	= (show (hours - 12)) ++ ":" ++ showMin ++ " pm"
	where
	showMin
		| minutes < 10	= "0" ++ show minutes
		| otherwise		= show minutes


--A simple if-then-else example, showing a message about a grade:
showMsg :: Int -> String
showMsg n = if n < 70 then "failing" else "passing"


--Another if-then-else example, showing a message about list length:
showLen :: [a] -> String
showLen lst = (show (theLen)) ++ (if theLen == 1 then " item" else " items")
	where
	theLen = length lst


--Example simple main programs, uncomment to use, then compile and run, or load in GHCi or Hugs and type 'main':
--main = return ()
--main = putStrLn "Hello World"


--Polymorphic type example, using Maybe and showing a pet's name:
showPet :: Maybe (String, Int, String) -> String
showPet Nothing		= "none"
showPet (Just (name, age, species))	= "a " ++ species ++ " named " ++ name ++ ", aged " ++ (show age)


--The general frame listed for working with GHC is given in a separate example file, since it is not complete code.


--IO example to show the use of 'return':
getName1 :: IO String
getName1 = do
	putStr "Please enter your name: "
	name <- getLine
	putStrLn "Thank you.  Please wait."
	return name


--IO example to show the use of return a value from a monad function:
getName2 :: IO String
getName2 = do
	putStr "Please enter your name: "
	getLine



-- ## section4.html

--The following is the set of functions used to calculate physics movement, written in several ways.
--Each section is a block comment with all the original code, unmodified.
--You can test each as you like by uncommenting and loading or compiling as normal.


--Physics code, types section:

--types for variables
type Mass = Double			--only a type rename, but useful for clarifying parameters
type Pos = (Double, Double, Double)	--x, y, z
type Obj = (Mass, Pos)			--interchangeable with (Double, (Double, Double, Double))

--list of functions needed
{-
Takes a list of objects.
Returns a list of (sum of mass times other object mass over distance for all objects)
Order is preserved.
-}

--overall function type (copied below)
--calcMassesOverDists :: [Obj] -> [Double]


--Physics code, example data section:

objs1 = [(1.0, (0.0, 0.0, 0.0)), (1.0, (1.0, 0.0, 0.0))]
objs2 = [(4.5, (0.0, 0.0, 0.0)), (11.0, (1.0, 0.0, 0.0)), (2.25, (0.0, 3.0, 3.0)), (7.5, (3.0, 1.0, 0.0))]
objs3 = [(45.0, (0.0, 0.0, 0.0)), (110.0, (1.0, 0.0, 0.0)), (22.5, (0.0, 3.0, 3.0)), (75.0, (3.0, 1.0, 0.0))]


--Physics code, version 1:
{-
--Here we pass the objects in as two parameters to the helper function so we can iterate twice.
--This does not copy the list of objects.
calcMassesOverDists :: [Obj] -> [Double]
calcMassesOverDists objs = calcHelper objs objs

--This is a function that computes a distance between two Pos values, used in calcMMoD.
distXYZ :: Pos -> Pos -> Double
distXYZ (x1, y1, z1) (x2, y2, z2) = sqrt (xd * xd + yd * yd + zd * zd)
	where
	(xd, yd, zd) = (x1 - x2, y1 - y2, z1 - z2)	--three assignments at once using a tuple

--This iterates over the list of objects and calculates the sum for each.
--It uses pattern matching to recurse and terminate.
calcHelper :: [Obj] -> [Obj] -> [Double]
calcHelper (obj:objs) objList	= (sum (calcMMoD obj objList)) : calcHelper objs objList
calcHelper [] _	= []

--This calculates the list of mass times mass over distance for a single object.
--It uses pattern matching to recurse and terminate and a where clause to keep the code clear.
calcMMoD :: Obj -> [Obj] -> [Double]
calcMMoD obj@(m1, pos1) ((m2, pos2):rest)	= safeValue : calcMMoD obj rest
	where
	dist = distXYZ pos1 pos2
	safeValue = if pos1 == pos2 then 0 else m1 * m2 / dist
calcMMoD _ [] = []
-}


--Physics code, version 2:
{-
--Here we use 'map' instead of writing the recursion out.
calcMassesOverDists :: [Obj] -> [Double]
calcMassesOverDists objList = map (\obj -> sum (calcMMoD obj objList)) objList

--Again, we use 'map' instead of writing the recursion out.
calcMMoD :: Obj -> [Obj] -> [Double]
calcMMoD obj objList = map (mMoDHelper obj) objList

--Here we don't bother spacing out the code since we're not forming a list any more.
--Note that this function no longer operates or returns a list.
mMoDHelper :: Obj -> Obj -> Double
mMoDHelper (m1, pos1) (m2, pos2) = if pos1 == pos2 then 0 else m1 * m2 / distXYZ pos1 pos2

--This is unchanged.
distXYZ :: Pos -> Pos -> Double
distXYZ (x1, y1, z1) (x2, y2, z2) = sqrt (xd * xd + yd * yd + zd * zd)
	where
	(xd, yd, zd) = (x1 - x2, y1 - y2, z1 - z2)
-}


--Physics code, version 3:
{-
--Same as above.
calcMassesOverDists :: [Obj] -> [Double]
calcMassesOverDists objList = map (\obj -> sum (calcMMoD obj objList)) objList

--The code which avoids division by zero is now included here.
calcMMoD :: Obj -> [Obj] -> [Double]
calcMMoD (m1, pos1) objList = map (\(m2, pos2) ->
	if pos1 == pos2 then 0 else m1 * m2 / distXYZ pos1 pos2) objList

--This is unchanged.
distXYZ :: Pos -> Pos -> Double
distXYZ (x1, y1, z1) (x2, y2, z2) = sqrt (xd * xd + yd * yd + zd * zd)
	where
	(xd, yd, zd) = (x1 - x2, y1 - y2, z1 - z2)
-}


--Physics code, version 4:
{-
--Now we have nested lambda functions which avoid a function call.
--Not really necessary, since no clarity is gained.
calcMassesOverDists :: [Obj] -> [Double]
calcMassesOverDists objList = map
	(\obj1@(m1, pos1) -> sum (map (\(m2, pos2) ->
		if pos1 == pos2 then 0 else m1 * m2 / distXYZ pos1 pos2) objList) )
	objList

--This is unchanged.
distXYZ :: Pos -> Pos -> Double
distXYZ (x1, y1, z1) (x2, y2, z2) = sqrt (xd * xd + yd * yd + zd * zd)
	where
	(xd, yd, zd) = (x1 - x2, y1 - y2, z1 - z2)
-}



--Tree data type:
data Tree a = Null | Node a (Tree a) (Tree a)


--Tree example:
t1 :: Tree Int
t1 = Node 3 (Node 2 Null Null) (Node 5 (Node 4 Null Null) Null)


--Inorder tree traversal function:
inOrderList :: Tree a -> [a]
inOrderList Null	= []
inOrderList (Node item left right)	=
	inOrderList left ++ [item] ++ inOrderList right


--Infinite tree construction function:
foo :: Int -> Tree Int
foo n = Node n (foo (n - 1)) (foo (n + 1))

t2 :: Tree Int
t2 = foo 0


--Infinite type example:
data Forever a = AThing a (Forever a)



-- ## section5.html
-- (nothing)


-- ## section6.html

--Summation example to demonstrate '$':
sumListedv1, sumListedv2 :: Num a => [a] -> [Int] -> a
sumListedv1 nums indices = sum (map (nums !!) indices)

sumListedv2 nums indices = sum $ map (nums !!) indices


--List membership example to demonstrate '`':
printIsMemberv1, printIsMemberv2 :: (Eq a, Show a) => [a] -> a -> IO ()
printIsMemberv1 items item = if elem item items
	then putStrLn $ (show item) ++ " is a member of the list."
	else putStrLn $ (show item) ++ " is NOT a member of the list."

printIsMemberv2 items item = if item `elem` items
	then putStrLn $ (show item) ++ " is a member of the list."
	else putStrLn $ (show item) ++ " is NOT a member of the list."

