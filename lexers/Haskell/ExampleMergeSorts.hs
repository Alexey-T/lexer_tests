--Eric Etheridge
--2009 05 18
--ExampleMergeSort.hs

--variant implementations of mergeSort
--Usage:
--This file is best used in GHCi.
--Use 'mergeSort1', 'mergeSort2', and 'mergeSort3' on any list.
--E.g. 'mergeSort2 [3,5,2,6,4]'
--Some sample lists are provided.


--Sample Unordered Lists:

list1 = [3, 5, 2, 6, 4]
list2 = [3, 5, 2, 6, 4, 3, 5, 2, 6, 4]
list3 = [1, 3, 8, 5, 2, 6, 4, 9, 11, -1, 7]




--The same merge operation is used by all versions of mergeSort:

	--this is not tail recursion and may therefore cause stack issues
	--however, it avoids a reverse step and some slightly more complicated code
merge :: Ord a => [a] -> [a] -> [a]
merge [] right = right
merge left [] = left
merge left@(x:xs) right@(y:ys)
	| x < y		= x : merge xs right
	| otherwise	= y : merge left ys


--Types are the same for each version of mergeSort:
--mergeSort1, mergeSort3, mergeSort2 :: Ord a => [a] -> [a]



--Merge Sort Version 1, Simple and Standard:
	--For an empty list, an empty list is returned.
mergeSort1 [] = []
	--For a single value, that single value is returned.
mergeSort1 [x] = [x]
	--For more than one value (not matched above), we recurse twice then merge.
mergeSort1 theList = merge (mergeSort1 left) (mergeSort1 right)
	where
	(left, right) = splitAt ((length theList) `div` 2) theList

--This walks down the list in an unnecessary manner.




--It is possible to take the length once and then estimate from there:

--Merge Sort Version 3, Split Based on Max Length:
	--a helper function is required which passes a (maximum) length
mergeSort2 xs = mergeSort2helper xs (length xs)

mergeSort2helper :: Ord a => [a] -> Int -> [a]
	--For an empty list, an empty list is returned.
mergeSort2helper [] _ = []
	--For a single value, that single value is returned.
mergeSort2helper [x] _ = [x]
	--More than a single value is split based on its max possible length.
mergeSort2helper theList k = merge (mergeSort2helper left halfK) (mergeSort2helper right halfK)
	where
		--We must split at least one item off of a two-item list.
	halfK = max (k `div` 2) 1
	(left, right) = splitAt halfK theList

--This still walks down the list to split the list each time.



--We can avoid the call to splitAt.
--Split in half by sending every other item to other list, rather than split at middle

--Merge Sort Version 3, Split By Alternation:
	--a helper function is required which passes more parameters
mergeSort3 xs = mergeSort3helper xs [] []

	--the new parameters hold the left and right lists
mergeSort3helper :: (Ord a, Show a) => [a] -> [a] -> [a] -> [a]
	--For an empty list, an empty list is returned.
mergeSort3helper [] [] [] = []
	--For a single value, that single value is returned.
mergeSort3helper [x] [] [] = [x]
	--More than a single value is split (which reverses order with each pass).
mergeSort3helper (x:xs) left right = mergeSort3helper xs right (x:left)
	--The end of a list with stored values triggers recursion then merge.
mergeSort3helper [] left right = merge (mergeSort3helper left [] []) (mergeSort3helper right [] [])

--However, this is not actually a gain in performance.

--In #1, we use two builtin functions to walk down the list.
--In #2, we use one.
--In #3, we walk down the list using a function we wrote.
--This is not an improvement.
--In fact, it is a loss over #2 since #2 only walks half the list with each call to splitAt.

