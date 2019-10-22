--Eric Etheridge
--2009 05 18
--ExampleGHCframe.hs

--This file contains the simple framework for working with GHC listed in section 3.
--This file can be loaded at an interpretive prompt or compiled and run.

--At the very least, the definition of 'someFunc' must be completed in order to use this code.
--Its type should be changed as needed.


someFunc :: Int -> Int -> [Int]
someFunc .........

main = do
	putStr "prompt 1"
	a <- getLine
	putStr "prompt 2"
	b <- getLine
	putStrLn (show (someFunc (read a) (read b)))

