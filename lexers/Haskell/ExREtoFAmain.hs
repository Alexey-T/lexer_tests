--Eric Etheridge
--2009 05 18
--ExREtoFAmain.hs

--This file contains the main function for the regular expression and finite automata example.

--Usage:
--This can be used in GHCi as follows:
-- $ ghci ExampleREtoFAmain.hs
-- > main

--This can also be compiled and run from the command line:
-- $ ghc ExampleREtoFAmain.hs --make
-- $ ./ExampleREtoFAmain.hs

--See ExampleREtoFAexampleTests.txt for starting points to test this program.


import ExREtoFAtypes
import ExREtoFAconvertRE	--stage 1: first input string to RE (3 versions across 3 modules with 2 submodules)
import ExREtoFAconvertNFA	--stage 2: RE to NFA (1 version completed)
import ExREtoFAconvertDFA	--stage 3: NFA to DFA (1 version)
import ExREtoFAexecution	--stage 4: running RE, NFA, and DFA on further input strings (3 operations, 2 versions each)
import Control.Monad (when)
import System.IO (hFlush, stdout)
import Data.List (nub)

main = do
	putStr ""
		--'putStr' does not add a newline, which means that the output is not flushed to stdout.
		--The default mode is line buffering, which is true for most Unix systems.
		--We could change the buffering mode, but it is more appropriate to flush as needed.
	putStr "Enter a regular expression: "
	hFlush stdout
	reStr <- getLine
	putStrLn "Converting to an RE structure..."
		--The 'let' syntax here allows us to define variable bindings in a do notations.
		--This does not cause evaluation.
	let (Just theRE1) = convertStringToRE1 reStr
	let (Just theRE2) = convertStringToRE2 reStr
	let (Just theRE3) = convertStringToRE3 reStr
	putStrLn $ "Converted to RE."
	if theRE1 == theRE2 && theRE1 == theRE3
		then do
			putStrLn "The regular expressions are equal using conversions 1, 2, and 3."
			putStrLn (show theRE1)
		else do
			putStrLn "The regular expressions are NOT equal using conversions 1, 2, and 3."
			putStrLn "First (non-monadic):"
			putStrLn (show theRE1)
			putStrLn "Second (monadic):"
			putStrLn (show theRE2)
			putStrLn "Third (monadic streamlined):"
			putStrLn (show theRE3)

	let theRE = theRE1
	putStrLn ""
	let listedChars = findCharsReferenced theRE
	putStrLn $ "This RE lists the following characters: \"" ++ listedChars ++ "\""
	putStr "Is this the correct list of valid inputs? (Y/n)"
	hFlush stdout
	yn <- getLine
		--Here we use an 'if then else' construct as a line in do notation.
		--This is fine as long as each branch has the same return type, as normal.
		--This can make the parser refuse to accept additional lines as part the block, however.
	if yn `elem` ["", "y", "Y", "yes", "YES", "Yes"]
		then handleRE theRE listedChars
		else do
		putStr "Enter the valid characters, or enter for [a-zA-Z0-9]:"
		hFlush stdout
		chars <- getLine
		if chars /= ""
			then handleRE theRE (nub chars)
			else handleRE theRE (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])



handleRE theRE validChars = do
	putStrLn "Converting to an NFA structure..."
	let theNFA = convertREtoNFA validChars theRE
	putStrLn $ "Converted to NFA.  Number of states: " ++ (show (length theNFA))
	putStrLn (show theNFA)

	putStrLn "Converting to a DFA structure..."
	let theDFA = convertNFAtoDFA validChars theNFA
	putStrLn $ "Converted to DFA.  Number of states: " ++ (show (length (snd theDFA)))
	putStrLn (show theDFA)

	tryInputs validChars theRE theNFA theDFA


--Control.Monad: when
tryInputs validChars theRE theNFA theDFA = do
	putStrLn ""
	putStr "Use Ctrl-C to quit.  Enter an input to test: "
	hFlush stdout
	testStr <- getLine
	when (not (validInputString validChars testStr)) noteInvalidInput

	putStrLn "Running DFA on input (equivalent for single state and state set tracking)..."
	let (Just result) = runDFA1 testStr theDFA
	putStrLn $ "Result from DFA: " ++ (show result)

	putStrLn "Running NFA on input using single state tracking..."
	let (Just result) = runNFA1 testStr theNFA
	putStrLn $ "Result from NFA: " ++ (show result)

	putStrLn "Running NFA on input using state set tracking..."
	let (Just result) = runNFA2 testStr theNFA
	putStrLn $ "Result from NFA: " ++ (show result)

	putStrLn "Running RE on input using single state tracking..."
	let result = runRE1 testStr theRE
	putStrLn $ "Result from RE: " ++ (show result)

	putStrLn "Running RE on input using state set tracking..."
	let result = runRE2 testStr theRE
	putStrLn $ "Result from RE: " ++ (show result)

	tryInputs validChars theRE theNFA theDFA

	where

	noteInvalidInput = do
		putStrLn "Invalid input string."
		putStrLn $ "Use valid input characters: " ++ validChars
		tryInputs validChars theRE theNFA theDFA

