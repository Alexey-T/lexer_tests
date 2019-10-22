--Eric Etheridge
--2009 05 18
--ExREtoFAconvertREv3monad.hs

--stage 1: first input string to RE (3 versions across 3 modules with 2 submodules)
--version 3's submodule.
--A few extra accessor functions are defined for this monad.


module ExREtoFAconvertREv3monad (
		--monad type for version 3, not passing constructor
		--instance as Monad is passed automatically
	REmaker3(),

		--running function
	runRE3monadReturnState,

		--accessor functions
	peekChars3, mayGetChar3, remChars3, spanREmaker3
	)
	where


import ExREtoFAtypes


{-#### Conversion from RE String to Regular Expression, Version #3, Monad Declaration ####-}


--the type for use as a Monad:
newtype REmaker3 a = RE3 ( String -> (String, a) )

--We must define the monad type as an instance of Monad:
instance Monad REmaker3 where
		--The result is of type REmaker3, so it uses the RE3 constuctor and takes a state.
	(RE3 initMonad)  >>=  monadAction	= RE3 (\stateA ->
		--With that state and the lefthand parameter to (>>=), we compute new values.
		let	(stateB, returnB) = initMonad stateA
		--The return value is passed to the righthand parameter, the composed action.
		--We deconstruct the result to get the new monad.
			(RE3 composedMonad) = monadAction returnB
		--The result is a new monad transformation which needs the intermediate state.
		in
		composedMonad stateB
		)
	--The returned monad must be a function which takes a state.  This state is unchanged.
	return v	= (RE3 (\state -> (state, v)))


--This function lets us call a composed monadic operation on an initial state.
runRE3monadReturnState :: String -> REmaker3 a -> (String, a)
runRE3monadReturnState str (RE3 monadAction) = monadAction str


--This function lets monadic actions examine the state.
peekChars3 :: Int -> REmaker3 [Char]
peekChars3 n = RE3 (\s -> (s, take n s))

--This function alters the state and returns Just a character, or Nothing if none is present.
mayGetChar3 :: REmaker3 (Maybe Char)
mayGetChar3 = RE3 (\s -> if s == "" then (s, Nothing) else (tail s, Just (head s)))

--This function allows a character to be removed and ignored.
--It is a NO-OP if there are no more characters in the state.
remChars3 :: Int -> REmaker3 ()
remChars3 n = RE3 (\s -> (drop n s, ()))

--This function takes a filtering function and scans for all the initial characters which satisfy it.
--This is simpler than in the second version because it has internal access.
spanREmaker3 :: (Char -> Bool) -> REmaker3 [Char]
spanREmaker3 checkFunc = RE3 (\s -> let (matches, rest) = span checkFunc s in (rest, matches))




