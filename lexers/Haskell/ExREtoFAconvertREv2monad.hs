--Eric Etheridge
--2009 05 18
--ExREtoFAconvertREv2monad.hs

--stage 1: first input string to RE (3 versions across 3 modules with 2 submodules)
--version 2's submodule.
--Only a few accessor functions are defined for this monad.


module ExREtoFAconvertREv2monad (
		--monad type for version 2, not passing constructor
		--instance as Monad is passed automatically
	REmaker2(),

		--running function
	runRE2monadReturnState,

		--accessor functions
	peekChars2, mayGetChar2
	)
	where


import ExREtoFAtypes


{-#### Conversion from RE String to Regular Expression, Version #2, Monad Declaration ####-}


--the type for use as a Monad:
--A 'data' type is used so that outside functions cannot take apart the monad.
--The type must include a type variable, used to pass return values.
--In simpler monads such as [] and Maybe, no other data is really present,
--but more complex monads such as this actually carry data of their own.
--This data must be a function parameter in order to be carried forward.
--The whole operation needs a 'newtype' because type synonyms (defining types
--with 'type') are not allowed in Instance declarations.
newtype REmaker2 a = RE2 ( String -> (String, a) )

--We must define the monad type as an instance of Monad:
instance Monad REmaker2 where
		--The result is of type REmaker2, so it uses the RE2 constuctor and takes a state.
	(RE2 initMonad)  >>=  monadAction	= RE2 (\stateA ->
		--With that state and the lefthand parameter to (>>=), we compute new values.
		let	(stateB, returnB) = initMonad stateA
		--The return value is passed to the righthand parameter, the composed action.
		--We deconstruct the result to get the new monad.
			(RE2 composedMonad) = monadAction returnB
		--The result is a new monad transformation which needs the intermediate state.
		in
--		trace (">>=2: stateA, stateB: \"" ++ stateA ++ "\", \"" ++ stateB ++ "\"")
		composedMonad stateB
		)
	--The returned monad must be a function which takes a state.  This state is unchanged.
	return v	= (RE2 (\state -> (state, v)))


--This function lets us call a composed monadic operation on an initial state.
--The one time that we call it, the return type will be 'Maybe MyRegExp'.
--This function is only necessary in this example
--because the monad type is in a separate file from functions that use the type.
--This is good programming practice, since it separates accessor functions from composition functions.
runRE2monadReturnState :: String -> REmaker2 a -> (String, a)
runRE2monadReturnState str (RE2 monadAction) = monadAction str


--This function lets monadic actions examine the state.
peekChars2 :: Int -> REmaker2 [Char]
peekChars2 n = RE2 (\s -> (s, take n s))

--This function alters the state and returns Just a character, or Nothing if none is present.
mayGetChar2 :: REmaker2 (Maybe Char)
mayGetChar2 = RE2 (\s -> if s == "" then (s, Nothing) else (tail s, Just (head s)))

