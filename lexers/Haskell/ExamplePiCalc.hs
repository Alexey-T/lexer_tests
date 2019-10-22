--Eric Etheridge
--2009 05 18
--ExamplePiCalc.hs

--calculations of pi
--Usage:
--This file is best used in GHCi.
--'pi1', 'pi2', and 'pi3' are calculated constants in this file.
--Once they are evaluated once, they are fixed.
--Compare them to the builtin value 'pi'.


--Most information taken from this page:
--http://en.wikipedia.org/wiki/Computing_%CF%80



import Data.List (mapAccumL)



pi1, pi2, pi3 :: Double
pi1iters, pi2iters, pi3iters :: Int

{-#### Version #1: Gregory-Leibniz Series ####-}

--(arctan(x), also called inverse tangent, specialized for x = 1)
--pi = +4 / 1 - 4 / 3 + 4 / 5 - 4 / 7 + 4 / 9 ...
--converges on useful value extrememly slowly (requires a large number of iterations)
--Data.List: mapAccumL
pi1 = sum $ take pi1iters $ map (\x -> 4.0/x) $ snd $
		mapAccumL (\m x -> (not m, if m then x else -x)) True [1,3..]

--approx. minimum iterations needed for 13 decimal digits of accuracy (only 13 using 500,000!)
pi1iters = 500000



{-#### Version #2: Algebraic Simplification of ArcTan ####-}

--algebraic simplification of above
--arctan(1) * 4 = pi
--pi/2 = sum (n=0..inf) of: product [1..n] / product [1,3..(2*n+1)]
--pi/2 = 1 + 1/3 + 1*2/3*5 + 1*2*3/3*5*7 + ...
pi2 = 2 * (sum $ take pi2iters $ map
		(\n -> product [1..n] / product [1,3..(2*n+1)])
		[0..])

--approx. minimum iterations needed for 15 decimal digits of accuracy
pi2iters = 50



{-#### Version #3: Equilateral Triangle ####-}

--from an equilateral triangle:
--sin (pi / 6) = 1/2
--pi = 3 * sum (n=0..inf) of:
--  binomChoose(2n, n) * (1 / ((2n + 1) * 16 ^ n))


--Prelude: fromIntegral and (^)
--http://www.haskell.org/ghc/docs/6.6/html/libraries/base/Prelude.html#8
pi3 = 3 * (sum $ take pi3iters $ map
		(\x -> fromIntegral (binomChoose (2*x) x) / fromIntegral (2*x + 1) / fromIntegral (16^x))
		[0..])

--approx. minimum iterations needed for 15 decimal digits of accuracy
pi3iters = 25


--extra function for calculating pi3:
--reference: http://en.wikipedia.org/wiki/Binomial_coefficient
--We do not constrain this to integers, since the result grows exponentially and easily exceeds Int.
binomChoose :: (Integral t) => t -> t -> t
binomChoose n k = if 0 <= k && k <= n then product [k+1..n] `div` product [1..n-k] else 0



{-#### Other Possible Methods ####-}

--Archimedes method:
--sandwich area of polygons circumscribing and inscribing a circle

--Salamin–Brent / Gauss–Legendre algorithm

