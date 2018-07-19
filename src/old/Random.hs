{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Random
  ( succeeds
  , samplicate
  ) where



----------------------------------------------
  -- Randomness helpers
----------------------------------------------

import Samplable
import Distribution


-- Assuming that a probability is between 0 and 1 we can call succeeds with the
-- probability and a seed, and get back the next seed and a bool that represent
-- whether the probabilistic activity suceeded or not.

type Prob = Double
succeeds :: Prob -> Seed -> (Bool, Seed)
succeeds p s = let x = sample (Uniform (0 :: Double) (1 :: Double)) s
                in (fst x <= p, snd x)


-- samplicate works like replicate but replicates the constant c + X many times
-- where c is the integer base of the passed double, and X is a random variable
-- where the probability of 1 is equal to the decimals of the passed double.

samplicate :: a -> Double -> Seed -> ([a], Seed)
samplicate x p s
  | p > 1     = let next = samplicate x (p - 1) s
                 in (x : fst next, snd next)
  | otherwise = let roll = succeeds p s
                    dots = if (fst roll) then [x] else []
                 in (dots, snd roll)


