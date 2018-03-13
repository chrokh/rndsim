{-# LANGUAGE FunctionalDependencies , FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Samplable
  ( Seed
  , Distribution (Uniform, Triangular, Estimate)
  , Samplable
  , sample
  , sampleAll
  , samplicate
  , succeeds
  ) where


----------------------------------------------
  -- Distributions and sampling
----------------------------------------------

-- We're going to have to use a seed but I'm not sure how that will work yet so
-- let's just set up some dummy type that we can pass around.

type Seed = Int


-- To describe stochastic data, we need statistical distributions. So, let's
-- define a few distribution data types.

data Distribution a = Uniform a a
                    | Triangular a a a
                    | Estimate a


-- Distributions must be samplable, but how to sample a distribution might
-- depend on what the distribution contains. In other words, if we have a
-- distribution over a's then when sampling we should receive an a. Concretely,
-- for decimal numbers we want a decimal back and for integers, we want an
-- integer back. We express this by defining the class of Samplable, and
-- stating that samplable things must define the type sample that given a
-- Distribution of a and a Seed, return a sample of type a along with a Seed.
-- Importantly we must always return a new Seed since sampling is an impure
-- operation.

class Samplable a b | a -> b where
  sample :: a -> Seed -> (b, Seed)

instance Samplable (Distribution Double) Double where
  sample (Estimate x) seed             = (x, seed)    -- TODO
  sample (Uniform min max) seed        = (min, seed)  -- TODO
  sample (Triangular min mid max) seed = (min, seed)  -- TODO

instance Samplable (Distribution Int) Int where
  sample (Estimate x) seed             = (x, seed)   -- TODO
  sample (Uniform min max) seed        = (min, seed) -- TODO
  sample (Triangular min mid max) seed = (min, seed) -- TODO


-- If you have a list of samplables and a seed then you can sample them to
-- produce a list of samples and a new seed.

sampleAll :: Samplable a b => [a] -> Seed -> ([b], Seed)
sampleAll [] s        = ([], s)
sampleAll (hd : tl) s = let this = sample hd s
                            next = sampleAll tl (snd this)
                            smpl = fst this
                         in (smpl : fst next, snd next)


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
