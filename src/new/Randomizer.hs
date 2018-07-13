module New.Randomizer
  ( Randomizer (Randomizer)
  , split
  ) where

import System.Random


data Randomizer = Randomizer


-- TODO: Should use StdGen rather than this fake implementation.
instance RandomGen Randomizer where
  next     g = (10, g)
  genRange g = (0,100)
  split    g = (g, g)
