{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Discoverable
  ( Discoverable (Discoverable)
  ) where


----------------------------------------------
  -- Basic research / Discovery
----------------------------------------------

import Samplable
import Random
import ProductDist
import Product


data Discoverable = Discoverable { freq :: Double
                                 , prod :: ProductDist
                                 }

instance Samplable Discoverable [Product] where
  sample disc seed = let dists = samplicate (prod disc) (freq disc) seed
                      in sampleAll (fst dists) (snd dists)

