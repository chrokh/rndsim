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
import ProjectDist
import Project


data Discoverable = Discoverable { freq :: Double
                                 , proj :: ProjectDist
                                 }

instance Samplable Discoverable [Project] where
  sample disc seed = let dists = samplicate (proj disc) (freq disc) seed
                      in sampleAll (fst dists) (snd dists)

