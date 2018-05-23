{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module DSL
  ( uni
  , tri
  , pnt
  , hold
  , thru
  , upto
  , draw
  ) where


----------------------------------------------
  -- DSL
----------------------------------------------

import Samplable
import Distribution
import Shape
import ActivityDist
import CurveDist


-- The following DSL is defined in order to simplify the construction of
-- stochastic products.

uni min max     = Uniform min max
tri min mid max = Triangular min mid max
pnt n           = Estimate n

hold t =
  ActivityDist { timeDist = t
               , cashDist = IdCurveDist
               , costDist = IdCurveDist
               , probDist = IdCurveDist
               }
thru s t r c p =
  ActivityDist { timeDist = t
               , cashDist = AreaCurveDist s r
               , costDist = AreaCurveDist s c
               , probDist = AreaCurveDist s p
               }
upto s t r c p =
  ActivityDist { timeDist = t
               , cashDist = GoalCurveDist s r
               , costDist = GoalCurveDist s c
               , probDist = GoalCurveDist s p
               }
draw s t r c p =
  ActivityDist { timeDist = t
               , cashDist = PntsCurveDist s (fst r) (snd r)
               , costDist = PntsCurveDist s (fst c) (snd c)
               , probDist = PntsCurveDist s (fst p) (snd p)
               }

