{-# LANGUAGE NamedFieldPuns, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ProductDist
  ( ProductDist
  , ActivityDist ( ActivityDist
                 , timeDist
                 , cashDist
                 , costDist
                 , probDist
                 )
  , CurveDist ( IdCurveDist
              , GoalCurveDist
              , AreaCurveDist
              , PntsCurveDist
              )
  ) where


----------------------------------------------
  -- Sampling products
----------------------------------------------

import Samplable
import Distribution
import Product
import Curve
import Shape
import Expression


-- However, since we need to be able to describe stochastic products, and thus
-- stages, steps and props, we will define stochastic counterparts for all
-- these types.

type ProductDist = [ActivityDist]
data ActivityDist = ActivityDist { timeDist :: Distribution Int
                                 , cashDist :: CurveDist
                                 , costDist :: CurveDist
                                 , probDist :: CurveDist
                                 }

data CurveDist = IdCurveDist
               | GoalCurveDist Shape (Distribution Double)
               | AreaCurveDist Shape (Distribution Double)
               | PntsCurveDist Shape (Distribution Double) (Distribution Double)


-- When sampling a Product dist we will get a Product. Clearly this entails
-- sampling each individual ActivityDist of the ProductDist, which in turn
-- entails sampling each individual distribution (e.g. CurveDist) of all
-- properties of the activity. Of course, the same seed must not be reused when
-- sampling multiple times, which means that all sampling functions return the
-- sampled result and a new seed that can be passed to the next sampler.

instance Samplable ProductDist Product where
  sample = sampleAll

instance Samplable ActivityDist Activity where
  sample y2 seed = let s1 = sample (timeDist y2) seed
                       s2 = sample (cashDist y2) (snd s1)
                       s3 = sample (costDist y2) (snd s2)
                       s4 = sample (probDist y2) (snd s3)
                       _time = fst s1
                       _cash = Value $ fst s2
                       _cost = Value $ fst s3
                       _prob = Value $ fst s4
                       stp = Activity { _time, _cash, _cost, _prob }
                    in (stp, snd s4)

instance Samplable CurveDist Curve where
  sample IdCurveDist seed = (IdCurve, seed)
  sample (GoalCurveDist shape dist) seed = let x = sample dist seed
                                            in (GoalCurve shape (fst x), snd x)
  sample (AreaCurveDist shape dist) seed = let x = sample dist seed
                                            in (AreaCurve shape (fst x), snd x)
  sample (PntsCurveDist shape d1 d2) seed = let x1 = sample d1 seed
                                                x2 = sample d2 (snd x1)
                                             in (PntsCurve shape (fst x1) (fst x2), snd x2)

