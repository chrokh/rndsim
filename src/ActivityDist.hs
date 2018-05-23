{-# LANGUAGE NamedFieldPuns, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ActivityDist
  ( ActivityDist ( ActivityDist
                 , timeDist
                 , cashDist
                 , costDist
                 , probDist
                 )
  ) where


import Samplable
import Distribution
import Activity
import CurveDist
import Expression


data ActivityDist
  = ActivityDist
    { timeDist :: Distribution Int
    , cashDist :: CurveDist
    , costDist :: CurveDist
    , probDist :: CurveDist
    }


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
