{-# LANGUAGE NamedFieldPuns, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CurveDist
  ( CurveDist ( IdCurveDist
              , GoalCurveDist
              , AreaCurveDist
              , PntsCurveDist
              )
  ) where


import Samplable
import Distribution
import Shape
import Curve


data CurveDist
  = IdCurveDist
  | GoalCurveDist Shape (Distribution Double)
  | AreaCurveDist Shape (Distribution Double)
  | PntsCurveDist Shape (Distribution Double) (Distribution Double)

instance Samplable CurveDist Curve where
  sample IdCurveDist seed = (IdCurve, seed)

  sample (GoalCurveDist shape dist) seed =
    let x = sample dist seed
     in (GoalCurve shape (fst x), snd x)

  sample (AreaCurveDist shape dist) seed =
    let x = sample dist seed
     in (AreaCurve shape (fst x), snd x)

  sample (PntsCurveDist shape d1 d2) seed =
    let x1 = sample d1 seed
        x2 = sample d2 (snd x1)
     in (PntsCurve shape (fst x1) (fst x2), snd x2)

