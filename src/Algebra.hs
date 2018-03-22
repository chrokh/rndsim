{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Algebra
  ( CurveExp
  , (@+)
  , (@-)
  , (@/)
  , (@*)
  , (@=)
  , compute
  ) where


----------------------------------------------
  -- Curve algebra
----------------------------------------------

import Curve
import Expression

-- By combining curves using operators we can produce arbitrarily complex
-- curves. This can be thought of as an algebra where operators operate on
-- curves and curve expressions to produce complex curve expressions.

type CurveExp = Exp Curve Double

(@+) :: Curve -> CurveExp -> CurveExp
(@+) c1 c2 = Operation c2 (+) c1

(@-) :: Curve -> CurveExp -> CurveExp
(@-) c1 c2 = Operation c2 (-) c1

(@/) :: Curve -> CurveExp -> CurveExp
(@/) c1 c2 = Operation c2 (/) c1

(@*) :: Curve -> CurveExp -> CurveExp
(@*) c1 c2 = Operation c2 (*) c1

(@=) :: Curve -> CurveExp -> CurveExp
(@=) c1 _ = Value c1

compute :: CurveExp -> Double -> Int -> Int -> Double
compute expr y1 dx x = eval ((flip $ fx y1 dx) x) expr


