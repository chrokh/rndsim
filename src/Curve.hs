{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Curve
  ( Curve (PntsCurve, GoalCurve, AreaCurve, IdCurve)
  , fx
  , shape
  , y2
  , y1
  , area
  ) where


----------------------------------------------
  -- Curves
----------------------------------------------

import Shape


-- The way a property of a stage is distributed over the length of the stage is
-- described by a curve. A curve is a partial description of a curve that can
-- be used to produce a function that maps from x to y.

data Curve = PntsCurve Shape Double Double
           | GoalCurve Shape Double
           | AreaCurve Shape Double
           | IdCurve


-- To map from a given x to some y, using a curve, we need to supply some
-- starting y (y1) and some delta x (dx) over which we should solve for any
-- potentially remaining properties of the curve.

fx ::  Double -> Int -> Curve -> Int -> Double
fx y1' dx curve x = let y1''   = y1 curve y1' dx
                        y2''   = y2 curve y1' dx
                     in interpolate (shape curve) dx y2'' y1'' x


-- We can trivially extract the shape of a curve.

shape :: Curve -> Shape
shape IdCurve           = Lin
shape (GoalCurve s _)   = s
shape (AreaCurve s _)   = s
shape (PntsCurve s _ _) = s


-- Using integration we can find y2 (target y) of a curve if given some y1
-- (starting point) and some delta x (time span).

y2 :: Curve -> Double -> Int -> Double
y2 (IdCurve) y1 _         = y1
y2 (AreaCurve _ area) _ _ = area / 2 -- TODO: Temp. Use integrals.
y2 (GoalCurve _ y2) _ _   = y2
y2 (PntsCurve _ _ y2) _ _ = y2


-- We can also ask a curve for it's y1 when given some previous y1 (starting
-- point) and some delta x. Note that all curves except a curve with an already
-- known y1 returns the supplied y1.

y1 :: Curve -> Double -> Int -> Double
y1 (PntsCurve _ y1 _) _ _ = y1
y1 _ y1 _                 = y1


-- Conversely, we can also find the area of a curve if given some y1 and some
-- delta x. The word 'area' is used indicatively and not precisely. The area of
-- a series of a probability curve is the total probability of successfully
-- performing each step of the curve. For probability, the area is in other
-- words the product, while for other properties it is the sum.

area :: Curve -> Double -> Int -> Double
area (IdCurve) y1 dx        = y1 * (fromIntegral dx) -- optimization
area (AreaCurve _ area) _ _ = area
area curve y1 dx            = foldr (+) 0 (map (fx y1 dx curve) [0..(dx-1)])
                            -- TODO: + should be * for prob.



