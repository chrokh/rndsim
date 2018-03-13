{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Shape
  ( Easing
  , Shape (Lin, Con, Sig, Exp, Log)
  , interpolate
  , easer
  ) where


----------------------------------------------
  -- Easing and interpolation
----------------------------------------------


-- Shapes describe different kinds of curves.

data Shape = Lin | Con | Sig | Exp | Log


-- Easing functions produce multipliers in the range [0, 1] when given values
-- in the range [0, 1].  The output can be thought of as the 'eased' version of
-- the input.  The non-trivial easing functions are sourced from:
-- https://gist.github.com/gre/1650294

type Easing = Double -> Double
always :: Easing; always x = 1
never  :: Easing; never  x = 0
linear :: Easing; linear x = x
quad   :: Easing; quad   x = if x < 0.5
                                then 2 * x * x
                                else -1 + (4 - 2 * x) * x


-- All shapes have a corresponding easing function.

easer :: Shape -> (Double -> Double)
easer Lin = linear
easer Con = always
easer Sig = quad
easer _   = \x -> x -- TODO


-- When given an easing function, a delta x, and the two y-values at min(dx)
-- and max(dx), interpolate yields a numeric function that can be graphed on a
-- cartesian plane. Example:
--
-- interpolate linear 100 10 5 2 = (linear 2/5) * (100-10) + 10 = 190

interpolate :: Easing -> Int -> Double -> Double -> Int -> Double
interpolate f dx y2 y1 x = let _x = fromIntegral x
                               _dx = fromIntegral x
                            in (f (_x/_dx)) * (y2-y1) + y1

