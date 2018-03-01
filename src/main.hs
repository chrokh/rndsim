{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}






----------------------------------------------
  -- Distributions and sampling
----------------------------------------------

-- We're going to have to use a seed but I'm not sure how that will work yet so
-- let's just set up some dummy type that we can pass around.

type Seed = Int


-- To describe stochastic data, we need statistical distributions. So, let's
-- define a few distribution data types.

data Distribution a = Uniform a a
                    | Triangular a a a
                    | Estimate a


-- Distributions must be samplable, but how to sample a distribution might
-- depend on what the distribution contains. In other words, if we have a
-- distribution over a's then when sampling we should receive an a. Concretely,
-- for decimal numbers we want a decimal back and for integers, we want an
-- integer back. We express this by defining the class of Samplable, and
-- stating that samplable things must define the type sample that given a
-- Distribution of a and a Seed, return a sample of type a along with a Seed.
-- Importantly we must always return a new Seed since sampling is an impure
-- operation.

class Samplable a where
  sample :: Distribution a -> Seed -> (a, Seed)

instance Samplable Double where
  sample (Estimate x) seed             = (x, seed)    -- TODO
  sample (Uniform min max) seed        = (min, seed)  -- TODO
  sample (Triangular min mid max) seed = (min, seed)  -- TODO

instance Samplable Int where
  sample (Estimate x) seed             = (x, seed)   -- TODO
  sample (Uniform min max) seed        = (min, seed) -- TODO
  sample (Triangular min mid max) seed = (min, seed) -- TODO








----------------------------------------------
  -- Easing and interpolation
----------------------------------------------

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


-- When given an easing function, a delta x, and the two y-values at min(dx)
-- and max(dx), interpolate yields a numeric function that can be graphed on a
-- cartesian plane. Example:
--
-- interpolate linear 100 10 5 2 = (linear 2/5) * (100-10) + 10 = 190

interpolate :: Easing -> Int -> Double -> Double -> Int -> Double
interpolate f dx y2 y1 x = let _x = fromIntegral x
                               _dx = fromIntegral x
                            in (f (_x/_dx)) * (y2-y1) + y1


-- Shapes describe different kinds of curves.

data Shape = Lin | Con | Sig | Exp | Log

-- All shapes have a corresponding easing function.

easer :: Shape -> (Double -> Double)
easer Lin = linear
easer Con = always
easer Sig = quad
easer _   = \x -> x -- TODO








----------------------------------------------
  -- Curves
----------------------------------------------

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

fx :: Curve -> Double -> Int -> Int -> Double
fx curve y1' dx x = let y1''   = y1 curve y1' dx
                        y2''   = y2 curve y1' dx
                        easing = easer $ shape curve
                     in interpolate easing dx y2'' y1'' x


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
area shape y1 dx            = foldr (+) 0 (map (fx shape y1 dx) [0..(dx-1)])
                            -- TODO: + should be * for prob.




-- Interventions combines curves with curves using operators and produce
-- composite curves. A composite curve is either just a curve, or a recursive
-- composition of an operator, a curve, and a composite curve.
data CompCurve = BaseCurve Curve
               | CompCurve Operator Curve CompCurve


-- Any operator that can operate on a pair of doubles and produce a double is
-- an operator that can be used to alter stage properties.
type Operator = Double -> Double -> Double


-- We can perform multiple operations on composite curves. base is a simple
-- function that takes a composite curve, strips all of the wrappers and
-- returns the base curve at the bottom.
base :: CompCurve -> Curve
base (BaseCurve c)        = c
base (CompCurve o c next) = base next

-- A more complex composite curve operation is unwrap. When given a composite
-- curve, some delta x, some starting y, and some x, it computes f(x), i.e. y,
-- for all the curves in the composite and folds them into a final number using
-- their respective operators.
unwrap :: CompCurve -> Int -> Double -> Int -> Double
unwrap (BaseCurve curve) dx y1 x          = fx curve y1 dx x
unwrap (CompCurve op curve next) dx y1 x  = (unwrap next dx y1 x) `op` (unwrap (BaseCurve curve) dx y1 x)








----------------------------------------------
  -- Projects and Activities
----------------------------------------------

-- A project is a series of activities that need to be performed for the
-- project to be completed.
type Proj = [Activity]

-- An activity is what is commonly described as a stage. An activity spans some
-- time and entails some cost, some revenues, and some probability of success.
-- These properties are distributed over the course of the stage as described
-- by their respective composite curves.
data Activity = Activity { time :: Int
                         , cash :: CompCurve
                         , cost :: CompCurve
                         , prob :: CompCurve
                         }

-- To compute the value (i.e. height / y / f(x) ) of some particular property
-- of a project at some given x (i.e. time step) we need to pass a getter that
-- specifies which property we are looking for, a default value (which can be
-- thought of as the previous value in the recursion or the accumulator in a
-- fold), a project (which is a list of activities), and finally some x.
prop :: (Activity -> CompCurve) -> Double -> Proj -> Int -> Double
prop _ prev [] _  = prev
prop get prev (hd:tl) x
  | x < 0          = prev
  | x <= (time hd) = unwrap (get hd) (time hd) prev x
  | otherwise      = let y1 = unwrap (get hd) (time hd) prev x
                     in prop get y1 tl (x - time hd)

-- prop is probably more easily understood through the following simplifying
-- aliases that help us extract particular properties from projects.
cashAt = prop cash 0 :: Proj -> Int -> Double
costAt = prop cost 0 :: Proj -> Int -> Double
probAt = prop prob 1 :: Proj -> Int -> Double






----------------------------------------------
  -- Sampling projects
----------------------------------------------

-- However, since we need to be able to describe stochastic projects, and thus
-- stages, steps and props, we will define stochastic counterparts for all
-- these types.

type StochasticProject = [StochasticActivity]
data StochasticActivity = StochasticActivity { timeDist :: Distribution Int
                                             , cashDist :: StochasticCurve
                                             , costDist :: StochasticCurve
                                             , probDist :: StochasticCurve
                                             }

data StochasticCurve = StochasticIdCurve
                     | StochasticGoalCurve Shape (Distribution Double)
                     | StochasticAreaCurve Shape (Distribution Double)



-- When passing a StochasticProject and a seed to the function sampleProject we
-- will get back a concrete Project along with the next seed. Clearly this also
-- entails turning StochasticActivities and StochasticCurves into Stages and Props
-- respectively. The following sampling functions will of course not apply the
-- same seed multiple times but rather always return a new seed along with
-- every "random" sampling so that the next sampling can make sure of the new
-- seed.

sampleProject :: StochasticProject -> Seed -> ([Activity], Seed)
sampleProject [   ] seed = ([], seed)
sampleProject (h:t) seed = let _h = sampleActivity h seed
                               _t = sampleProject t (snd _h)
                               _this = fst _h
                               _next = fst _t
                               _seed = snd _t
                            in (_this : _next , _seed)

sampleActivity :: StochasticActivity -> Seed -> (Activity, Seed)
sampleActivity y2 seed = let s1 = sample (timeDist y2) seed
                             s2 = sampleCurve (cashDist y2) (snd s1)
                             s3 = sampleCurve (costDist y2) (snd s2)
                             s4 = sampleCurve (probDist y2) (snd s3)
                             time = fst s1
                             cash = BaseCurve $ fst s2
                             cost = BaseCurve $ fst s3
                             prob = BaseCurve $ fst s4
                             stp = Activity { time, cash, cost, prob }
                          in (stp, snd s4)

sampleCurve :: StochasticCurve -> Seed -> (Curve, Seed)
sampleCurve StochasticIdCurve seed = (IdCurve, seed)
sampleCurve (StochasticGoalCurve shape dist) seed =
  let x = sample dist seed
   in (GoalCurve shape (fst x), (snd x))
sampleCurve (StochasticAreaCurve shape dist) seed =
  let x = sample dist seed
   in (AreaCurve shape (fst x), (snd x))





--
-- MAIN
--

main = putStrLn ("Hello world")

