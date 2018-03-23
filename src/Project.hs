{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Project
  ( Project
  , Activity (Activity, _time, _cash, _cost, _prob)
  , step
  , stpCash
  , stpCost
  , stpProb
  , stpFlow
  , stgCash
  , stgCost
  , stgProb
  , projBase
  , propDiff
  , flowDiff
  , activityBase
  ) where


----------------------------------------------
  -- Projects and Activities
----------------------------------------------

import Algebra
import Expression -- TODO: Should not be needed?


-- A project is a series of activities that need to be performed for the
-- project to be completed.

type Project = [Activity]

-- An activity is what is commonly described as a stage. An activity spans some
-- time and entails some cost, some revenues, and some probability of success.
-- These properties are distributed over the course of the stage as described
-- by their respective composite curves.

data Activity = Activity { _time :: Int
                         , _cash :: CurveExp
                         , _cost :: CurveExp
                         , _prob :: CurveExp
                         }

-- To compute the value of some particular property at some particular project
-- step, i.e. at some given time step, i.e. at some particular x, we need to
-- pass a getter that specifies which property we are looking for, a default
-- value (which can be thought of as the previous value in the recursion or the
-- accumulator in a fold), a project (which is a list of activities), and
-- finally some x.

step :: (Activity -> CurveExp) -> Double -> Int -> Project -> Double
step _ prev _ [] = prev
step get prev x (hd:tl)
  | x < 0           = prev
  | x <= (_time hd) = compute (get hd) prev (_time hd) x
  | otherwise       = let y1 = compute (get hd) prev (_time hd) x
                       in step get y1 (x - _time hd) tl


-- step is probably more easily understood through the following simplifying
-- aliases that help us extract particular properties from projects.

stpCash = step _cash 0 :: Int -> Project -> Double
stpCost = step _cost 0 :: Int -> Project -> Double
stpProb = step _prob 1 :: Int -> Project -> Double
stpFlow :: Int -> Project -> Double
stpFlow t p = stpCash t p - stpCost t p


-- To compute the total value of some particular property during all steps of
-- some particular stage, i.e. the e.g. cost of some whole stage or the
-- probability of a full stage, we need to pass a getter that specifies which
-- property we are looking for, a default value (which can be thought of as the
-- value of that property of the last step of the previous stage).

stage :: (Activity -> CurveExp) -> Double -> Int -> Project -> Double
stage _ prev _ [] = prev
stage get prev stg (hd:tl)
  | stg == 0  = let f x = compute (get hd) prev (_time hd) x
                    xs = [0..(_time hd - 1)]
                 in foldr (+) 0 $ map f xs
  | otherwise = let prv = compute (get hd) prev (_time hd) (_time hd - 1)
                 in stage get prv (stg - 1) tl


-- Let's also define a few aliases for common stage queries.

stgCash = stage _cash 0
stgCost = stage _cost 0
stgProb = stage _prob 1


-- If a project has been changed by expressions we can always ask for the
-- project's base.

projBase :: Project -> Project
projBase [] = []
projBase (hd:tl) = activityBase hd : projBase tl

activityBase :: Activity -> Activity
activityBase a = Activity { _time = _time a
                          , _cash = Value $ base $ _cash a
                          , _cost = Value $ base $ _cost a
                          , _prob = Value $ base $ _prob a }


-- Sum of some property of some project.
propSum :: (Activity -> CurveExp) -> Project -> Double
propSum f p = foldr (+) 0 $ map (\i -> stage f 0 i p) [0..(length p-1)]

-- Difference between the total sum of some property between two projects.
propDiff :: (Activity -> CurveExp) -> Project -> Project -> Double
propDiff f p1 p2 = (propSum f p1) - (propSum f p2)

-- Difference in total non-discounted cash flow (revenues - costs) between two projects.
flowDiff :: Project -> Project -> Double
flowDiff p1 p2 = propDiff _cash p1 p2 + propDiff _cost p1 p2
