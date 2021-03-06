{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Product
  ( Product
  , stpCash
  , stpCost
  , stpProb
  , stpFlow
  , stgCash
  , stgCost
  , stgProb
  , prodBase
  , propDiff
  , flowDiff
  , activityBase
  ) where


----------------------------------------------
  -- Products and Activities
----------------------------------------------

import Algebra
import Activity


-- A product is a series of activities that need to be performed for the
-- product to be completed.

type Product = [Activity]


-- To compute the value of some particular property at some particular product
-- step, i.e. at some given time step, i.e. at some particular x, we need to
-- pass a getter that specifies which property we are looking for, a default
-- value (which can be thought of as the previous value in the recursion or the
-- accumulator in a fold), a product (which is a list of activities), and
-- finally some x.

step :: (Activity -> CurveExp) -> Double -> Int -> Product -> Double
step _ prev _ [] = prev
step get prev x (hd:tl)
  | x < 0           = prev
  | x <= (_time hd) = compute (get hd) prev (_time hd) x
  | otherwise       = let y1 = compute (get hd) prev (_time hd) x
                       in step get y1 (x - _time hd) tl


-- step is probably more easily understood through the following simplifying
-- aliases that help us extract particular properties from products.

stpCash = step _cash 0 :: Int -> Product -> Double
stpCost = step _cost 0 :: Int -> Product -> Double
stpProb = step _prob 1 :: Int -> Product -> Double
stpFlow :: Int -> Product -> Double
stpFlow t p = stpCash t p - stpCost t p


-- To compute the total value of some particular property during all steps of
-- some particular stage, i.e. the e.g. cost of some whole stage or the
-- probability of a full stage, we need to pass a getter that specifies which
-- property we are looking for, a default value (which can be thought of as the
-- value of that property of the last step of the previous stage).

stage :: (Activity -> CurveExp) -> Double -> Int -> Product -> Double
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


-- If a product has been changed by expressions we can always ask for the
-- product's base.

prodBase :: Product -> Product
prodBase [] = []
prodBase (hd:tl) = activityBase hd : prodBase tl


-- Sum of some property of some product.
propSum :: (Activity -> CurveExp) -> Product -> Double
propSum f p = foldr (+) 0 $ map (\i -> stage f 0 i p) [0..(length p-1)]

-- Difference between the total sum of some property between two products.
propDiff :: (Activity -> CurveExp) -> Product -> Product -> Double
propDiff f p1 p2 = (propSum f p1) - (propSum f p2)

-- Difference in total non-discounted cash flow (revenues - costs) between two products.
flowDiff :: Product -> Product -> Double
flowDiff p1 p2 = propDiff _cash p1 p2 + propDiff _cost p1 p2
