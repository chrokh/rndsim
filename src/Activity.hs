{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Activity
  ( Activity (Activity, _time, _cash, _cost, _prob)
  , activityBase
  ) where


----------------------------------------------
  -- Products and Activities
----------------------------------------------

import Algebra
import Expression -- TODO: Should not be needed?


-- An activity is what is commonly described as a stage. An activity spans some
-- time and entails some cost, some revenues, and some probability of success.
-- These properties are distributed over the course of the stage as described
-- by their respective composite curves.

data Activity = Activity { _time :: Int
                         , _cash :: CurveExp
                         , _cost :: CurveExp
                         , _prob :: CurveExp
                         }

activityBase :: Activity -> Activity
activityBase a = Activity { _time = _time a
                          , _cash = Value $ base $ _cash a
                          , _cost = Value $ base $ _cost a
                          , _prob = Value $ base $ _prob a }

