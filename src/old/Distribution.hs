{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Distribution
  ( Distribution (Uniform, Triangular, Estimate)
  ) where


----------------------------------------------
  -- Distributions
----------------------------------------------

import Samplable


-- To describe stochastic data, we need statistical distributions. So, let's
-- define a few distribution data types.

data Distribution a = Uniform a a
                    | Triangular a a a
                    | Estimate a


-- Distributions are samplable.

instance Samplable (Distribution Double) Double where
  sample (Estimate x) seed             = (x, seed)    -- TODO
  sample (Uniform min max) seed        = (min, seed)  -- TODO
  sample (Triangular min mid max) seed = (min, seed)  -- TODO

instance Samplable (Distribution Int) Int where
  sample (Estimate x) seed             = (x, seed)   -- TODO
  sample (Uniform min max) seed        = (min, seed) -- TODO
  sample (Triangular min mid max) seed = (min, seed) -- TODO
