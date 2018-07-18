module New.Actions.ProductionInfo
  ( ProductionInfo ( project
                   , producer
                   , disease
                   , marketShare
                   )
  ) where

import New.Aliases
import New.Uuid


-- While a drug can cure multiple diseases. We assume that the rate at which a
-- drug penetrates a given disease market is independent across diseases.

data ProductionInfo = ProductionInfo
  { project     :: Uuid
  , producer    :: Uuid
  , disease     :: Disease
  , marketShare :: Percentage
  }
