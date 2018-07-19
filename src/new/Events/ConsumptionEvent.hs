module New.Events.ConsumptionEvent
  ( ConsumptionEvent ( consumer
                     , project
                     , producer
                     , disease
                     , units
                     )
  ) where

import New.Aliases
import New.Uuid

data ConsumptionEvent = ConsumptionEvent
  { consumer :: Uuid
  , project  :: Uuid
  , producer :: Uuid
  , disease  :: Disease
  , units    :: Int
  }
