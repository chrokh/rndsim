module New.Events.ConsumptionEvent
  ( ConsumptionEvent ( consumer
                     , disease
                     , units
                     , price
                     , producerFund
                     )
  ) where

import New.Aliases
import New.Uuid

data ConsumptionEvent = ConsumptionEvent
  { consumer     :: Uuid
  , producerFund :: Uuid
  , disease      :: Disease
  , units        :: Int
  , price        :: Mu
  }
