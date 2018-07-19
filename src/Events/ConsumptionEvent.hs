module Events.ConsumptionEvent
  ( ConsumptionEvent ( consumer
                     , disease
                     , units
                     , price
                     , producerFund
                     )
  ) where

import Aliases
import Uuid

data ConsumptionEvent = ConsumptionEvent
  { consumer     :: Uuid
  , producerFund :: Uuid
  , disease      :: Disease
  , units        :: Int
  , price        :: Mu
  }
