module New.Fund
  ( Fund ( Fund
         , uuid
         , balance
         )
  , withdraw
  ) where

import New.Aliases


data Fund = Fund
  { uuid    :: Uuid
  , balance :: Mu
  }

withdraw :: Double -> Fund -> Fund
withdraw amnt = id -- TODO
