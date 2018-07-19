module Fund
  ( Fund ( Fund
         , balance
         )
  , withdraw
  , deposit
  ) where

import Aliases
import Actionable
import Action
import Uuid
import Events.ConsumptionEvent


data Fund = Fund
  { _uuid   :: Uuid
  , balance :: Mu
  }

withdraw :: Double -> Fund -> Fund
withdraw x f = f { balance = (balance f) - x }

deposit :: Double -> Fund -> Fund
deposit x f = f { balance = (balance f) + x }


instance Actionable Fund where

  interpret (Consumption e) x
    | (producerFund e == uuid x) =
      deposit ((fromIntegral $ units e) * (price e)) x
    | otherwise = x

  interpret (Development e) x
    | (payer e == uuid x) = withdraw (cost e) x
    | otherwise = x

  interpret _ x = x


instance Identifiable Fund where
  uuid x = _uuid x

