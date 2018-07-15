module New.Fund
  ( Fund ( Fund
         , balance
         )
  , withdraw
  ) where

import New.Aliases
import New.Actionable
import New.Action
import New.Uuid


data Fund = Fund
  { _uuid   :: Uuid
  , balance :: Mu
  }

withdraw :: Double -> Fund -> Fund
withdraw amnt = id -- TODO



instance Actionable Fund where
  interpret (Development info) fund
    | (payer info == uuid fund) = withdraw (cost info) fund
    | otherwise = fund
  interpret _ x = x


instance Identifiable Fund where
  uuid x = _uuid x
