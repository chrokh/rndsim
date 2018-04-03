module Intervention
  ( Fund (Fund, period, size, recipients, remaining)
  , Intervention (Intervention, effect, fund, horizon)
  , commit
  , wrap
  , replenish
  ) where


----------------------------------------------
  -- Interventions
----------------------------------------------

import Algebra
import Product


data Intervention = Intervention
  { effect    :: Product -> Product
  , fund      :: Fund
  , horizon   :: Int
  }

data Fund = Fund
  { period :: Int
  , size  :: Double
  , recipients :: Int
  , remaining :: Double
  }


commit :: Intervention -> Product -> (Intervention, Product)
commit i p
  | commitable i p =
    let p' = wrap i p
        d  = flowDiff p' p
        i' = i { fund = withdraw d (fund i) }
     in (i', p')
  | otherwise = (i, p)

wrap :: Intervention -> Product -> Product
wrap i p
  | (eligible i p) = effect i p
  | otherwise = p

commitable :: Intervention -> Product -> Bool
commitable i p = eligible i p && available i p

eligible :: Intervention -> Product -> Bool
eligible i p = True

available :: Intervention -> Product -> Bool
available i p = (recipients.fund) i > 0 && (remaining.fund) i > costOf i p

costOf :: Intervention -> Product -> Double
costOf i p = flowDiff (wrap i p) p

replenish :: Fund -> Int -> Fund
replenish p t
  | t `mod` (period p) == 0 = p { remaining = size p }
  | otherwise = p

withdraw :: Double -> Fund -> Fund
withdraw amount fnd = fnd
  { recipients = (recipients fnd) - 1
  , remaining  = (remaining fnd) - amount
  }
