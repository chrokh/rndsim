module Intervention
  ( Intervention (Intervention, effect, fund, horizon)
  , commit
  , wrap
  , replenish
  ) where


----------------------------------------------
  -- Interventions
----------------------------------------------

import Algebra
import Product
import Fund


data Intervention = Intervention
  { effect    :: Product -> Product
  , fund      :: Fund
  , horizon   :: Int
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
