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
import Project


data Intervention = Intervention
  { effect    :: Project -> Project
  , fund      :: Fund
  , horizon   :: Int
  }

data Fund = Fund
  { period :: Int
  , size  :: Double
  , recipients :: Int
  , remaining :: Double
  }


commit :: Intervention -> Project -> (Intervention, Project)
commit i p
  | commitable i p =
    let p' = wrap i p
        d  = flowDiff p' p
        i' = i { fund = withdraw d (fund i) }
     in (i', p')
  | otherwise = (i, p)

wrap :: Intervention -> Project -> Project
wrap i p
  | (eligible i p) = effect i p
  | otherwise = p

commitable :: Intervention -> Project -> Bool
commitable i p = eligible i p && available i p

eligible :: Intervention -> Project -> Bool
eligible i p = True

available :: Intervention -> Project -> Bool
available i p = (recipients.fund) i > 0 && (remaining.fund) i > costOf i p

costOf :: Intervention -> Project -> Double
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
