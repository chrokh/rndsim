module Agent
  ( Agent (Agent, capital, skillset, criteria)
  , Criteria (Criteria, threshold, discount)
  , interpret
  ) where

import Skillset
import Project

data Agent = Agent
  { skillset :: Skillset
  , criteria :: Criteria
  , capital  :: Double
  }

data Criteria = Criteria
  { threshold :: Double
  , discount  :: Double
  }


interpret :: Agent -> Project -> Project
interpret a p = wrap (skillset a) p

data Developable = Developable
  { proj :: Project
  , at   :: Int
  }

affords :: Agent -> Developable -> Bool
affords a d = stpCost (at d) (proj d) >= capital a

develop :: Agent -> Developable -> (Agent, Developable)
develop a d
  | affords a d =
    let a' = a { capital = (capital a - (stpFlow (at d) (proj d))) }
        d' = d { at = at d + 1 }
       in (a', d')
  | otherwise = (a, d)
