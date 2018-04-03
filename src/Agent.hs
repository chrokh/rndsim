module Agent
  ( Agent (Agent, capital, skillset, criteria)
  , Criteria (Criteria, threshold, discount)
  , interpret
  ) where

import Skillset
import Product

data Agent = Agent
  { skillset :: Skillset
  , criteria :: Criteria
  , capital  :: Double
  }

data Criteria = Criteria
  { threshold :: Double
  , discount  :: Double
  }


interpret :: Agent -> Product -> Product
interpret a p = wrap (skillset a) p

data Project = Project
  { prod :: Product
  , at   :: Int
  }

affords :: Agent -> Project -> Bool
affords a d = stpCost (at d) (prod d) >= capital a

develop :: Agent -> Project -> (Agent, Project)
develop a p
  | affords a p =
    let a' = a { capital = (capital a - (stpFlow (at p) (prod p))) }
        p' = p { at = at p + 1 }
       in (a', p')
  | otherwise = (a, p)
