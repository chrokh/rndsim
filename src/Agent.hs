module Agent
  ( Agent (Agent, skillset, criteria)
  , Criteria (Criteria, capital, threshold, discount)
  , interpret
  ) where

import Skills
import Project

data Agent = Agent
  { skillset :: Skillset
  , criteria :: Criteria
  }

data Criteria = Criteria
  { capital   :: Double
  , threshold :: Double
  , discount  :: Double
  }


interpret :: Agent -> Project -> Project
interpret a p = wrap (skillset a) p

