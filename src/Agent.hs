module Agent (Agent (Agent)) where

import Skills

data Agent = Agent
  { skills    :: Skills
  , capital   :: Double
  , threshold :: Double
  , discount  :: Double
  }

