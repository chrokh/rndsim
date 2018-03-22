module Skills
  ( Efficacy
  , Skillset
  , Skills ( costEff
           , cashEff
           , probEff
           , timeEff
           )
  , wrap
  ) where

import Project
import Algebra
import Curve
import Shape

type Efficacy = Double
type Skillset = [Skills]
data Skills = Skills
  { costEff :: Efficacy
  , cashEff :: Efficacy
  , probEff :: Efficacy
  , timeEff :: Efficacy
  }


wrap :: Skillset -> Project -> Project
wrap [] p = p
wrap _ [] = []
wrap (sh:st) (ah:at) = Activity
  -- TODO: Time should not be "permanently" changed!
  { _time = round $ (fromIntegral $ _time ah) * (timeEff sh)
  , _cash = (GoalCurve Con (cashEff sh)) @* (_cash ah)
  , _cost = (GoalCurve Con (costEff sh)) @* (_cost ah)
  , _prob = (GoalCurve Con (probEff sh)) @* (_prob ah)
  } : wrap st at
