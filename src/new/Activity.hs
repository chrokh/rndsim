module New.Activity
  ( Activity (Activity)
  ) where

import New.Aliases


data Activity = Activity

data ActiveActivity = ActiveActivity
  { unrealized :: Activity
  , realized   :: [Step]
  }

data Step = Step
  { cash :: Mu
  , cost :: Mu
  , prob :: Probability
  }

