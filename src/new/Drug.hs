module New.Drug
  ( Drug ( uuid
         , cures
         , completed
         , remaining
         , current
         )
  ) where

import New.Aliases


data Drug = Drug
  { uuid      :: Uuid
  , cures     :: [Disease]
  , completed :: [Activity]
  , remaining :: [Activity]
  , current   :: Activity
  }


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
