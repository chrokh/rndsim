module Drug
  ( Drug ( cures
         , completed
         , remaining
         , current
         )
  ) where

import Aliases
import Actionable
import Activity
import Action
import Uuid


data Drug = Drug
  { _uuid     :: Uuid
  , cures     :: [Disease]
  , completed :: [Activity]
  , remaining :: [Activity]
  , current   :: Activity
  }


instance Identifiable Drug where
  uuid x = _uuid x


instance Actionable Drug where
  interpret (Development props) drg
    | (target props == uuid drg) =
      drg { current   = nextCurrent drg
          , remaining = nextRemaining drg
          , completed = nextCompleted drg
          }
    | otherwise = drg
  interpret _ a = a

nextCurrent   x = current x -- TODO
nextRemaining x = [] -- TODO
nextCompleted x = [] -- TODO
