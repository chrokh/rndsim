module New.Drug
  ( Drug ( uuid
         , cures
         , completed
         , remaining
         , current
         )
  ) where

import New.Aliases
import New.Actionable
import New.Activity
import New.Action


data Drug = Drug
  { uuid      :: Uuid
  , cures     :: [Disease]
  , completed :: [Activity]
  , remaining :: [Activity]
  , current   :: Activity
  }



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
