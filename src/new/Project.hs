module New.Project
  ( Project ( Project
            , state
            , drug
            , fund
            )
  , ProjectState (Developing, Terminated) --- TODO TEMP!!!
  ) where


import New.Aliases
import New.Drug
import New.Fund
import New.Actionable
import New.Action
import New.Drug


data Project = Project
  { uuid  :: Uuid
  , state :: ProjectState
  , drug  :: Drug
  , fund  :: Fund
  --, ... TODO
  }

data ProjectState = Developing | Terminated


instance Actionable Project where
  interpret action@(Development _) prj =
    prj
      { drug = interpret action (drug prj)
      , fund = interpret action (fund prj)
      }
  interpret (Termination _) prj =
    prj { state = Terminated }
  interpret _ a = a
