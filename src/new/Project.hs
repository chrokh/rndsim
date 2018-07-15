module New.Project
  ( Project ( Project
            , state
            , drug
            , fund
            )
  , ProjectState ( Alive
                 , Terminated
                 , Failed
                 )
  ) where


import New.Aliases
import New.Fund hiding (uuid)
import New.Drug hiding (uuid)
import New.Actionable
import New.Action


data Project = Project
  { uuid  :: Uuid
  , state :: ProjectState
  , drug  :: Drug
  , fund  :: Fund
  --, ... TODO
  }

data ProjectState = Alive | Terminated | Failed


instance Actionable Project where
  --
  interpret action@(Development _) prj =
    prj
      { drug = interpret action (drug prj)
      , fund = interpret action (fund prj)
      }
  --
  interpret (Termination pid) prj
    | (uuid prj == pid) = prj { state = Terminated }
    | otherwise         = prj
  --
  interpret (Failure pid) prj
    | (uuid prj == pid) = prj { state = Failed }
    | otherwise         = prj
  --
  interpret _ prj = prj


