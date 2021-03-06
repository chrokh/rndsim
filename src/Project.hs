module Project
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


import Fund hiding (uuid)
import Drug hiding (uuid)
import Actionable
import Action
import Uuid


data Project = Project
  { _uuid  :: Uuid
  , state  :: ProjectState
  , drug   :: Drug
  , fund   :: Fund
  }

data ProjectState = Alive | Terminated | Failed deriving (Eq)


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


instance Identifiable Project where
  uuid x = _uuid x
