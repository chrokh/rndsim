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


data Project = Project
  { uuid  :: Uuid
  , state :: ProjectState
  , drug  :: Drug
  , fund  :: Fund
  --, ... TODO
  }

data ProjectState = Developing | Terminated
