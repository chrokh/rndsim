module New.Project
  ( Project ( Project
            , state
            , drug
            )
  , ProjectState (Developing, Terminated) --- TODO TEMP!!!
  ) where


import New.Aliases
import New.Drug


data Project = Project
  { uuid  :: Uuid
  , state :: ProjectState
  , drug  :: Drug
  --, ... TODO
  }

data ProjectState = Developing | Terminated
