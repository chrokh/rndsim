module New.Project
  ( Project ( Project
            , state
            )
  , ProjectState (Developing, Terminated) --- TODO TEMP!!!
  ) where


import New.Aliases


data Project = Project
  { uuid :: Uuid
  , state :: ProjectState
  --, ... TODO
  }

data ProjectState = Developing | Terminated
