module New.Project
  ( Project ( Project
            , state
            )
  , ProjectState (Developing, Terminated) --- TODO TEMP!!!
  ) where


import New.Aliases
import New.Product


data Project = Project
  { uuid    :: Uuid
  , state   :: ProjectState
  , product :: Product
  --, ... TODO
  }

data ProjectState = Developing | Terminated
