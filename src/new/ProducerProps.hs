module New.ProducerProps
  ( ProducerProps ( ProducerProps
                  , projects
                  , fund
                  )
  ) where

import New.Aliases
import New.Randomizer
import New.Project (Project)
import New.Fund
import New.Uuid
import New.Actionable


data ProducerProps = ProducerProps
  { _uuid                     :: Uuid
  , tags                      :: [Tag]
  , projects                  :: [Project]
  , fund                      :: Fund
  }


instance Actionable ProducerProps where
  interpret action props = props
    { projects = map (interpret action) (projects props)
    , fund     = interpret action (fund props)
    }
