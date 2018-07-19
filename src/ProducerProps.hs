module ProducerProps
  ( ProducerProps ( ProducerProps
                  , projects
                  , fund
                  )
  ) where

import Aliases
import Randomizer
import Project (Project)
import Fund
import Uuid
import Actionable


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
