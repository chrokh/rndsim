module New.ProducerProps
  ( ProducerProps ( ProducerProps
                  , projects
                  , fund
                  )
  ) where

import New.Aliases
import New.Randomizer
import New.Project
import New.Fund
import New.Uuid


data ProducerProps = ProducerProps
  { _uuid                     :: Uuid
  , tags                      :: [Tag]
  , projects                  :: [Project]
  , fund                      :: Fund
  }
