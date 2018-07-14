module New.ProducerProps
  ( ProducerProps ( ProducerProps
                  , uuid
                  , projects
                  , fund
                  )
  ) where

import New.Aliases
import New.Randomizer
import New.Project
import New.Fund


data ProducerProps = ProducerProps
  { uuid                      :: Uuid
  , tags                      :: [Tag]
  , projects                  :: [Project]
  , fund                      :: Fund
  }
