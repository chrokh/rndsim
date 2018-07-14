module New.ProducerProps
  ( ProducerProps ( ProducerProps
                  , uuid
                  , projects
                  )
  ) where

import New.Aliases
import New.Randomizer
import New.Project


data ProducerProps = ProducerProps
  { uuid                      :: Uuid
  , tags                      :: [Tag]
  , projects                  :: [Project]
  }

