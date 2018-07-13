module New.ProducerProps
  ( ProducerProps (ProducerProps)
  ) where

import New.Aliases


data ProducerProps = ProducerProps
  { id                        :: Int
  , tags                      :: [Tag]
  -- ...
  }
