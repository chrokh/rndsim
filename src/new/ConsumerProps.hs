module New.ConsumerProps
  ( ConsumerProps (ConsumerProps)
  ) where

import New.Aliases


data ConsumerProps = ConsumerProps
  { id                        :: Uuid
  , region                    :: Region
  -- ...
  }
