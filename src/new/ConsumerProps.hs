module New.ConsumerProps
  ( ConsumerProps (ConsumerProps)
  ) where

import New.Aliases
import New.Uuid


data ConsumerProps = ConsumerProps
  { _uuid                     :: Uuid
  , region                    :: Region
  -- ...
  }
