module New.Agent
  ( Agent ( Consumer
          , Producer
          )
  ) where

import New.ConsumerProps
import New.ProducerProps


data Agent
  = Producer ProducerProps
  | Consumer ConsumerProps

