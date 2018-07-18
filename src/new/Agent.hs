module New.Agent
  ( Agent ( Consumer
          , Producer
          )
  ) where

import New.ConsumerProps
import New.ProducerProps
import New.Actionable
import New.Action


data Agent
  = Producer ProducerProps
  | Consumer ConsumerProps


instance Actionable Agent where
  interpret a (Producer x) = Producer (interpret a x)
  interpret a (Consumer x) = Consumer (interpret a x)
