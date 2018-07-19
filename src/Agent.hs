module Agent
  ( Agent ( Consumer
          , Producer
          )
  ) where

import ConsumerProps
import ProducerProps
import Actionable
import Action


data Agent
  = Producer ProducerProps
  | Consumer ConsumerProps


instance Actionable Agent where
  interpret a (Producer x) = Producer (interpret a x)
  interpret a (Consumer x) = Consumer (interpret a x)
