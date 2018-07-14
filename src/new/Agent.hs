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
  interpret action@(Development info) (Producer props) =
    Producer props
      { projects = map (interpret action) (projects props)
      , fund = interpret action (fund props)
      }
  interpret action@(Termination _) (Producer props) =
    Producer props { projects = map (interpret action) (projects props) }
  interpret _ a = a
