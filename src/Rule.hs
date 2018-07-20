module Rule
  ( Rule
  , termination
  , development
  ) where

import System.Random
import Agent
import Action
import Project
import ProducerProps
import Uuid



type Rule = Agent -> StdGen -> [Action]


-- TODO: Many rules missing

termination :: Rule
termination (Consumer props) rnd = []
termination (Producer props) rnd =
  let isTerminatable p = Alive == state p -- TODO: && at stage-gate
      unacceptable p   = 0 >= (utilityFunction props $ p)
      projs = (filter unacceptable) . (filter isTerminatable) $ projects props
      actions = map (\p -> Termination (uuid p)) projs
   in actions

-- TODO: This is an incorrect test-implementation
development :: Rule
development (Consumer props) rng = []
development (Producer props) rng =
  -- TODO:
  --  If in development
  --  Find out cost
  --  If affords
  map makeDevelopmentAction (projects props)

makeDevelopmentAction p =
  (Development DevelopmentEvent
    { target = uuid p
    , payer  = uuid (Project.fund p)
    , cost   = 100 })
