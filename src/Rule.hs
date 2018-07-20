module Rule
  ( Rule
  ) where

import Randomizer
import Agent
import Action
import Project
import ProducerProps
import Uuid



type Rule = Agent -> Randomizer -> [Action]


-- TODO: Many rules missing

termination :: Rule
termination (Consumer props) rnd = []
termination (Producer props) rnd =
  let isTerminatable p = Alive == state p -- TODO: && at stage-gate
      unacceptable p   = 0 >= (utilityFunction props $ p)
      projs = (filter unacceptable) . (filter isTerminatable) $ projects props
      actions = map (\p -> Termination (uuid p)) projs
   in actions
