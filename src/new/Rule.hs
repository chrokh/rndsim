module New.Rule
  ( Rule
  ) where

import New.Randomizer
import New.Agent
import New.Action


type Rule = Agent -> Randomizer -> [Action]


-- Silly example.
-- TODO: Real implementations.
termination :: Rule
termination (Consumer props) rnd = []
termination (Producer props) rnd = []
