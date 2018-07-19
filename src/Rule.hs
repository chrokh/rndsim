module Rule
  ( Rule
  ) where

import Randomizer
import Agent
import Action


type Rule = Agent -> Randomizer -> [Action]


-- Silly example.
-- TODO: Real implementations.
termination :: Rule
termination (Consumer props) rnd = []
termination (Producer props) rnd = []
