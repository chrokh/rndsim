module New.Simulation
  ( simulate
  ) where

import New.Randomizer
import New.Agent
import New.Action
import New.Agent
import New.Rule
import New.Actionable


-- Steps through every rule and applies it to every agent at a time and returns
-- the list of all actions performed during the simulation.
--
simulate
  :: [Agent]     -- Agents in the world
  -> [Rule]      -- Rules to apply
  -> Randomizer  -- RNG
  -> [Action]    -- Log of actions performed
simulate = simulate' []


-- TODO: Might need to be tail-recursive for the sake of performance.
-- TODO: The birth of a new agent is a non-specific transformation.
--
simulate'
  :: [Agent]    -- Agents processed
  -> [Agent]    -- Agents remaining
  -> [Rule]     -- Rules remaining
  -> Randomizer -- RNG
  -> [Action]   -- Log of actions performed
simulate' _ [] [] _ = []
simulate' _ _  [] _ = []
simulate' past [] (rule:rtl) rnd = simulate' [] past rtl rnd
simulate' past (agent:future) (rule:rtl) rnd =
  let
      rnd'    = split rnd
      actions = rule agent (fst rnd')
      agent'  = interpretMany actions agent
      past'   = map (interpretMany actions) past
      future' = map (interpretMany actions) future
   in
     actions ++ simulate' (past' ++ [agent']) (future') (rule:rtl) (snd rnd')

