module Simulation
  ( simulate
  ) where

import System.Random
import Agent
import Action
import Agent
import Rule
import Actionable


-- Steps through every rule and applies it to every agent at a time and returns
-- the list of all actions performed during the simulation.
--
simulate
  :: [Agent]     -- Agents in the world
  -> [Rule]      -- Rules to apply
  -> Int         -- Number of timesteps to simulate
  -> StdGen      -- RNG
  -> [Action]    -- Returns log of actions that were performed
simulate agents rules steps rng =
  simulate' [] agents (concat $ replicate steps rules) rng


-- TODO: Might need to be tail-recursive for the sake of performance.
-- TODO: The birth of a new agent is a non-specific transformation.
--
simulate'
  :: [Agent]    -- Agents processed
  -> [Agent]    -- Agents remaining
  -> [Rule]     -- Rules remaining
  -> StdGen     -- RNG
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

