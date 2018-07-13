module New.Action
  ( Action ()
  , interpret
  , interpretMany
  ) where

import New.Agent


data Action
  = Failure
  | Termination
  | AndSoForth


-- When an action is interpreted by an agent, a new agent is produced. This new
-- agent is a representation of the same agent after having internalized /
-- interpreted / processed the phenomena / action.
--
interpret :: Action -> Agent -> Agent
interpret action agent = agent -- TODO: Implement!


-- Multiple actions can be sequentially interpreted by the same agent,
-- producing new representations of the agent along the way.
--
interpretMany :: [Action] -> Agent -> Agent
interpretMany actions agent = (foldl (.) id (map interpret actions)) agent

