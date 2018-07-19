module Actionable
  ( Actionable ( interpret )
  , interpretMany
  ) where

import Data.Function
import Action


-- When an action is interpreted by an agent, a new agent is produced. This new
-- agent is a representation of the same agent after having internalized /
-- interpreted / processed the phenomena / action. In order to divide and
-- conquer the problem, not only agents are receptive to actions. I.e. not only
-- agents are actionable. Also the parts of which the agent is constituated are
-- actionable.
-- TODO: Rewrite above comment since Actionable is now polymorphic.
--
class Actionable a where
  interpret :: Action -> a -> a



-- Multiple actions can be sequentially interpreted by the same agent,
-- producing new representations of the agent along the way.
--
interpretMany :: Actionable a => [Action] -> a -> a
interpretMany actions x = foldl (&) x (map interpret actions)
