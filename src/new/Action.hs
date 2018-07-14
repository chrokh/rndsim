module New.Action
  ( Action ()
  , interpret
  , interpretMany
  ) where

import Data.Function
import New.Aliases
import New.Agent
import New.ProducerProps
import New.Project


data Action
  = Failure
  | Termination Uuid
  | Discovery
  | AndSoForth



-- Multiple actions can be sequentially interpreted by the same agent,
-- producing new representations of the agent along the way.
--
interpretMany :: [Action] -> Agent -> Agent
interpretMany actions agent = foldl (&) agent (map interpret actions)


-- When an action is interpreted by an agent, a new agent is produced. This new
-- agent is a representation of the same agent after having internalized /
-- interpreted / processed the phenomena / action. In order to divide and
-- conquer the problem, not only agents are receptive to actions. I.e. not only
-- agents are actionable. Also the parts of which the agent is constituated are
-- actionable.
--
class Actionable a where
  interpret :: Action -> a -> a


instance Actionable Agent where
  interpret action@(Termination pid) (Producer props) =
    Producer props { projects = map (interpret action) (projects props) }
  interpret _ a = a

instance Actionable Project where
  interpret (Termination pid) proj = proj { state = Terminated }
  interpret _ a = a

