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
import New.Drug
import New.Fund


data Action
--------------------------------
  = Discovery      DiscoveryData
  | Development    DevelopmentData
  | Failure        Uuid
  | Termination    Uuid
--------------------------------
  | Drug           Todo
  | Consumption    Todo
--------------------------------
  | Spinoff        Todo
--------------------------------
  | OffersRequest  Todo
  | Offer          Todo
  | OfferAccept    Todo
--------------------------------
  | DemandRenewal  Todo
--------------------------------
  | FundRenewal    Todo
--------------------------------
  | Transaction    TransactionData
--------------------------------

data DiscoveryData = DiscoveryData
  { discovery  :: Drug
  , discoverer :: Uuid
  }

data DevelopmentData = DevelopmentData
  { target :: Uuid
  , payer  :: Uuid
  , cost   :: Mu
  }

data TransactionData = TransactionData
  { buyer  :: Uuid
  , seller :: Uuid
  , price  :: Mu
  }

data Todo = Todo


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
  interpret action@(Development info) (Producer props) =
    Producer props
      { projects = map (interpret action) (projects props)
      , New.ProducerProps.fund = interpret action (New.ProducerProps.fund props)
      }
  interpret action@(Termination _) (Producer props) =
    Producer props { projects = map (interpret action) (projects props) }
  interpret _ a = a

instance Actionable Project where
  interpret action@(Development _) prj =
    prj
      { drug = interpret action (drug prj)
      , New.Project.fund = interpret action (New.Project.fund prj)
      }
  interpret (Termination _) prj =
    prj { state = Terminated }
  interpret _ a = a

instance Actionable Drug where
  interpret (Development props) drg
    | (target props == New.Drug.uuid drg) =
      drg { current   = nextCurrent drg
          , remaining = nextRemaining drg
          , completed = nextCompleted drg
          }
    | otherwise = drg
  interpret _ a = a

nextCurrent   x = current x -- TODO
nextRemaining x = [] -- TODO
nextCompleted x = [] -- TODO

instance Actionable Fund where
  interpret (Development info) fund
    | (payer info == New.Fund.uuid fund) = withdraw (cost info) fund
    | otherwise = fund
  interpret _ x = x
