module Action
  ( Action ( Discovery
           , Development
           , Failure
           , Termination
           , Production
           , Consumption
           , Spinoff
           , OffersRequest
           , Offer
           , OfferAccept
           , DemandRenewal
           , FundRenewal
           , Transaction
           )
  , DevelopmentEvent ( DevelopmentEvent
                     , target
                     , payer
                     , cost
                     )
  ) where

import Aliases
import Activity
import Uuid
import Events.ProductionEvent
import Events.ConsumptionEvent


data Action
--------------------------------
  = Discovery      DiscoveryEvent
  | Development    DevelopmentEvent
  | Failure        Uuid
  | Termination    Uuid
--------------------------------
  | Production     ProductionEvent
  | Consumption    ConsumptionEvent
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
  | Transaction    TransactionEvent
--------------------------------

data DiscoveryEvent = DiscoveryEvent
  { name       :: Uuid
  , cures      :: [Disease]
  , activities :: [Activity]
  , discoverer :: Uuid
  }

data DevelopmentEvent = DevelopmentEvent
  { target :: Uuid
  , payer  :: Uuid
  , cost   :: Mu
  }

data TransactionEvent = TransactionEvent
  { buyer  :: Uuid
  , seller :: Uuid
  , price  :: Mu
  }

data Todo = Todo

