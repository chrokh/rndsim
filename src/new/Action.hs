module New.Action
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
  , DevelopmentInfo ( target
                    , payer
                    , cost
                    )
  ) where

import New.Aliases
import New.Activity
import New.Uuid
import New.Actions.ProductionInfo


data Action
--------------------------------
  = Discovery      DiscoveryInfo
  | Development    DevelopmentInfo
  | Failure        Uuid
  | Termination    Uuid
--------------------------------
  | Production     ProductionInfo
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
  | Transaction    TransactionInfo
--------------------------------

data DiscoveryInfo = DiscoveryInfo
  { name       :: Uuid
  , cures      :: [Disease]
  , activities :: [Activity]
  , discoverer :: Uuid
  }

data DevelopmentInfo = DevelopmentInfo
  { target :: Uuid
  , payer  :: Uuid
  , cost   :: Mu
  }

data TransactionInfo = TransactionInfo
  { buyer  :: Uuid
  , seller :: Uuid
  , price  :: Mu
  }

data Todo = Todo

