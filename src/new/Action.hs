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
  , DevelopmentData ( target
                    , payer
                    , cost
                    )
  ) where

import New.Aliases
import New.Activity
import New.Uuid


data Action
--------------------------------
  = Discovery      DiscoveryData
  | Development    DevelopmentData
  | Failure        Uuid
  | Termination    Uuid
--------------------------------
  | Production     Todo
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
  { name       :: Uuid
  , cures      :: [Disease]
  , activities :: [Activity]
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

