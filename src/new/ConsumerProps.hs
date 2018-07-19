module New.ConsumerProps
  ( ConsumerProps ( ConsumerProps )
  ) where

import Control.Lens
import Data.Map
import New.Aliases
import New.Uuid
import New.Actionable
import New.Action
import New.Events.ProductionEvent
import New.Project


data ConsumerProps = ConsumerProps
  { _uuid      :: Uuid
  , region     :: Region
  , population :: Int
  , resistant  :: Map Uuid Percentage
  , infected   :: Map Disease Percentage
  , suppliers  :: Map Disease (Map ProjectId Supply)
  }


data Supply = Supply
  { distributor :: Uuid
  , penetration :: MarketShare
  }


instance Actionable ConsumerProps where
  interpret action@(Production event) x =
    let value = Supply { distributor = producer event
                       , penetration = marketShare event }
        key1  = disease event
        key2  = project event
     in x { suppliers = set (ix key1.ix key2) value (suppliers x) }
  interpret _ x = x

