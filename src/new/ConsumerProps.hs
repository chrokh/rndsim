module New.ConsumerProps
  ( ConsumerProps ( ConsumerProps )
  ) where

import Control.Lens
import Data.Map
import New.Aliases
import New.Uuid
import New.Actionable
import New.Action
import New.Actions.ProductionInfo
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
  interpret action@(Production info) x =
    let value = Supply { distributor = producer info
                       , penetration = marketShare info }
        key1  = disease info
        key2  = project info
     in x { suppliers = set (ix key1.ix key2) value (suppliers x) }
  interpret _ x = x

