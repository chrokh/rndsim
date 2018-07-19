module New.ConsumerProps
  ( ConsumerProps ( ConsumerProps )
  ) where

import Control.Lens
import Data.Map
import New.Aliases
import New.Uuid
import New.Actionable
import New.Action
import New.Events.ProductionEvent as ProductionEvent
import New.Events.ConsumptionEvent as ConsumptionEvent
import New.Project
import New.Uuid


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


instance Identifiable ConsumerProps where
  uuid x = _uuid x


instance Actionable ConsumerProps where

  interpret (Production event) x =
    let value = Supply { distributor = ProductionEvent.producer event
                       , penetration = marketShare event }
        key1  = ProductionEvent.disease event
        key2  = ProductionEvent.project event
     in x { suppliers = set (ix key1.ix key2) value (suppliers x) }

  interpret (Consumption e) x
    | (consumer e == uuid x) =
      let f :: Percentage -> Percentage
          f y = (unitsInfected - unitsTreated) / totalUnits where
            totalUnits    = fromIntegral (population x)
            unitsInfected = y / totalUnits
            unitsTreated  = fromIntegral (units e)
       in x { infected = over (ix (ConsumptionEvent.disease e)) f (infected x) }
    | otherwise = x

  interpret _ x = x

