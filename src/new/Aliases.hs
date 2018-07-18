module New.Aliases
  ( Tag
  , Region
  , Mu
  , Disease
  , Probability
  , Percentage
  , MarketShare
  , ProjectId
  , ProducerId
  ) where

import New.Uuid


type Tag         = String
type Region      = Tag
type Mu          = Double -- Monetary unit
type Disease     = String
type Probability = Double
type Percentage  = Double
type MarketShare = Percentage

-- Semantically useful identifier aliases
type ProjectId   = Uuid
type ProducerId  = Uuid
