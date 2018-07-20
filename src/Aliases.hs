module Aliases
  ( Tag
  , Region
  , Mu
  , Disease
  , Probability
  , Percentage
  , MarketShare
  , ProjectId
  , ProducerId
  , Utility
  ) where

import Uuid


type Tag         = String
type Region      = Tag
type Mu          = Double -- Monetary unit
type Disease     = String
type Probability = Double
type Percentage  = Double
type MarketShare = Percentage
type Utility     = Double

-- Semantically useful identifier aliases
type ProjectId   = Uuid
type ProducerId  = Uuid
