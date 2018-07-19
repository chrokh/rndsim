module Uuid
  ( Uuid
  , uuid
  , ideq
  , Identifiable
  ) where

type Uuid = Int

class Identifiable a where
  uuid :: a -> Uuid

  ideq :: a -> a -> Bool
  ideq x y = (uuid x) == (uuid y)

