module New.Product
  ( Product
  ) where

import New.Aliases


data Product = Product
  { id         :: Uuid
  , cures      :: [Disease]
  }
