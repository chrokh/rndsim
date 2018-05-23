{-# LANGUAGE NamedFieldPuns, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ProductDist
  ( ProductDist
  ) where


import Samplable
import ActivityDist
import Product

type ProductDist = [ActivityDist]

instance Samplable ProductDist Product where
  sample = sampleAll


