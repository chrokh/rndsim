module Evaluation ( evaluate ) where

import Product
import Agent

evaluate :: Product -> Agent -> Double
evaluate p a = enpv (criteria a) (interpret a p)

enpv :: Criteria -> Product -> Double
enpv c p = 100 -- TODO: Obviously not correct :)
