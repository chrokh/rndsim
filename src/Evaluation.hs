module Evaluation ( evaluate ) where

import Project
import Agent

evaluate :: Project -> Agent -> Double
evaluate p a = enpv (criteria a) (interpret a p)

enpv :: Criteria -> Project -> Double
enpv c p = 100 -- TODO: Obviously not correct :)
