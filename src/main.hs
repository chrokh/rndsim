{-# LANGUAGE NamedFieldPuns, TemplateHaskell, NoMonomorphismRestriction #-}

import Control.Lens
import Data.Maybe
import Control.Arrow



--
-- STAGES & STEPS
--

-- To describe a project we use stages.
-- A stage has four properties. All
-- properties are assumed to be uniformly
-- distributed across the time of the stage.

data Stage = Stage { _stageCash :: Double
                   , _stageCost :: Double
                   , _stageProb :: Double
                   , _stageTime :: Int
                   } deriving (Show)

-- To perform computation over stages we
-- convert stages to steps.

data Step = Step { _stepCash :: Double
                 , _stepCost :: Double
                 , _stepProb :: Double
                 } deriving (Show)


-- Let's generate lenses...

makeLenses ''Stage
makeLenses ''Step


-- The time of any given step is always 1.
-- So, when constructing steps from a stage
-- we 'spread' all its properties across its
-- time dimension.
expandStage :: Stage -> [Step]
expandStage x =
  let time = (fromIntegral (_stageTime x))
      _stepCash = (_stageCash x) / time
      _stepCost = (_stageCost x) / time
      _stepProb = (_stageProb x) ** (1 / time)
      step = Step { _stepCash, _stepCost, _stepProb }
   in replicate (_stageTime x) step


-- Since a project consists of multiple
-- stages we can of course also convert
-- a list of stages to a list of lists
-- of steps.
expandStages :: [Stage] -> [[Step]]
expandStages xs = map expandStage xs



--
-- INTERVENTIONS
--

-- We'll be defining quite a few interventions
-- so let's set up some function aliases for
-- the leneses generated from the records.

cash = stageCash
cost = stageCost
time = stageTime
prob = stageProb


-- And then a few helper functions to make
-- the lens interaction a bit more pleasant.

gett l i stgs   = fromMaybe 0 $ preview (ix i . l) stgs
sett l i f stgs = over (ix i . l) f stgs


-- Now let's define a few interventions...

fdper :: Double -> Int -> [Stage] -> [Stage]
fdper x i stages = sett cash i (const x) stages

pdper :: Double -> Int -> [Stage] -> [Stage]
pdper x i stages = sett cash i (+x) stages

costCappedGrant :: Double -> Int -> [Stage] -> [Stage]
costCappedGrant x i stages = sett cash i (+x) >>> sett cash i (min $ gett cost i stages) $ stages


-- Of course we could use the raw lenses to
-- construct interventions (below) but the
-- interventions constructed using our less
-- expressive DSL (above) are clearly
-- more readable.

pdper' :: Double -> [Stage] -> [Stage]
pdper' size stages = over (ix 5.stageCash) (size +) stages

fdper' :: Double -> [Stage] -> [Stage]
fdper' size stages = set (ix 5.stageCash) size stages

costCappedGrant' :: Double -> Int -> [Stage] -> [Stage]
costCappedGrant' size i stages =
  over (ix i.stageCash) (min $ fromMaybe 0 $ preview (ix i.stageCost) stages)
  . over (ix i.stageCash) (size +)
  $ stages




--
-- EXAMPLES
--

stage = Stage { _stageCost = 100
              , _stageCash = 50
              , _stageProb = 0.5
              , _stageTime = 10
              }
proj = [stage, stage, stage]



--
-- MAIN
--

main = putStrLn ("hello world")

