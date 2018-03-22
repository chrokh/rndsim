{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Control.Lens
import Samplable
import Shape
import Curve
import Algebra
import Project
import ProjectDist
import DSL
import Intervention


----------------------------------------------
  -- Interventions
----------------------------------------------

type Intervention = Project -> Project

makeLenses ''Activity

-- Example effects
fdper :: Int -> Double -> Project -> Project
fdper i size = over (ix i) $ over cash ((@=) $ GoalCurve Con size)

pdper :: Int -> Double -> Project -> Project
pdper i size = over (ix i) $ over cash $ (@+) $ GoalCurve Con size

grant :: Int -> Double -> Project -> Project
grant i size p = over (ix i) (over cash $ (@=) $ GoalCurve Con $ max size $ stgCost i p) p


-- Example interventions
iFDPER i size = Intervention
  { effect    = fdper i size
  , horizon   = 3
  , fund      = Fund
    { period     = 12
    , size       = 2000
    , recipients = 3
    , remaining  = 0
    }
  }

iGRANT i size = Intervention
  { effect  = grant i size
  , horizon = 0
  , fund    = Fund
    { period     = 12
    , size       = 100
    , recipients = 99999
    , remaining  = 100
  }
}



----------------------------------------------
  -- Projects
----------------------------------------------

-- Example with DSL

ex1 = [ thru Lin (uni 6 12) (tri 100 150 200) (uni 100 200) (pnt 1)
      , upto Sig (uni 10 24) (uni 100 200) (tri 100 150 200) (pnt 1)
      , draw Exp (uni 3 12) (pnt 10, pnt 20) (uni 10 20, uni 20 30) (tri 10 20 30, pnt 50)
      , hold (uni 12 20)
      ]


-- Example project without DSL

ex2 = [ ActivityDist { timeDist = Uniform 6 12
                     , cashDist = AreaCurveDist Lin (Triangular 100 150 200)
                     , costDist = AreaCurveDist Lin (Uniform 100 200)
                     , probDist = AreaCurveDist Lin (Estimate 1)
                     }
      , ActivityDist { timeDist = Uniform 10 24
                     , cashDist = GoalCurveDist Sig (Uniform 100 200)
                     , costDist = GoalCurveDist Sig (Triangular 100 150 200)
                     , probDist = GoalCurveDist Sig (Estimate 1)
                     }
      , ActivityDist { timeDist = Uniform 3 12
                     , cashDist = PntsCurveDist Exp (Estimate 100) (Estimate 200)
                     , costDist = PntsCurveDist Exp (Uniform 10 20) (Uniform 20 30)
                     , probDist = PntsCurveDist Exp (Triangular 10 20 30) (Estimate 50)
                     }
      , ActivityDist { timeDist = Uniform 12 20
                     , cashDist = IdCurveDist
                     , costDist = IdCurveDist
                     , probDist = IdCurveDist
                     }
      ]




----------------------------------------------
  -- Main
----------------------------------------------

main = putStrLn ("Hello world")

