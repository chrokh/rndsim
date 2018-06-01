module Fund
  ( Fund ( Fund
         , period
         , size
         , recipients
         , remaining
         )
  , replenish
  , withdraw
  ) where


data Fund = Fund
  { period :: Int
  , size  :: Double
  , recipients :: Int
  , remaining :: Double
  }


replenish :: Fund -> Int -> Fund
replenish p t
  | t `mod` (period p) == 0 = p { remaining = size p }
  | otherwise = p

withdraw :: Double -> Fund -> Fund
withdraw amount fnd = fnd
  { recipients = (recipients fnd) - 1
  , remaining  = (remaining fnd) - amount
  }
