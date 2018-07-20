import System.Random
import Simulation
import Rule

agents = []
rules  = [termination, development]
steps  = 360
seed   = 1
rng    = mkStdGen seed

output = simulate agents rules steps rng

-- TODO: Temp dummy main
main = putStrLn $ unwords $ map (\x -> ".") output
