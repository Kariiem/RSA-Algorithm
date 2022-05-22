module Main where

import Primes
import TimeIt

main = do
  times_sizes <- mapM (timeItWallTime 1) benchMarkBruteforce
  let timesList = map fst times_sizes
  let sizesList = map snd times_sizes
  writeFile "brutestats.py" $ "timings = " ++ show timesList
  appendFile "brutestats.py" $ "\n\nsizes = " ++ show sizesList

maximumKeySize = 70

bruteforce :: Integer -> (Integer, Integer)
bruteforce n = go $ integerSquareRoot n
  where
    go x = if n `rem` x == 0 then (x, n `div` x) else go (x -1)

benchMarkBruteforce :: [(Int, (Integer, Integer))]
benchMarkBruteforce = zip [10 .. maximumKeySize] [bruteforce (p * q) | k <- [10 .. maximumKeySize], let (p, q) = genPrimePairs k]

-- >>>bruteforce 9
-- (3,3)
