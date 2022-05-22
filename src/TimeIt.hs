module TimeIt (deepseq, timeItCPUTime, timeItWallTime) where

import Control.DeepSeq
import Criterion.Measurement

timeIt :: (NFData b1, NFData b2, Real a) => IO a -> Double -> (b1, b2) -> IO (Double, b1)
timeIt timer npasses val = do
  initializeTime
  start <- timer
  end <- val `deepseq` timer
  -- print val
  return (realToFrac (end - start) / npasses, fst val)

timeItCPUTime :: (NFData b1, NFData b2) => Double -> (b1, b2) -> IO (Double, b1)
timeItCPUTime = timeIt getCPUTime

timeItWallTime :: (NFData b1, NFData b2) => Double -> (b1, b2) -> IO (Double, b1)
timeItWallTime = timeIt getTime