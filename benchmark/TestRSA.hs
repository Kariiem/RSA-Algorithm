module Main where

import qualified ByteStringUtils as C
import Control.DeepSeq
import qualified Data.Fixed
import Data.List
import Data.Time
import Primes
import RSA

numPasses = 20

testString :: C.StrictByteString
testString =
  "I'm wondering"

testStringInt :: Integer
testStringInt = C.fromStringToInt testString

calcKeySize :: Integer -> Int
calcKeySize message = 1 + integerLog2' message

timeIt :: (NFData b1, NFData b2) => (b1, b2) -> IO (Double, b1)
timeIt val = do
  start <- getCurrentTime
  let str = rnf val
  stop <- getCurrentTime
  print str
  return (realToFrac (diffUTCTime stop start) / numPasses, fst val)

main :: IO ()
main = do
  times_sizes <- mapM timeIt benchMarks
  let timesList = map fst times_sizes
  let sizesList = map snd times_sizes
  writeFile "data.py" $show timesList
  appendFile "data.py" $show sizesList

runAll :: C.StrictByteString -> Int -> (Bool, C.StrictByteString, C.StrictByteString)
runAll str nbits = (str == plain, cipher, plain)
  where
    (p, q) = genPrimePairs (nbits + 1)
    ((e, n), (d, _)) = genKeys p q

    plain = decrypt cipher d n
    cipher = encrypt str e n

runEncrypt :: C.StrictByteString -> Integer -> Integer -> C.StrictByteString
runEncrypt str p q = encrypt str e n
  where
    ((e, n), _) = genKeys p q

runDecrypt :: C.StrictByteString -> Integer -> Integer -> C.StrictByteString
runDecrypt str p q = decrypt str d n
  where
    (_, (d, n)) = genKeys p q

rsaEncryptionVsTime :: C.StrictByteString -> Int -> C.StrictByteString
rsaEncryptionVsTime str nbits = runEncrypt str p q
  where
    (p, q) = genPrimePairs (nbits + 1)
    ((e, n), (d, _)) = genKeys p q

--benchMarks :: [(Int, Bool)]
benchMarks :: [(Int, [C.StrictByteString])]
benchMarks = zip keySizeList bench
  where
    !smallestKeySize = calcKeySize testStringInt

    !keySizeList = [smallestKeySize + 1 .. 10 * smallestKeySize]

    !primePairs = [genPrimePairs nbits | nbits <- keySizeList]

    bench = [[rsaEncryptionVsTime testString k | i <- [1 .. numPasses]] | k <- keySizeList]
