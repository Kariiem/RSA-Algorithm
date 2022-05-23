module Main where

import qualified ByteStringUtils as C
import Data.List
import Primes
import RSA
import TimeIt

numPasses = 1

maxKeysSize = 2048

testString :: C.StrictByteString
testString =
  "I'm wondering"

testStringInt :: Integer
testStringInt = C.fromStringToInt testString

main :: IO ()
main = do
  times_sizes <- mapM (timeItWallTime numPasses) benchMarks
  let timesList = map fst times_sizes
  let sizesList = map snd times_sizes
  writeFile "stats.py" $ "timings = " ++ show timesList
  appendFile "stats.py" $ "\n\nsizes = " ++ show sizesList

rsaEncryptionVsTime :: C.StrictByteString -> Int -> C.StrictByteString
rsaEncryptionVsTime str nbits = runEncrypt str p q
  where
    (p, q) = genPrimePairs (nbits + 1)
    ((e, n), (d, _)) = genKeys p q

--benchMarks :: [(Int, Bool)]
benchMarks :: [(Int, [C.StrictByteString])]
benchMarks = primePairs `deepseq` zip keySizeList bench
  where
    !smallestKeySize = countBits testStringInt

    !keySizeList = [smallestKeySize .. maxKeysSize]

    primePairs = [[genPrimePairs nbits | i <- [1 .. numPasses]] | nbits <- keySizeList]

    bench =
      [ [encrypt testString e n | (p, q) <- primePairs !! (k - smallestKeySize), let ((e, n), (d, _)) = genKeys p q]
        | k <- keySizeList
      ]

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
