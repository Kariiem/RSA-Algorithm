module Main where

import Criterion.Main
import qualified Data.ByteString.Char8 as C
import Primes
import RSA

testString :: C.ByteString
testString = "Because GHC optimises aggressively when compiling with -O, it is potentially easy to write innocent-looking benchmark code that will only be evaluated once, for which all but the first iteration of the timing loop will be timing the cost of doing nothing.To work around this, we provide two functions for benchmarking pure code."

main :: IO ()
main = return ()

rsaEncryptionVsTime :: C.ByteString -> (Integer, Integer) -> C.ByteString
rsaEncryptionVsTime str (e, n) = encrypt str e n

