module RSA where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char
import Data.STRef.Strict
import Primes

type Key = (Integer, Integer)

fromStringToInt :: C.ByteString -> Integer
fromStringToInt = C.foldl' step 0
  where
    step :: Integer -> Char -> Integer
    step acc ch = acc * 256 + fromIntegral (ord ch)

fromIntToString :: Integer -> C.ByteString
fromIntToString n =
  LC.toStrict $ B.toLazyByteString $ snd $ until (\(num, _) -> num == 0) step (n, mempty)
  where
    step (num, buildStr) =
      let (num_quot, num_rem) = num `quotRem` 256
          ch = C.singleton $ chr $ fromIntegral num_rem
       in (num_quot, B.byteString ch <> buildStr)

encrypt :: C.ByteString -> Integer -> Integer -> C.ByteString
encrypt msg e n = fromIntToString $ powMod numMsg e n
  where
    numMsg = fromStringToInt msg

decrypt :: C.ByteString -> Integer -> Integer -> C.ByteString
decrypt cipher d n = fromIntToString $ powMod numCipher d n
  where
    numCipher = fromStringToInt cipher

genKeys :: Integer -> Integer -> Integer -> (Key, Key)
genKeys p q e = ((e, n), (d, n))
  where
    phiN = (p -1) * (q -1)
    -- (p-1)(q-1) = pq - p -q +1
    n = phiN + p + q -1

    d = modInverse e phiN
