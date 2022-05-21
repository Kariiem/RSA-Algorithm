module RSA where

import ByteStringUtils
import Primes

type Key = (Integer, Integer)

encrypt :: StrictByteString -> Integer -> Integer -> StrictByteString
encrypt msg e n = fromIntToString $ powMod numMsg e n
  where
    numMsg = fromStringToInt msg

decrypt :: StrictByteString -> Integer -> Integer -> StrictByteString
decrypt cipher d n = fromIntToString $ powMod numCipher d n
  where
    numCipher = fromStringToInt cipher

genKeys :: Integer -> Integer -> (Key, Key)
genKeys p q = ((e, n), (d, n))
  where
    phiN = (p -1) * (q -1)
    -- (p-1)(q-1) = pq - p -q +1
    n = phiN + p + q -1

    e = genCoprime phiN

    d = modInverse e phiN

genKeysWithE :: Integer -> Integer -> Integer -> (Key, Key)
genKeysWithE p q e = ((e, n), (d, n))
  where
    phiN = (p -1) * (q -1)
    -- (p-1)(q-1) = pq - p -q +1
    n = phiN + p + q -1

    -- e = genCoprime phiN

    d = modInverse e phiN

