module Primes where

import Control.Monad.State
import GHC.IO (unsafePerformIO)
import Math.NumberTheory.Primes.Testing
import System.Random

type RandGen a = State StdGen a

type Key = (Integer, Integer)

randGen :: IO StdGen
randGen = initStdGen

randGen' :: StdGen
randGen' = mkStdGen 1

-- generator :: Int -> RandGen Integer
-- generator nbits =
--   state $ uniformR (2 ^ (nbits -1) :: Integer, 2 ^ nbits :: Integer)

genPrimes :: Int -> Integer
genPrimes nbits = generateOn isPrime (2 ^ (nbits -1), 2 ^ nbits -1)

genPrimePairs :: Int -> (Integer, Integer)
genPrimePairs nbits = (p, q)
  where
    p = genPrimes (nbits `div` 2)
    q = genQ (nbits `div` 2) p
    n = p * q

genQ :: Int -> Integer -> Integer
genQ nbits p = if q /= p then q else genQ nbits p
  where
    q = genPrimes nbits


generateOn :: (Integer -> Bool) -> (Integer, Integer) -> Integer
generateOn p (lower, upper) = evalState generator $ unsafePerformIO randGen
  where
    generator = do
      i <- genN
      if p i then return i else generator
    genN =
      state $ uniformR (lower, upper)

{-
function inverse(a, n)
    t := 0;     newt := 1
    r := n;     newr := a

    while newr ≠ 0 do
        quotient := r div newr
        (t, newt) := (newt, t − quotient × newt)
        (r, newr) := (newr, r − quotient × newr)

    if r > 1 then
        return "a is not invertible"
    if t < 0 then
        t := t + n

    return t
-}

modInverse :: Integer -> Integer -> Integer
modInverse a n = extract $until check step (0, 1, n, a)
  where
    check (t, newt, r, newr) = newr <= 0
    extract (t, _, r, _)
      --    | r > 1 = Nothing
      | t < 0 = t + n --Just $ t + n
      | otherwise = t --Just t
    step (t, newt, r, newr) =
      let quotation = r `div` newr
       in (newt, t - quotation * newt, newr, r - quotation * newr)

{-
function modular_pow(base, exponent, modulus) is
    if modulus = 1 then
        return 0
    Assert :: (modulus - 1) * (modulus - 1) does not overflow base
    result := 1
    base := base mod modulus
    while exponent > 0 do
        if (exponent mod 2 == 1) then
            result := (result * base) mod modulus
        exponent := exponent >> 1
        base := (base * base) mod modulus
    return result
-}

powMod :: Integer -> Integer -> Integer -> Integer
powMod a e n
  | n == 1 = 0
  | otherwise = extract $ until check step (1, a `rem` n, e)
  where
    check (result, base, exp) = exp <= 0
    extract (result, _, _) = result
    step (result, base, exp) =
      let result' = if (exp `rem` 2) == 1 then (result * base) `rem` n else result
       in (result', (base * base) `rem` n, exp * 2)

genCoprime :: Integer -> Integer
genCoprime phiN = generateOn (\n -> gcd n phiN == 1) (1, phiN -1)