module Primes
  ( modInverse,
    powMod,
    genCoprime,
    genPrimePairs,
    integerLog2',
  )
where

import Control.Monad.State
import GHC.IO (unsafePerformIO)
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Primes.Testing
import System.Random

type RandGen a = State StdGen a

randGen :: IO StdGen
randGen = initStdGen

randGen' :: StdGen
randGen' = mkStdGen 1

-- | Takes boolean test `p`, range in the form of @ (lower,upper) @,
--  generates random number statisfying `p` in the range @(lower,upper)@
generateOn :: (Integer -> Bool) -> (Integer, Integer) -> Integer
generateOn p (lower, upper) = evalState generator $ unsafePerformIO randGen
  where
    generator = do
      i <- genN
      if p i then return i else generator
    genN =
      state $ uniformR (lower, upper)

-- | Takes an Int `nbits`,generates random @ prime @ with the given `nbits`
genPrime :: Int -> Integer
genPrime nbits = generateOn isPrime (2 ^ (nbits -1), 2 ^ nbits -1)

-- | generates a pair of random primes @ (p,q) @ both having sizes equal to @ nbits `div`2 @
genPrimePairs :: Int -> (Integer, Integer)
genPrimePairs nbits = (p, q)
  where
    halfNbits = nbits `div` 2
    p = genPrime halfNbits
    -- q = head $ filter (\n -> (n /= p) && isPrime n) [2 ^ (halfNbits -1) .. 2 ^ halfNbits -1]
    q = genQ (2 ^ (halfNbits -1), p -1) (p + 1, 2 ^ halfNbits -1)

-- | Takes an Integer `phiN`,generates a random number `e` coprime with `phiN`, and @ 1 < e < phiN @
genCoprime :: Integer -> Integer
genCoprime phiN = generateOn (\n -> gcd n phiN == 1) (1, phiN -1)

genQ :: (Integer, Integer) -> (Integer, Integer) -> Integer
genQ (ll, lu) (rl, ru) = evalState generator $ unsafePerformIO randGen
  where
    generator = do
      i <- genNL
      j <- genNR
      if isPrime i
        then return i
        else
          if isPrime j
            then return j
            else generator
    genNL =
      state $ uniformR (ll, lu)
    genNR =
      state $ uniformR (rl, ru)

{-function inverse(a, n)
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

-- | @modInverse a n@ gives `b` such that @ ab congruent 1 mod n @
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

{-function modular_pow(base, exponent, modulus) is
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
      let result' =
            if (exp `rem` 2) == 1
              then (result * base) `rem` n
              else result
       in (result', (base * base) `rem` n, exp `div` 2)
