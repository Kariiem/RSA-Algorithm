module Primes
  ( isPrime,
    genPrime,
    genQ,
    genCoprime,
    genPrimePairs,
    countBits,
    modInverse,
    powMod,
    integerSquareRoot,
  )
where

import Control.Monad.State
import GHC.IO (unsafePerformIO)
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.Roots
import System.Random

type RandGen a = State StdGen a

randGen :: IO StdGen
randGen = initStdGen

randGen' :: StdGen
randGen' = mkStdGen 1

countBits :: Integer -> Int
countBits integer = 1 + integerLog2' integer

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
    nEbits = if even nbits then nbits else nbits + 1
    halfNbits = nEbits `div` 2
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

-- | @modInverse a n@ gives `b` such that @ ab congruent 1 mod n @
modInverse :: Integer -> Integer -> Integer
modInverse a n = extract $until check step (0, 1, n, a)
  where
    check (t, newt, r, newr) = newr <= 0
    extract (t, _, r, _)
      | t < 0 = t + n
      | otherwise = t
    step (t, newt, r, newr) =
      let quotation = r `div` newr
       in (newt, t - quotation * newt, newr, r - quotation * newr)

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
