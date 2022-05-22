-- Chosen Ciphertext Attack module
module CCA where

import qualified ByteStringUtils as C
import Primes
import RSA

main :: IO ()
main = choseCiphertextAttack

choseCiphertextAttack = do
  C.putStrLn "Enter a message, this is the one Alice is sending to Bob."
  plaintext <- C.getLine
  let (p, q) = genPrimePairs $ countBits $ C.fromStringToInt plaintext
  let ((e, n), (d, _)) = genKeys p q
  let ciphertext = encrypt plaintext e n
  C.putStrLn ciphertext
  C.putStrLn "Now Eve intercepts the calculated cipher text."
  let r = genCoprime n
  let inverseR = modInverse r n
  putStrLn "Eve sends C' to Bob, where C' = C x r^e mod n "
  putStr "r  = " >> print r
  let ciphertext' = C.fromIntToString $ (C.fromStringToInt ciphertext * powMod r e n) `rem` n
  putStr "C' = " >> C.putStrLn ciphertext'
  putStrLn
    "Bob decrypts C' as \nY = (C')^d\n  = (C x r^e)^d mod n\n  = (M^e x r^e)^d mod n\n\
    \  = (M x r) mod n, since e,d are inverse mod n,\n then sends it back to Eve"
  let y = decrypt ciphertext' d n
  putStr "Y  = " >> C.putStrLn y
  let original = C.fromIntToString $ (inverseR * C.fromStringToInt y) `rem` n
  putStr "original text = " >> C.putStrLn original
  if original == plaintext
    then putStrLn "Voila, now Eve has the original plaintext"
    else putStrLn "this chosen ciphertext attack should have succeded"
