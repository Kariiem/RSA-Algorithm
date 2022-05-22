module Client where

import qualified ByteStringUtils as C
import Control.Monad
import Data.Char
import Network.Socket
import Primes
import RSA
import System.IO

readP :: Int -> IO Integer
readP nbits = do
  putStrLn "Enter p ? either enter a prime number or (n/N) for auto"
  ans <- map toLower . filter (not . isSpace) <$> getLine
  let p = genPrime nbits
  if ans /= "n"
    then
      let num = read ans
       in if isPrime num
            then return num
            else putStrLn "entered number is not a prime." >> readP nbits
    else return p

readQ :: Int -> Integer -> IO Integer
readQ nbits p = do
  putStrLn "Enter q ? either enter a prime number or (n/N) for auto"
  let q = genQ (2 ^ (nbits -1), p -1) (p + 1, 2 ^ nbits -1)
  ans <- map toLower . filter (not . isSpace) <$> getLine
  if ans /= "n"
    then
      let num = read ans
       in if num /= p && isPrime num
            then return num
            else putStrLn "entered number is not a prime." >> readQ nbits p
    else return q

readE :: Integer -> Integer -> IO Integer
readE p q = do
  putStrLn "Enter e ? either enter a number or (n/N) for auto"
  let phi = (p -1) * (q -1)
  ans <- map toLower . filter (not . isSpace) <$> getLine
  if ans /= "n"
    then
      let num = read ans
       in if 1 < num && num < phi && gcd num phi == 1
            then return num
            else putStrLn "entered number is not coprime with phi of n" >> readE p q
    else return $ genCoprime phi

processInput :: IO (Key, Key)
processInput = do
  putStrLn "enter default key size, to use when no parameters are given"
  defaultSizeOfPrime <- read . filter (not . isSpace) <$> getLine
  putStrLn "autogen parameters ? (y/n)"
  ans <- map toLower . filter (not . isSpace) <$> getLine
  putStrLn ans
  if ans == "y"
    then
      let (p, q) = genPrimePairs defaultSizeOfPrime
          (pub, pr) = genKeys p q
       in do
            return (pub, pr)
    else do
      p <- readP defaultSizeOfPrime
      q <- readQ defaultSizeOfPrime p
      e <- readE p q
      let phi = (p -1) * (q -1)
      let n = phi + p + q -1
      let d = modInverse e phi
      return ((e, n), (d, n))

main = do
  sock <- conSocket 8080
  (public, private) <- processInput
  runConn public private sock

conSocket port = do
  -- create socket
  sock <- socket AF_INET Stream 0
  -- connect tp TCP port 'port'.
  connect sock (SockAddrInet port 0)
  return sock

runConn :: Key -> Key -> Socket -> IO ()
runConn pubKey priKey sock = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  C.hPutStrLn hdl $ C.toByteString pubKey

  readSocketWriteTerminalLooop hdl priKey

  hClose hdl

readSocketWriteTerminalLooop :: Handle -> Key -> IO ()
readSocketWriteTerminalLooop hdl (d, n) = do
  str <- C.hGetLine hdl
  if str /= "quit"
    then do
      C.putStr "received(encrypted): "
      C.putStrLn str
      C.putStr "received(decrypted): "
      C.putStrLn $ decrypt str d n
      readSocketWriteTerminalLooop hdl (d, n)
    else C.putStrLn "quiting connection ..."