module Client where

import Control.Monad
import Data.Binary as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Network.Socket
import Primes
import RSA
import System.IO

main = conSocket 8080

conSocket port = do
  -- create socket
  sock <- socket AF_INET Stream 0
  -- connect tp TCP port 'port'.
  connect sock (SockAddrInet port 0)
  -- action
  runConn sock

runConn :: Socket -> IO ()
runConn sock = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  let pubKey = genPrimes 4096
  C.hPutStrLn hdl . C.pack . show $ pubKey
  readSocketWriteTerminalLooop hdl
  hClose hdl

readSocketWriteTerminalLooop hdl = do
  str <- C.hGetLine hdl
  if str /= "quit"
    then do
      C.putStr "received: "
      C.putStrLn str
      readSocketWriteTerminalLooop hdl
    else C.putStrLn "quiting connection ..."