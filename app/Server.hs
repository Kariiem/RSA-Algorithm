module Server where

import Control.Monad
import qualified ByteStringUtils as C
import Network.Socket
import System.IO

main = createSocket 8080

createSocket port = do
  -- create socket, AF_INET: IPv4, 0: localhost
  sock <- socket AF_INET Stream 0
  -- make socket immediately reusable
  setSocketOption sock ReuseAddr 1
  -- listen on TCP port 'port'.
  bind sock (SockAddrInet port 0)
  -- set a max of 2 queued connections
  listen sock 1
  -- accept a connection
  conn <- accept sock
  -- action
  runConn conn

runConn (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  pubKey <- C.hGetLine hdl
  C.putStrLn pubKey

  readTerminalWriteSocketLooop hdl

  hClose hdl

readTerminalWriteSocketLooop hdl = do
  str <- C.getLine
  if str /= "quit"
    then do
      C.hPutStrLn hdl str
      readTerminalWriteSocketLooop hdl
    else do
      C.hPutStrLn hdl str
      C.putStrLn "quiting connection ..."