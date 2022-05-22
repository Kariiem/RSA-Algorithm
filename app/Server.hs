module Server where

import qualified ByteStringUtils as C
import Control.Monad
import Network.Socket
import RSA
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
  let (e, n) = (read $ C.unpack pubKey) :: Key

  readTerminalWriteSocketLooop hdl (e, n)

  hClose hdl

readTerminalWriteSocketLooop :: Handle -> Key -> IO ()
readTerminalWriteSocketLooop hdl (e, n) = do
  str <- C.getLine
  if str /= "quit"
    then do
      let plainTexts = C.processPlainText str n
      let ciphers = map (\p -> encrypt p e n) plainTexts
      mapM_ (C.hPutStrLn hdl) ciphers
      readTerminalWriteSocketLooop hdl (e, n)
    else do
      C.hPutStrLn hdl str
      C.putStrLn "quiting connection ..."
