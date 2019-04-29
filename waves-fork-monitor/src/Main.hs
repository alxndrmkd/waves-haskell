{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM.Lifted
import           Control.Monad.Base
import           Control.Monad.Logger
import           Network.Socket
import           Waves.Network
import           Waves.Utils

main :: IO ()
main = do
  addr <- resolve "18.202.196.73" 6868--"34.237.49.199" 6865
  sock <- open addr
  runStdoutLoggingT $ app sock

type App = LoggingT IO

app :: Socket -> App ()
app sock = do
  nodeEi <-
    forkRunNode sock 'W' "Haskell-FTW" (AppVersion 0 17 0) Nothing mkHandler
  case nodeEi of
    Left err -> liftBase $ print err
    Right nd -> do
      sendMessage nd GetPeers
      readMVar (nodeFinished nd)

resolve :: String -> Int -> IO AddrInfo
resolve host port = do
  let hints = defaultHints {addrSocketType = Stream}
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  return addr

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  return sock

mkHandler :: NodeInfo -> App (NodeHandler App)
mkHandler ni = do
  $(logInfo) $ "Connected to: " <> tshow ni
  return handler
  where
    handler :: (NetworkMessage, NodeSink) -> App ()
    handler (GetPeers, out) = atomically $ writeTChan out (Peers [])
    handler (ScoreChanged _, out) = atomically $ writeTChan out (ScoreChanged 0)
    handler _ = return ()
