{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Waves.Network.Transport.Node where

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM.Lifted
import qualified Control.Exception               as E
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString                 as BS
import           Data.Serialize
import           Data.Text
import           Network.Socket                  hiding (recv, recvFrom, send,
                                                  sendTo)
import           Network.Socket.ByteString
import           Waves.Network.Protocol.SerDe
import           Waves.Network.Protocol.Types
import           Waves.Network.Transport.Error
import           Waves.Utils

type NodeSink = TChan NetworkMessage

type NodeHandler m = (NetworkMessage, NodeSink) -> m ()

data Node = Node
  { nodeInfo     :: !NodeInfo
  , nodeSocket   :: !(MVar Socket)
  , nodeRx       :: !ThreadId
  , nodeTx       :: !ThreadId
  , nodeSink     :: !NodeSink
  , nodeFinished :: !(MVar ())
  }

instance Show Node where
  show Node {nodeInfo} = show nodeInfo

-- | Shakes hands and handle messages from socket in separate thread
--   In case of unsuccessful handshake returns error
forkRunNode ::
     (MonadBaseControl IO m, MonadLogger m)
  => Socket
  -> Char
  -> Text
  -> AppVersion
  -> Maybe SockAddr
  -> (NodeInfo -> m (NodeHandler m))
  -> m (Either TransportError Node)
forkRunNode sock chain name ver decAddr mkHandler = do
  maybeNodeInfo <- shakeHands sock handshake
  mapM mkNode maybeNodeInfo
  where
    handshake = Handshake (Data.Text.snoc "waves" chain) ver name 0 decAddr
    mkNode nodeInfo = do
      handler <- mkHandler nodeInfo
      nodeSocket <- newMVar sock
      nodeFinished <- newEmptyMVar
      nodeSink <- atomically newTChan
      nodeRx <-
        runWorker
          ("[" <> tshow nodeInfo <> "-rx]")
          nodeFinished
          (writeLoop nodeSocket nodeSink)
      nodeTx <-
        runWorker
          ("[" <> tshow nodeInfo <> "-tx]")
          nodeFinished
          (readLoop nodeSocket nodeSink handler)
      let cleanup = do
            liftBase (close sock)
            killThread nodeRx
            killThread nodeTx
            putMVar nodeFinished ()
      _ <- mkWeakMVar nodeSocket cleanup
      $(logInfo) $ tshow nodeInfo <> " started"
      return Node {..}

stopNode :: (MonadBase IO m) => Node -> m ()
stopNode Node {..} = putMVar nodeFinished ()

sendMessage :: (MonadBaseControl IO m) => Node -> NetworkMessage -> m ()
sendMessage Node {nodeSink} msg = atomically $ writeTChan nodeSink msg

--------------------------------------------------------------------------------------
runWorker ::
     (MonadBaseControl IO m, MonadLogger m)
  => Text
  -> MVar ()
  -> m a
  -> m ThreadId
runWorker prefix finFlag worker =
  forkFinally worker $ \r -> do
    logFinally prefix r
    putMVar finFlag ()

logFinally :: (MonadLogger m) => Text -> Either E.SomeException a -> m ()
logFinally prefix (Left err)
  | E.displayException err == "thread killed" =
    $(logInfo) $ prefix <> " stopped"
  | otherwise =
    $(logError) $ prefix <> " stopped because of exception: " <> tshow err
logFinally prefix _ =
  $(logError) $ prefix <> " stopped because of unknown reason"

writeLoop ::
     (MonadBaseControl IO m, MonadLogger m) => MVar Socket -> NodeSink -> m ()
writeLoop nodeSocket nodeSink = do
  sinkD <- atomically (dupTChan nodeSink)
  forever $ do
    msg <- atomically $ readTChan sinkD
    wr <- withMVar nodeSocket (`sendMsg` msg)
    case wr of
      Left err -> E.throw err
      Right () -> return ()
  where
    sendMsg sock msg = runExceptT $ writeNetworkMessage sock msg

readLoop ::
     (MonadBase IO m, MonadLogger m)
  => MVar Socket
  -> NodeSink
  -> NodeHandler m
  -> m (Either TransportError ())
readLoop sockVar nodeSink handler = do
  sock <- readMVar sockVar
  forever $ do
    msgEi <- runExceptT $ readNetworkMessage sock
    $(logInfo) $ tshow msgEi
    case msgEi of
      Left err  -> E.throw err
      Right msg -> handler (msg, nodeSink)

--------------------------------------------------------------------------------------
shakeHands ::
     (MonadBase IO m)
  => Socket
  -> Handshake
  -> m (Either TransportError NodeInfo)
shakeHands sock localHS =
  runExceptT $ do
    writeHandshake sock localHS
    remoteHS <- readHandshake sock
    let remoteAppName = hsAppName remoteHS
        localAppName = hsAppName localHS
        appNameValid = remoteAppName == localAppName
    liftEither $
      if appNameValid
        then return (infoFromHandshake remoteHS)
        else let ioError =
                   userError $
                   "Remote app name " ++
                   show remoteAppName ++
                   " doesnot match local: " ++ show localAppName
                 errMsg = "Error while shaking hands"
              in throwError (TransportIOError ioError errMsg)

readHandshake :: MonadBase IO m => Socket -> ExceptT TransportError m Handshake
readHandshake sock = do
  hsBytes <- liftBaseIOE "Can't read handshake" (recv sock 4096)
  case runGet get hsBytes of
    Left err ->
      throwError (TransportProtoError (pack err) "Cant parse handshake")
    Right hs -> return hs

writeHandshake ::
     MonadBase IO m => Socket -> Handshake -> ExceptT TransportError m ()
writeHandshake = socketWrite

--------------------------------------------------------------------------------------
newtype MessageLength =
  MessageLength Int

instance Serialize MessageLength where
  get = MessageLength <$> getInt32
  put = error "get only wrapper"

readNetworkMessageLength ::
     (MonadBase IO m) => Socket -> ExceptT TransportError m Int
readNetworkMessageLength sock = do
  (MessageLength len) <- socketRead sock 4
  liftBase $ print len
  return len

readNetworkMessage ::
     (MonadBase IO m) => Socket -> ExceptT TransportError m NetworkMessage
readNetworkMessage sock = do
  len <- readNetworkMessageLength sock
  socketRead sock len

writeNetworkMessage ::
     MonadBase IO m => Socket -> NetworkMessage -> ExceptT TransportError m ()
writeNetworkMessage sock msg =
  let bytes = runPut (put msg)
      len = runPut (putInt32 $ BS.length bytes)
   in socketWriteBytes sock (BS.concat [len, bytes])

--------------------------------------------------------------------------------------
socketWrite ::
     (MonadBase IO m, Serialize a) => Socket -> a -> ExceptT TransportError m ()
socketWrite sock msg = socketWriteBytes sock (runPut $ put msg)

socketWriteBytes ::
     (MonadBase IO m) => Socket -> BS.ByteString -> ExceptT TransportError m ()
socketWriteBytes sock bytes = liftBaseIOE errMsg $ sendAll sock bytes
  where
    errMsg = "Cant write " <> tshow (BS.length bytes) <> " to socket"

socketRead ::
     (MonadBase IO m, Serialize a)
  => Socket
  -> Int
  -> ExceptT TransportError m a
socketRead sock len = do
  bytes <- socketReadBytes sock len
  liftBase $ print (BS.length bytes)
  case runGet get bytes of
    Left err -> throwError (TransportProtoError (pack err) "Cant parse message")
    Right rs -> return rs

socketReadBytes ::
     (MonadBase IO m) => Socket -> Int -> ExceptT TransportError m BS.ByteString
socketReadBytes sock len = liftBaseIOE errMsg $ recv sock len
  where
    errMsg = "Cant read " <> tshow len <> " bytes from socket"

liftBaseIOE :: (MonadBase IO m) => Text -> IO a -> ExceptT TransportError m a
liftBaseIOE msg action = do
  ei <- liftBase $ (Right <$> action) `E.catch` (return . wrapError)
  liftEither ei
  where
    wrapError e = Left (TransportIOError e msg)
