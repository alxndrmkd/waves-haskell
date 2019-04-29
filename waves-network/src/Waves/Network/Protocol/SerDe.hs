{-# LANGUAGE MultiWayIf #-}

module Waves.Network.Protocol.SerDe where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString              as BS
import           Data.Serialize               hiding (getInt8, putInt8)
import           Data.Text
import           Data.Text.Encoding
import           Data.Word
import           Network.Socket
import           Waves.Crypto
import           Waves.Data
import           Waves.Network.Protocol.Types

------------------------------------------------------------------------
instance Serialize Handshake where
  get = do
    app <- getInt8 >>= (fmap decodeUtf8 . getByteString)
    ver <- get
    name <- getInt8 >>= (fmap decodeUtf8 . getByteString)
    nonce <- getInt64
    address <- getAddress
    skip 8
    return $ Handshake app ver name nonce address
  put (Handshake app ver name nonce address) = do
    putInt8 (Data.Text.length app)
    putByteString (encodeUtf8 app)
    put ver
    putInt8 (Data.Text.length name)
    putByteString (encodeUtf8 name)
    putInt64 nonce
    putAddress address
    putInt64 nonce

instance Serialize AppVersion where
  get = AppVersion <$> getInt32 <*> getInt32 <*> getInt32
  put (AppVersion maj min patch) = do
    putInt32 maj
    putInt32 min
    putInt32 patch

getAddress :: Get (Maybe SockAddr)
getAddress = do
  len <- getInt32
  if | len == 8 ->
       do host <- getWord32host
          port <- fromIntegral <$> getInt32be
          return $ Just (SockAddrInet port host)
     | len == 0 -> return Nothing
     | otherwise -> fail "IPv6 not supported"

putAddress :: Putter (Maybe SockAddr)
putAddress (Just (SockAddrInet port host)) = do
  putInt32 8
  putWord32host host
  putInt32 (fromIntegral port)
putAddress Nothing = putInt32 0

----------------------------------------------------------------------
_MGCK_ :: Word32
_MGCK_ = 0x12345678

instance Serialize NetworkMessage where
  get = getMessage
  put = putMessage

getMessage :: Get NetworkMessage
getMessage = do
  skip 4
  cid <- get
  len <- getInt32
  chks <-
    if len == 0
      then return (Checksum 0)
      else get
  pl <-
    if len == 0
      then return BS.empty
      else getByteString len
  let checksumValid = (calculateChecksum pl) == chks
  if checksumValid
    then case runGet (getMessage' cid) pl of
           Left err  -> error err
           Right msg -> return msg
    else error $
         "Invalid checksum: ID -  " ++ show cid ++ " M: " ++ show (encode58 pl)

getMessage' :: ContentID -> Get NetworkMessage
getMessage' GetPeersID = return GetPeers
getMessage' PeersID = do
  cnt <- getInt32
  ps <- replicateM cnt getPeer
  return (Peers ps)
  where
    getPeer = SockAddrInet <$> (fromIntegral <$> getInt32) <*> getWord32be
getMessage' GetSignaturesID = do
  cnt <- getInt32
  GetSignatures <$> replicateM cnt get
getMessage' SignaturesID = do
  cnt <- getInt32
  Signatures <$> replicateM cnt get
getMessage' GetBlockID = GetBlock <$> get
getMessage' BlockID = BlockMessage <$> get
getMessage' ScoreID = do
  n <- remaining
  score <- bs2i <$> getByteString (fromIntegral n)
  return $ ScoreChanged score
  where
    bs2i = BS.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0
getMessage' TransactionID = TransactionMessage <$> get
getMessage' CheckpointID = do
  cnt <- getInt32
  chks <- replicateM cnt get
  return $ CheckpointMessage chks
getMessage' MicroblockInvID = MicroblockInv <$> get <*> get <*> get <*> get
getMessage' MicroblockRequestID = MicroblockRequest <$> get
getMessage' MicroblockResponseID = MicroblockResponse <$> get

putMessage :: Putter NetworkMessage
putMessage m = putMessageFrame (contentIdOfMessage m) m

contentIdOfMessage :: NetworkMessage -> ContentID
contentIdOfMessage GetPeers                = GetPeersID
contentIdOfMessage (Peers _)               = PeersID
contentIdOfMessage (GetSignatures _)       = GetSignaturesID
contentIdOfMessage (Signatures _)          = SignaturesID
contentIdOfMessage (GetBlock _)            = GetBlockID
contentIdOfMessage (BlockMessage _)        = BlockID
contentIdOfMessage (CheckpointMessage _)   = CheckpointID
contentIdOfMessage (ScoreChanged _)        = ScoreID
contentIdOfMessage (TransactionMessage _)  = TransactionID
contentIdOfMessage (MicroblockRequest _)   = MicroblockRequestID
contentIdOfMessage (MicroblockInv _ _ _ _) = MicroblockInvID
contentIdOfMessage (MicroblockResponse _)  = MicroblockResponseID

putMessageFrame :: ContentID -> NetworkMessage -> Put
putMessageFrame id msg =
  let payload = runPut (putMessagePayload msg)
      payloadLen = BS.length payload
   in if payloadLen == 0
        then putEmpty id
        else putNonEmpty id payloadLen payload

putEmpty :: ContentID -> Put
putEmpty cid = put _MGCK_ >> put cid >> putInt32 0

putNonEmpty :: ContentID -> Int -> BS.ByteString -> Put
putNonEmpty cid plen p = do
  put _MGCK_
  put cid
  putInt32 plen
  put chks
  putByteString p
  where
    chks = calculateChecksum p

putMessagePayload :: Putter NetworkMessage
putMessagePayload GetPeers = return ()
putMessagePayload (Peers ps) = putInt32 (Prelude.length ps) >> mapM_ putPeer ps
  where
    putPeer (SockAddrInet port host) = do
      putInt32 8
      putWord32host host
      putInt32 (fromIntegral port)
putMessagePayload (GetSignatures ss) =
  putInt32 (Prelude.length ss) >> mapM_ put ss
putMessagePayload (Signatures ss) = putInt32 (Prelude.length ss) >> mapM_ put ss
putMessagePayload (GetBlock sig) = put sig
putMessagePayload (ScoreChanged sc) = putByteString (i2bs sc)
putMessagePayload (BlockMessage bl) = put bl
putMessagePayload (TransactionMessage tx) = put tx
putMessagePayload (CheckpointMessage chks) =
  putInt32 (Prelude.length chks) >> mapM_ put chks
putMessagePayload (MicroblockInv pk s1 s2 s3) =
  put pk >> put s1 >> put s2 >> put s3
putMessagePayload (MicroblockRequest s) = put s
putMessagePayload (MicroblockResponse mb) = put mb

i2bs :: Integer -> BS.ByteString
i2bs =
  BS.reverse .
  BS.unfoldr
    (\i' ->
       if i' == 0
         then Nothing
         else Just (fromIntegral i', i' `shiftR` 8))

instance Serialize Checkpoint where
  get = Checkpoint <$> getInt32 <*> get
  put (Checkpoint h s) = putInt32 h >> put s

-----------------------------------------------------------------------
instance Serialize Magic where
  get = Magic <$> getWord32be
  put (Magic w) = putWord32be w

instance Serialize Checksum where
  get = Checksum <$> getWord32be
  put (Checksum w) = putWord32be w

instance Serialize ContentID where
  put GetPeersID           = putWord8 1
  put PeersID              = putWord8 2
  put GetSignaturesID      = putWord8 20
  put SignaturesID         = putWord8 21
  put GetBlockID           = putWord8 22
  put BlockID              = putWord8 23
  put ScoreID              = putWord8 24
  put TransactionID        = putWord8 25
  put CheckpointID         = putWord8 100
  put MicroblockInvID      = putWord8 26
  put MicroblockRequestID  = putWord8 27
  put MicroblockResponseID = putWord8 28
  get = do
    byte <- getWord8
    if | byte == 1 -> return GetPeersID
       | byte == 2 -> return PeersID
       | byte == 20 -> return GetSignaturesID
       | byte == 21 -> return SignaturesID
       | byte == 22 -> return GetBlockID
       | byte == 23 -> return BlockID
       | byte == 24 -> return ScoreID
       | byte == 25 -> return TransactionID
       | byte == 26 -> return MicroblockInvID
       | byte == 27 -> return MicroblockRequestID
       | byte == 28 -> return MicroblockResponseID
       | byte == 100 -> return CheckpointID
       | otherwise -> fail ("Unknown ContentID - " ++ show byte)

-----------------------------------------------------------------------
getInt64 :: Get Int
getInt64 = fromIntegral <$> getInt64be

putInt64 :: Putter Int
putInt64 = putInt64be . fromIntegral

getInt32 :: Get Int
getInt32 = fromIntegral <$> getInt32be

putInt32 :: Putter Int
putInt32 = putInt32be . fromIntegral

getInt8 :: Get Int
getInt8 = fromIntegral <$> getWord8

putInt8 :: Putter Int
putInt8 = putWord8 . fromIntegral
