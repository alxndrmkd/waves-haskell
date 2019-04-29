{-# LANGUAGE MultiWayIf #-}

module Waves.Data.Microblock where

import           Control.Monad          (replicateM)
import           Data.ByteString
import           Data.Serialize
import           Waves.Crypto
import           Waves.Data.Transaction

data Microblock = Microblock
  { mbVersion                :: !Int
  , mbSender                 :: !PublicKey
  , mbTransactions           :: ![Tx]
  , mbPrevResBlockSignature  :: !Signature
  , mbTotalResBlockSignature :: !Signature
  , mbSignature              :: !Signature
  } deriving (Show)

instance Serialize Microblock where
  get = do
    ver <- fromIntegral <$> getWord8
    prevBlockSig <- get
    totalBlockSig <- get
    skip 4
    cnt <-
      if
        | ver == 1 || ver == 2 -> fromIntegral <$> getWord8
        | ver == 3 -> fromIntegral <$> getInt32be
        | otherwise -> fail "unknown microblock version"
    txs <- parseTransactions cnt
    sender <- get
    signature <- get
    return $ Microblock ver sender txs prevBlockSig totalBlockSig signature
  put (Microblock ver sender txs prevBlockSig totalBlockSig signature)
    = do
    putWord8 (fromIntegral ver)
    put prevBlockSig
    put totalBlockSig
    putInt32be (fromIntegral txLen)
    if
      | ver == 1 || ver == 2 -> putWord8 (fromIntegral txCount)
      | ver == 3 -> putInt32be (fromIntegral txCount)
      | otherwise -> fail "unknown microblock version"
    put txBytes
    put sender
    put signature
    where txLen = Data.ByteString.length txBytes
          txBytes = runPut (putTransactions txs)
          txCount = Prelude.length txs

parseTransactions :: Int -> Get [Tx]
parseTransactions cnt = replicateM cnt $ do
  skip 4
  get
          
putTransactions :: [Tx] -> Put
putTransactions = mapM_ putTx
  where putTx tx =
          let bytes = runPut (put tx)
          in do
            putInt32be (fromIntegral $ Data.ByteString.length bytes)
            putByteString bytes
          

