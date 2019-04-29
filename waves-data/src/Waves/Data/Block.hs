module Waves.Data.Block where

import           Control.Monad
import           Data.ByteString
import           Data.Serialize
import           Data.Set
import           Waves.Crypto
import           Waves.Data.Common
import           Waves.Data.Transaction

newtype BaseTarget =
  BaseTarget Int
  deriving (Show)

data ConsensusData = ConsensusData
  { cdBaseTarget          :: !BaseTarget
  , cdGenerationSignature :: !ByteString
  } deriving (Show)

data Block = Block
  { bTimestamp     :: !Timestamp
  , bVersion       :: !Int
  , bConsensusData :: !ConsensusData
  , bReference     :: !Signature
  , bGenerator     :: !PublicKey
  , bSignature     :: !Signature
  , bVotedFeatures :: !(Set Int)
  , bTransactions  :: ![Tx]
  } deriving (Show)

instance Serialize Block where
  get = do
    ver <- fromIntegral <$> getWord8
    timestamp <- fromIntegral <$> getInt64be
    reference <- get
    cDataLength <- fromIntegral <$> getInt32be
    baseTarget <- BaseTarget . fromIntegral <$> getInt64be
    genSig <- getByteString (cDataLength - 8)
    skip 4
    txCount <-
      if ver == 1 || ver == 2
        then fromIntegral <$> getWord8
        else fromIntegral <$> getInt32be
    txs <- replicateM txCount (skip 4 >> get)
    votesCount <-
      if ver == 1 || ver == 2
        then return 0
        else fromIntegral <$> getInt32be
    votes <- replicateM votesCount (fromIntegral <$> getInt16be)
    generator <- get
    signature <- get
    return $ Block timestamp ver (ConsensusData baseTarget genSig) reference generator signature (fromList votes) txs
  put (Block timestamp ver (ConsensusData (BaseTarget bt) genSig) reference generator signature votes txs)
    = do
    putWord8 (fromIntegral ver)
    putInt64be (fromIntegral timestamp)
    put reference
    putInt32be 40
    putInt64be (fromIntegral bt)
    putByteString genSig
    putInt32be (fromIntegral txLen)
    if ver == 1 || ver == 2
      then putWord8 (fromIntegral txCount)
      else putInt32be (fromIntegral txCount)
    putByteString txBytes
    if ver == 1 || ver == 2
      then return ()
      else do
        putInt32be (fromIntegral $ Data.Set.size votes)
        mapM_ (putInt16be . fromIntegral) votes
    put generator
    put signature
    where txCount = Prelude.length txs
          txLen = Data.ByteString.length txBytes
          txBytes = runPut (putTransactions txs)

putTransactions :: [Tx] -> Put
putTransactions = mapM_ putTx
  where putTx tx =
          let bytes = runPut (put tx)
          in do
            putInt32be (fromIntegral $ Data.ByteString.length bytes)
            putByteString bytes
