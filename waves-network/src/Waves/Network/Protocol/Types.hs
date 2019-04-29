{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Waves.Network.Protocol.Types where

import           Data.ByteString
import           Data.Serialize
import           Data.Text
import           Data.Word
import           Network.Socket         (SockAddr (..))
import           Waves.Crypto
import           Waves.Data
import           Waves.Data.Transaction

data NodeInfo = NodeInfo
  { niVersion :: !AppVersion
  , niChainId :: !Char
  , niName    :: !Text
  , niNonce   :: !Int
  , niAddress :: !(Maybe SockAddr)
  } deriving (Eq, Ord)

instance Show NodeInfo where
  show (NodeInfo ver chain name nonce addr) =
    "Node(" ++
    "waves" ++ [chain] ++ "-" ++ show ver ++ '|' : Data.Text.unpack name ++ ")"

newtype Magic =
  Magic Word32
  deriving (Show)

data AppVersion = AppVersion
  { avMaj   :: !Int
  , avMin   :: !Int
  , avPatch :: !Int
  } deriving (Eq, Ord)

instance Show AppVersion where
  show (AppVersion min maj patch) =
    'v' : show min ++ '.' : show maj ++ '.' : show patch

data Handshake = Handshake
  { hsAppName         :: !Text
  , hsAppVersion      :: !AppVersion
  , hsNodeName        :: !Text
  , hsNodeNonce       :: !Int
  , hsDeclaredAddress :: !(Maybe SockAddr)
  } deriving (Show)

infoFromHandshake :: Handshake -> NodeInfo
infoFromHandshake (Handshake chain ver name nonce addr) =
  NodeInfo ver (Data.Text.last chain) name nonce addr

newtype Checksum =
  Checksum Word32
  deriving (Show, Eq)

calculateChecksum :: ByteString -> Checksum
calculateChecksum w
  | Data.ByteString.null w = Checksum 0
  | otherwise = Checksum w32
  where
    bs = (Data.ByteString.take 4 . fastHashBS) w
    Right w32 = runGet getWord32be bs

data ContentID
  = GetPeersID
  | PeersID
  | GetSignaturesID
  | SignaturesID
  | GetBlockID
  | BlockID
  | CheckpointID
  | ScoreID
  | TransactionID
  | MicroblockRequestID
  | MicroblockInvID
  | MicroblockResponseID
  deriving (Show, Eq)

data NetworkMessage
  = GetPeers
  | Peers ![SockAddr]
  | GetSignatures ![Signature]
  | Signatures ![Signature]
  | GetBlock !Signature
  | BlockMessage !Block
  | TransactionMessage !Tx
  | CheckpointMessage ![Checkpoint]
  | ScoreChanged !Integer
  | MicroblockInv !PublicKey
                  !Signature
                  !Signature
                  !Signature
  | MicroblockRequest !Signature
  | MicroblockResponse !Microblock
  deriving (Show)
