module Waves.Data.Order where

import           Waves.Crypto
import           Waves.Data.Asset
import           Waves.Data.Common
import           Waves.Data.Proofs

data AssetPair = AssetPair
  { apPriceAsset  :: !Asset
  , apAmountAsset :: !Asset
  } deriving (Eq, Show)

data OrderType = BUY | SELL deriving (Eq, Show)

data OrderPayload = OrderPayload
  { ordSender     :: !PublicKey
  , ordMatcher    :: !PublicKey
  , ordPair       :: !AssetPair
  , ordType       :: !OrderType
  , ordPrice      :: !Int
  , ordAmount     :: !Int
  , ordFee        :: !Int
  , ordExpiration :: !Timestamp
  , ordTimestamp  :: !Timestamp
  } deriving (Show)
  
data Order
  = SignedOrder !OrderPayload !Signature
  | ProvenOrder !OrderPayload !Proofs
  deriving (Show)