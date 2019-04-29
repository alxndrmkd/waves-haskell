{-# LANGUAGE TypeSynonymInstances #-}

module Waves.Data.Asset where

import           Data.ByteString
import           Data.Serialize
import           Waves.Crypto

newtype AssetID = AssetID ByteString deriving (Eq)

data Asset = Waves | Token !AssetID deriving (Eq)

instance Serialize AssetID where
  get = AssetID <$> getByteString 32
  put (AssetID id) = putByteString id

instance Show AssetID where
  show (AssetID id) = show (encode58 id)

instance Show Asset where
  show Waves      = "WAVES"
  show (Token id) = show id

instance Serialize Asset where
  get = do
    m <- getWord8
    if m == 0
      then return Waves
      else Token <$> get
  put Waves = putWord8 0
  put (Token id) = do
    putWord8 1
    put id
