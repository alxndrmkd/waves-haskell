{-# LANGUAGE TypeSynonymInstances #-}

module Waves.Data.Common where

import           Data.Serialize
import           Waves.Crypto

type Timestamp = Int

instance Serialize PublicKey where
  get = do
    bytes <- getByteString 32
    case publicKey bytes of
      Nothing -> fail "Cannot deserialize public key"
      Just pk -> return pk
  put pk = putByteString (publicKeyBytes pk)

instance Serialize Signature where
  get = do
    bytes <- getByteString 64
    case signature bytes of
      Nothing  -> fail "Cannot deserialize signature"
      Just sig -> return sig
  put sig = putByteString (signatureBytes sig)
