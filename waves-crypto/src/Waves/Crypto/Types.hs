{-# LANGUAGE TypeSynonymInstances #-}

module Waves.Crypto.Types
  ( Signature
  , signature
  , signatureBytes
  , PrivateKey
  , privateKey
  , privateKeyBytes
  , PublicKey
  , publicKey
  , publicKeyBytes
  , KeyPair(..)
  ) where

import           Crypto.Elliptics.Curve25519
import           Waves.Crypto.Base

-------------------------------------------------
instance Show Signature where
  show = show . encode58 . signatureBytes

instance Eq Signature where
  (==) s1 s2 = signatureBytes s1 == signatureBytes s2

instance Ord Signature where
  compare s1 s2 = signatureBytes s1 `compare` signatureBytes s2

-------------------------------------------------
instance Show PublicKey where
  show = show . encode58 . publicKeyBytes

instance Eq PublicKey where
  (==) pk1 pk2 = publicKeyBytes pk1 == publicKeyBytes pk2

instance Ord PublicKey where
  compare pk1 pk2 = publicKeyBytes pk1 `compare` publicKeyBytes pk2

-------------------------------------------------
instance Show PrivateKey where
  show = show . encode58 . privateKeyBytes

instance Eq PrivateKey where
  (==) pk1 pk2 = privateKeyBytes pk1 == privateKeyBytes pk2

instance Ord PrivateKey where
  compare pk1 pk2 = privateKeyBytes pk1 `compare` privateKeyBytes pk2
