{-|
  Module      : Curve25519
  Copyright   : (c) Alexandr Makoed, 2018
  License     : MIT
  Maintainer  : disfenoid@gmail.com
  Stability   : experimental
  Portability : POSIX
-}
module Crypto.Elliptics.Curve25519
  ( 
  -- * Constants
    curve25519KeyLength
  , curve25519SignatureLength
  -- * Types
  , PrivateKey
  , PublicKey
  , Signature
  , KeyPair(..)
  -- * Smart constructors
  , privateKey
  , publicKey
  , signature
  -- * Functions
  , privateKeyBytes
  , publicKeyBytes
  , signatureBytes
  , сurve25519Sign
  , curve25519Verify
  , curve25519Keygen
  , curve25519PrivateKeygen
  , keyPairgen
  ) where

import qualified Data.ByteString        as B
import           Data.ByteString.Unsafe
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           System.IO.Unsafe

-- |Length of curve25519 Private/Public key (32)
curve25519KeyLength :: Int
curve25519KeyLength = 32

-- |Length of curve25519 signature (64)
curve25519SignatureLength :: Int
curve25519SignatureLength = 64

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- |Curve25519 PrivateKey
newtype PrivateKey =
  Prv B.ByteString

-- |Curve25519 PublicKey
newtype PublicKey =
  Pub B.ByteString

-- |Curve25519 Signature
newtype Signature =
  Sig B.ByteString

-- |Curve25519 KeyPair
data KeyPair = KeyPair
  { getPublicKey  :: !PublicKey
  , getPrivateKey :: !PrivateKey
  }

-- |Generates keypair from random 'ByteString'.
-- Input bytestring should have a length of 32 bytes.
keyPairgen :: B.ByteString -> KeyPair
keyPairgen rnd = KeyPair xpub xprv
  where
    xprv = curve25519PrivateKeygen rnd
    xpub = curve25519Keygen xprv

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- |Converts public key to ByteString
publicKeyBytes :: PublicKey -> B.ByteString
publicKeyBytes (Pub bs) = bs

-- |Converts 'ByteString' to 'PublicKey'.
-- Returns 'Nothing' if length of input bytestring not equal to 'curve25519KeyLength'
publicKey :: B.ByteString -> Maybe PublicKey
publicKey bs =
  if B.length bs == curve25519KeyLength
    then Just (Pub bs)
    else Nothing

-- |Converts 'PrivateKey' to 'ByteString'
privateKeyBytes :: PrivateKey -> B.ByteString
privateKeyBytes (Prv bs) = bs

-- |Converts 'ByteString' to 'PrivateKey'.
-- Returns 'Nothing' if length of input bytestring not equal to 'curve25519KeyLength'
privateKey :: B.ByteString -> Maybe PrivateKey
privateKey bs =
  if B.length bs == curve25519KeyLength
    then Just (Prv bs)
    else Nothing

-- |Converts 'Signature' to 'ByteString'
signatureBytes :: Signature -> B.ByteString
signatureBytes (Sig bs) = bs

-- |Converts 'ByteString' to 'Signature'.
-- Returns Nothing if length of input bytestring not equal to 'curve25519SignatureLength'
signature :: B.ByteString -> Maybe Signature
signature bs =
  if B.length bs == curve25519SignatureLength
    then Just (Sig bs)
    else Nothing

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
foreign import ccall unsafe "curve25519_keygen" c_curve25519_keygen
  :: Ptr CChar -> Ptr CChar -> IO ()

-- |Generate 'PublicKey' from 'PrivateKey'
curve25519Keygen :: PrivateKey -> PublicKey
curve25519Keygen (Prv xprv) =
  unsafePerformIO $
  B.useAsCString xprv $ \xprvPtr ->
    fmap Pub $
    allocaBytes curve25519KeyLength $ \xpubPtr -> do
      c_curve25519_keygen xpubPtr xprvPtr
      B.packCStringLen (xpubPtr, curve25519KeyLength)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
foreign import ccall unsafe "curve25519_sign" c_curve25519_sign
  :: Ptr CChar ->
  Ptr CChar -> Ptr CChar -> CULong -> Ptr CChar -> IO CInt

-- |Sign message with 'PrivateKey' using random 'ByteString'.
сurve25519Sign :: PrivateKey -> B.ByteString -> B.ByteString -> Signature
сurve25519Sign (Prv xprv) msg random =
  unsafePerformIO $
  B.useAsCString xprv $ \xprvPtr ->
    B.useAsCStringLen msg $ \(msgPtr, msgLen) ->
      B.useAsCString random $ \rndPtr ->
        fmap Sig $
        allocaBytes curve25519SignatureLength $ \outPtr -> do
          c_curve25519_sign outPtr xprvPtr msgPtr (fromIntegral msgLen) rndPtr
          B.packCStringLen (outPtr, curve25519SignatureLength)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
foreign import ccall unsafe "curve25519_verify" c_curve25519_verify
  :: Ptr CChar -> Ptr CChar -> Ptr CChar -> CULong -> IO CInt

-- |Verify 'Signature' of message with 'PublicKey'.
-- Returns True if 'Signature' valid.
curve25519Verify :: PublicKey -> Signature -> B.ByteString -> Bool
curve25519Verify (Pub xpub) (Sig sig) msg =
  unsafePerformIO $
  B.useAsCString xpub $ \xpubPtr ->
    B.useAsCString sig $ \sigPtr ->
      B.useAsCStringLen msg $ \(msgPtr, msgLen) -> do
        r <- c_curve25519_verify sigPtr xpubPtr msgPtr (fromIntegral msgLen)
        return $ r == 0

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
foreign import ccall unsafe "curve25519_private_keygen" c_curve25519_private_keygen
  :: Ptr CChar -> Ptr CChar -> IO ()

-- |Generates 'PrivateKey' from random 'ByteString'.
-- Input 'ByteString' should have a length of 'curve25519KeyLength'.
curve25519PrivateKeygen :: B.ByteString -> PrivateKey
curve25519PrivateKeygen random =
  unsafePerformIO $
  B.useAsCString random $ \rndPtr ->
    fmap Prv $
    allocaBytes curve25519KeyLength $ \outPtr -> do
      c_curve25519_private_keygen rndPtr outPtr
      B.packCStringLen (outPtr, curve25519KeyLength)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
