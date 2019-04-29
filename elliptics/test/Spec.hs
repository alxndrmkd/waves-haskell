{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Crypto.Elliptics.Curve25519 as C
import qualified Data.ByteString             as B
import           Test.QuickCheck
import           Test.QuickCheck.Gen

main :: IO ()
main =
  quickCheck signatureValidWhenSignedWithCorrectKey >>
  quickCheck signatureInvalidWhenSignedWithIncorrectKey

newtype BS32 = BS32
  { getBS32 :: B.ByteString
  } deriving (Show)

newtype BS64 = BS64
  { getBS64 :: B.ByteString
  } deriving (Show)

type Preconditions = (BS32, B.ByteString, BS64)

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

instance Arbitrary BS32 where
  arbitrary = fmap (BS32 . B.pack) (vectorOf 32 arbitrary)

instance Arbitrary BS64 where
  arbitrary = fmap (BS64 . B.pack) (vectorOf 64 arbitrary)

signatureValidWhenSignedWithCorrectKey :: Preconditions -> Bool
signatureValidWhenSignedWithCorrectKey (BS32 seed, msg, BS64 rnd) =
  let (C.KeyPair xpub xprv) = C.keyPairgen seed
      signature = C.сurve25519Sign xprv msg rnd
   in C.curve25519Verify xpub signature msg

signatureInvalidWhenSignedWithIncorrectKey :: Preconditions -> Bool
signatureInvalidWhenSignedWithIncorrectKey (BS32 seed, msg, BS64 rnd) =
  let xprv = C.curve25519PrivateKeygen (B.take 32 rnd)
      (C.KeyPair xpub _) = C.keyPairgen seed
      signature = C.сurve25519Sign xprv msg rnd
   in not (C.curve25519Verify xpub signature msg)
