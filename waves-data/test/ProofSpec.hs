{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ProofSpec where

import qualified Data.ByteString     as B
import           Data.List
import           Data.Serialize
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Waves.Data.Proofs

type CorrectBS = B.ByteString

instance Arbitrary CorrectBS where
  arbitrary = fmap B.pack (vectorOf 64 arbitrary)

instance Arbitrary Proof where
  arbitrary = do
    shouldBeEmpty <- choose (True, False)
    if shouldBeEmpty
      then return EmptyProof
      else do
        (bs :: CorrectBS) <- arbitrary
        return (ProofBytes bs)

instance Arbitrary Proofs where
  arbitrary = Proofs <$> vectorOf 8 arbitrary

type WrongBS = B.ByteString

proofRoundTrip :: Proof -> Bool
proofRoundTrip proof = proof == proofFromBytes
  where (Right proofFromBytes) = runGet get proofBytes
        proofBytes = runPut (put proof)

proofsRoundTrip :: Proofs -> Bool
proofsRoundTrip proofs = trim proofs == proofsFromBytes
  where (Right proofsFromBytes) = runGet get proofsBytes
        proofsBytes = runPut (put proofs)

-- proofsTrimmedWhenSerializing
trim (Proofs ps) = Proofs $ dropWhileEnd proofEmpty ps