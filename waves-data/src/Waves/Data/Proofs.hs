{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Waves.Data.Proofs where

import           Control.Monad   (replicateM, when)
import           Data.ByteString
import qualified Data.ByteString as BS
import           Data.List
import           Data.Serialize
import           Data.Text       hiding (dropWhileEnd)
import qualified Data.Text       as T
import           Waves.Crypto
import           Waves.Utils

data Proof
  = EmptyProof
  | ProofBytes !ByteString
  deriving (Eq)

instance Show Proof where
  show EmptyProof      = "Proof[empty]"
  show (ProofBytes bs) = "Proof[" ++ (show . encode58) bs ++ "]"

newtype Proofs = Proofs [Proof] deriving (Eq, Show)

maxProofSize = 64
maxProofCount = 8

fromByteStringList :: [ByteString] -> Either Text Proofs
fromByteStringList proofs = Proofs <$> make 0 proofs
  where make i (x:xs)
          | i > maxProofCount = Left tooManyProofs
          | BS.length x > maxProofSize = Left tooBigProof
          | BS.null x = (:) EmptyProof <$> make (i + 1) xs
          | otherwise = (:) (ProofBytes x) <$> make (i + 1) xs


tooManyProofs :: Text
tooManyProofs = "Too many proofs (max = " <> tshow maxProofCount <> ")"

tooBigProof :: Text
tooBigProof = "Proof is too big (max allowed - " <> tshow maxProofSize <> ")"

instance Serialize Proof where
  get = do
    len <- fromIntegral <$> getInt16be
    if
      | len == 0 -> return EmptyProof
      | len > maxProofSize -> fail $ T.unpack tooBigProof
      | otherwise -> ProofBytes <$> getByteString len
  put EmptyProof = putInt16be 0
  put (ProofBytes proof) = do
    putInt16be len
    putByteString proof
    where len = fromIntegral $ BS.length proof


instance Serialize Proofs where
  get = do
    v <- getWord8 -- version
    when (v /= 1) $ fail "Unsupported proofs version"
    cnt <- fromIntegral <$> getInt16be
    if cnt > maxProofCount
      then fail $ T.unpack tooManyProofs
      else Proofs <$> replicateM cnt get
  put (Proofs proofs) = do
    putWord8 1
    putInt16be cnt
    mapM_ put trimmed
    where trimmed = dropWhileEnd proofEmpty proofs
          cnt = fromIntegral $ Prelude.length trimmed

proofEmpty :: Proof -> Bool
proofEmpty EmptyProof = True
proofEmpty _          = False
