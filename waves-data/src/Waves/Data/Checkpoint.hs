module Waves.Data.Checkpoint where

import Waves.Crypto

data Checkpoint = Checkpoint
  { bcHeight    :: !Int
  , bcSignature :: !Signature
  } deriving (Show)