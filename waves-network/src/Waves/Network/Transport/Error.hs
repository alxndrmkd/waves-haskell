{-# LANGUAGE ConstraintKinds #-}

module Waves.Network.Transport.Error where

import           Control.Exception
import           Data.Text
import           Waves.Network.Protocol.Types

data TransportError
  = TransportConnectionError !ConnectionError
                             !Text
  | TransportIOError !IOError
                     !Text
  | TransportProtoError !Text
                        !Text
  deriving (Show)

instance Exception TransportError

data ConnectionError
  = ConnectionTimeout
  | ConnectionClosed
  deriving (Show)
