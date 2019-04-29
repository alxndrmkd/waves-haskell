# Elliptics

## Bindings for trevp/ref10_extract.

```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Crypto.Elliptics.Curve25519 as C
import           Data.ByteString
import           Data.ByteString.Random      (random)
import           Data.Maybe

main :: IO ()
main = do
  seed <- random 32
  rnd <- random 64
  let (C.KeyPair xpub xprv) = C.keyPairgen seed
      signature = C.сurve25519Sign xprv msg rnd
  print $ C.curve25519Verify xpub signature msg

msg :: ByteString
msg = "Hello, world!"
```
