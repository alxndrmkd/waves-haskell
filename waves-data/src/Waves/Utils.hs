{-# LANGUAGE OverloadedStrings #-}

module Waves.Utils where

import           Data.Bits
import           Data.Text


tshow :: Show a => a -> Text
tshow = pack . show

ip4Delim :: Text
ip4Delim = "."

intRep :: [Text] -> [Int]
intRep = Prelude.map (read . unpack)

ip4 :: Text -> Int
ip4 str = (a `shift` 24) + (b `shift` 16) + (c `shift` 8) + d
    where [d,c,b,a] = intRep $ splitOn ip4Delim str
