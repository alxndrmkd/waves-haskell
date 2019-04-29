module Waves.Crypto.Base where

import           Data.ByteString.Base58
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString
import           Data.Text
import           Data.Text.Encoding

encode58 :: ByteString -> Text
encode58 = decodeUtf8 . encodeBase58 bitcoinAlphabet

decode58 :: Text -> Maybe ByteString
decode58 = decodeBase58 bitcoinAlphabet . encodeUtf8

encode64 :: ByteString -> Text
encode64 = decodeUtf8 . B64.encode

decode64 :: Text -> Maybe ByteString
decode64 str =
  case (B64.decode . encodeUtf8) str of
    Right v -> Just v
    Left _  -> Nothing
