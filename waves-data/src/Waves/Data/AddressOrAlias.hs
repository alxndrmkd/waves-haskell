{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Waves.Data.AddressOrAlias where

import           Data.ByteString
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import           Data.Serialize
import           Data.Text
import           Data.Text.Encoding
import           Waves.Crypto
import           Waves.Utils

data AddressOrAlias
  = Address_ Address
  | Alias_ Alias
  deriving (Eq)

chainIdFromAddressOrAlias :: AddressOrAlias -> Char
chainIdFromAddressOrAlias (Address_ (Address cid _)) = cid
chainIdFromAddressOrAlias (Alias_ (Alias cid _))     = cid

data Address = Address
  { aaChainId :: !Char
  , aaBytes   :: !ByteString
  } deriving (Eq)

data Alias = Alias
  { alChainId :: !Char
  , alAlias   :: !Text
  } deriving (Eq)

-- | Show Instances

instance Show AddressOrAlias where
  show (Address_ addr) = show addr
  show (Alias_ alias)  = show alias

instance Show Address where
  show (Address cId bs) = show cId ++ ':' : show (encode58 bs)

instance Show Alias where
  show (Alias cId al) = "alias:" ++ cId : ':' : Data.Text.unpack al

-- | Ser/de

instance Serialize AddressOrAlias where
  get = do
    typeId <- getWord8
    if
      | typeId == 1 -> Address_ <$> getAddress
      | typeId == 2 -> Alias_ <$> get
      | otherwise -> fail $ "Unknown address/alias type" ++ show typeId
  put (Address_ addr) = do
    putWord8 1
    put addr
  put (Alias_ al) = do
    putWord8 2
    put al

addressHashLength = 20
addressChecksumLength = 4
addressLength = 1 + 1 + addressHashLength + addressChecksumLength

getAddress :: Get Address
getAddress = do
  chId <- get
  hash <- getByteString addressHashLength
  chks <- getByteString addressChecksumLength
  let bytes = BS.cons 1 (BS.cons (BS.c2w chId) hash)
      checksum = (BS.take 4 . secureHashBS) bytes
  if checksum == chks
    then return (Address chId (BS.concat [ bytes, chks ]))
    else fail "Address checksum not valid"

getAlias :: Get Alias
getAlias = do
    chId <- get
    len <- getInt16be
    if
      | len < minAliasLength -> fail $ "Alias length - " ++ show len ++ " less than min allowed (4)"
      | len > maxAliasLength -> fail $ "Alias length - " ++ show len ++ " greater than max alowed (30)"
      | otherwise -> do
          alias <- getByteString (fromIntegral len)
          return (Alias chId (decodeUtf8 alias))


putAddress :: Putter Address
putAddress (Address _ bs) = putByteString bs

instance Serialize Address where
  get = skip 1 >> getAddress
  put (Address _ bs) = putByteString bs

minAliasLength = 4
maxAliasLength = 30

instance Serialize Alias where
  get = skip 1 >> getAlias
  put (Alias chId al) = do
    put chId
    putInt16be len
    putByteString albs
    where albs = encodeUtf8 al
          len = fromIntegral (BS.length albs)

addressFromPublicKey :: Char -> PublicKey -> Address
addressFromPublicKey chId xpub
  = Address chId addrBytes
  where addrBytes = BS.concat [ bytes, chks ]
        chks = (BS.take 4 . secureHashBS) bytes
        bytes = BS.cons 1 (BS.cons (BS.c2w chId) xpubHash)
        xpubHash = (BS.take 20 . secureHashBS) (publicKeyBytes xpub)

addressStringLength = 36

addressFromString :: Text -> Either Text Address
addressFromString addrStr
  = let addrLength = Data.Text.length addrStr
        addrBytes = decode58 addrStr
    in if addrLength >= addressStringLength
        then Left $ "Wrong address string length (actual - " <> tshow addrLength <> ", max - 36)"
        else case addrBytes of
          Nothing -> Left ("Expected base58 encoded string. Actual - " <> addrStr)
          Just bytes ->
            case runGet get (BS.tail bytes) of
              Left err   -> Left (Data.Text.pack err)
              Right addr -> Right addr
