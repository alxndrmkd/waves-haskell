{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AddressOrAliasSpec where

import           Control.Monad
import           Data.ByteString
import           Data.Maybe
import           Data.Serialize
import           Data.Serialize.Get
import           Data.Text
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Waves.Crypto
import           Waves.Data.AddressOrAlias

newtype AddrPrec = AddrPrec (Char, Text, Text) deriving Show

addressCheckInputData :: [AddrPrec]
addressCheckInputData = [
    AddrPrec ('W', "5CnGfSjguYfzWzaRmbxzCbF5qRNGTXEvayytSANkqQ6A", "3PQ8bp1aoqHQo3icNqFv6VM36V1jzPeaG1v")
	, AddrPrec ('W', "BstqhtQjQN9X78i6mEpaNnf6cMsZZRDVHNv3CqguXbxq", "3PQvBCHPnxXprTNq1rwdcDuxt6VGKRTM9wT")
	, AddrPrec ('W', "FckK43s6tQ9BBW77hSKuyRnfnrKuf6B7sEuJzcgkSDVf", "3PETfqHg9HyL92nfiujN5fBW6Ac1TYiVAAc")
	, AddrPrec ('T', "5CnGfSjguYfzWzaRmbxzCbF5qRNGTXEvayytSANkqQ6A", "3NC7nrggwhk2AbRC7kzv92yDjbVyALeGzE5")
  , AddrPrec ('T', "BstqhtQjQN9X78i6mEpaNnf6cMsZZRDVHNv3CqguXbxq", "3NCuNExVvpzSE15QkngdemY9XCyVVGhHA9h")
  ]

instance Arbitrary AddrPrec where
  arbitrary = elements addressCheckInputData

addressFromStringEqualsAddressFromPK :: AddrPrec -> Bool
addressFromStringEqualsAddressFromPK (AddrPrec (chid, xpub_base58, addr_base58))
  = let xpub = (fromJust . (decode58 >=> publicKey)) xpub_base58
        (Right addrFromString) = addressFromString addr_base58
        addrFromPK = addressFromPublicKey chid xpub
    in addrFromString == addrFromPK
