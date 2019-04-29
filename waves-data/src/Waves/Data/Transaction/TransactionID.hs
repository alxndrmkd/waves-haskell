{-# LANGUAGE OverloadedStrings #-}

module Waves.Data.Transaction.TransactionID where

import           Data.Serialize
import           Data.Text
import           Data.Word

data TxID
  = GenesisID | PaymentID | TransferID
  | IssueID | ReissueID | BurnID
  | ExchangeID | LeaseID | LeaseCancelID
  | CreateAliasID | MassPaymentID | DataID
  | SetScriptID | SponsorFeeID
  deriving (Eq, Show)

fromByte :: Word8 -> Either Text TxID
fromByte 1  = Right GenesisID
fromByte 2  = Right PaymentID
fromByte 3  = Right IssueID
fromByte 4  = Right TransferID
fromByte 5  = Right ReissueID
fromByte 6  = Right BurnID
fromByte 7  = Right ExchangeID
fromByte 8  = Right LeaseID
fromByte 9  = Right LeaseCancelID
fromByte 10 = Right CreateAliasID
fromByte 11 = Right MassPaymentID
fromByte 12 = Right DataID
fromByte 13 = Right SetScriptID
fromByte 14 = Right SponsorFeeID
fromByte _  = Left "Unknown transaction type"

instance Serialize TxID where
  get = do
    byte <- getWord8
    case fromByte byte of
      Right v  -> return v
      Left err -> fail (Data.Text.unpack err)
  put GenesisID     = putWord8 1 --
  put PaymentID     = putWord8 2 --
  put IssueID       = putWord8 3 --
  put TransferID    = putWord8 4 --
  put ReissueID     = putWord8 5 --
  put BurnID        = putWord8 6 --
  put ExchangeID    = putWord8 7 --
  put LeaseID       = putWord8 8 --
  put LeaseCancelID = putWord8 9 --
  put CreateAliasID = putWord8 1 --
  put MassPaymentID = putWord8 1 --
  put DataID        = putWord8 1 --
  put SetScriptID   = putWord8 1 --
  put SponsorFeeID  = putWord8 1 --
