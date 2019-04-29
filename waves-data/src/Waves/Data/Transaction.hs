module Waves.Data.Transaction (
    TxID(..)
  , Tx(..)
  , Payment(..)
  , Transfer(..)
  , Issue(..)
  , Reissue(..)
  , Burn(..)
  , SponsorFee(..)
  , Lease(..)
  , LeaseCancel(..)
  , MassPayment(..)
  , CreateAlias(..)
  , Exchange(..)
  , Data(..)
  , DataEntry(..)
  , SetScript(..)
  , txSign
  , txSigVerify
) where

import           Data.Serialize
import           Data.Text
import           Waves.Data.Transaction.TransactionDecoder
import           Waves.Data.Transaction.TransactionEncoder
import           Waves.Data.Transaction.TransactionID
import           Waves.Data.Transaction.Types
import           Waves.Data.Transaction.TransactionSign

instance Serialize Tx where
  get = do
    m <- getWord8
    if m == 0
      then do
        id <- get
        ver <- fromIntegral <$> getWord8
        txDecoder id ver
      else case fromByte m of
            Left err -> fail (Data.Text.unpack err)
            Right id -> txDecoder id 1
  put = txEncoder
