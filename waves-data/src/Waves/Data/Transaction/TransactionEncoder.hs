module Waves.Data.Transaction.TransactionEncoder where

import           Data.ByteString
import qualified Data.ByteString.Internal             as BS (c2w, w2c)
import           Data.Serialize
import           Data.Text
import           Data.Text.Encoding
import           Waves.Data.Order
import           Waves.Data.Transaction.TransactionID
import           Waves.Data.Transaction.Types

data TxVersion
  = V_1
  | V_2

txEncoder :: Putter Tx
txEncoder (SignedPayment (TxHeader _ sender feeAmount _ timestamp) (Payment recipient amount) signature) = do
  put PaymentID
  putInt64 timestamp
  put sender
  put recipient
  putInt64 amount
  putInt64 feeAmount
txEncoder (SignedTransfer (TxHeader _ sender feeAmount feeAsset timestamp) (Transfer recipient asset amount att) signature) = do
  put TransferID
  put signature
  put TransferID
  put sender
  put asset
  put feeAsset
  putInt64 timestamp
  putInt64 amount
  putInt64 feeAmount
  put recipient
  putInt16be (fromIntegral $ Data.ByteString.length att)
  putByteString att
txEncoder (SignedIssue (TxHeader _ sender feeAmount _ timestamp) (Issue name description quantity decimals reissuable scriptOpt) signature) = do
  put IssueID
  put signature
  put IssueID
  put sender
  putText name
  putText description
  putInt64 quantity
  putWord8 (fromIntegral decimals)
  putWord8
    (if reissuable
       then 1
       else 0)
  putInt64 feeAmount
  putInt64 timestamp
txEncoder (SignedReissue (TxHeader _ sender feeAmount _ timestamp) (Reissue asset quantity reissuable) signature) = do
  put ReissueID
  put signature
  put ReissueID
  put sender
  put asset
  putInt64 quantity
  putWord8
    (if reissuable
       then 1
       else 0)
  putInt64 feeAmount
  putInt64 timestamp
txEncoder (SignedBurn (TxHeader _ sender feeAmount _ timestamp) (Burn asset quantity) signature) = do
  put BurnID
  put sender
  put asset
  putInt64 quantity
  putInt64 feeAmount
  putInt64 timestamp
  put signature
txEncoder (SignedLease (TxHeader _ sender feeAmount _ timestamp) (Lease amount recipient) signature) = do
  put LeaseID
  put sender
  put recipient
  putInt64 amount
  putInt64 feeAmount
  putInt64 timestamp
  put signature
txEncoder (SignedLeaseCancel (TxHeader _ sender feeAmount _ timestamp) (LeaseCancel id) signature) = do
  put LeaseCancelID
  put sender
  putInt64 feeAmount
  putInt64 timestamp
  putByteString id
  put signature
txEncoder (SignedCreateAlias (TxHeader _ sender feeAmount _ timestamp) (CreateAlias alias) signature) = do
  put CreateAliasID
  put sender
  put alias
  putInt64 feeAmount
  putInt64 timestamp
  put signature
txEncoder (SignedExchange (TxHeader _ sender feeAmount _ timestamp) (Exchange o1 o2 amount price buyFee sellFee) signature) = do
  put ExchangeID
  let o1bs = runPut $ putSignedOrder o1
      o1len = Data.ByteString.length o1bs
      o2bs = runPut $ putSignedOrder o1
      o2len = Data.ByteString.length o1bs
  putInt32be (fromIntegral o1len)
  putInt32be (fromIntegral o2len)
  putByteString o1bs
  putByteString o2bs
  putInt64 price
  putInt64 amount
  putInt64 buyFee
  putInt64 sellFee
  putInt64 feeAmount
  putInt64 timestamp
  put signature
txEncoder (ProvenTransfer (TxHeader _ sender feeAmount feeAsset timestamp) (Transfer recipient asset amount att) proofs) = do
  putPrefix TransferID V_2
  put sender
  put asset
  put feeAsset
  putInt64 timestamp
  putInt64 amount
  putInt64 feeAmount
  put recipient
  putInt16be (fromIntegral $ Data.ByteString.length att)
  putByteString att
  put proofs
txEncoder (ProvenIssue (TxHeader (Just cId) sender feeAmount _ timestamp) (Issue name description quantity decimals reissuable scriptOpt) proofs) = do
  putPrefix IssueID V_2
  putChainId cId
  put sender
  putText name
  putText description
  putInt64 quantity
  putWord8 (fromIntegral decimals)
  putWord8
    (if reissuable
       then 1
       else 0)
  putInt64 feeAmount
  putInt64 timestamp
  put proofs
txEncoder (ProvenReissue (TxHeader (Just cId) sender feeAmount _ timestamp) (Reissue asset quantity reissuable) proofs) = do
  putPrefix ReissueID V_2
  putChainId cId
  put sender
  put asset
  putInt64 quantity
  putWord8
    (if reissuable
       then 1
       else 0)
  putInt64 feeAmount
  putInt64 timestamp
  put proofs
txEncoder (ProvenBurn (TxHeader (Just cId) sender feeAmount _ timestamp) (Burn asset quantity) proofs) = do
  putPrefix BurnID V_2
  putChainId cId
  put sender
  put asset
  putInt64 quantity
  putInt64 feeAmount
  putInt64 timestamp
  put proofs
txEncoder (ProvenLease (TxHeader _ sender feeAmount _ timestamp) (Lease amount recipient) proofs) = do
  putPrefix LeaseID V_2
  putWord8 0
  put sender
  put recipient
  putInt64 amount
  putInt64 feeAmount
  putInt64 timestamp
  put proofs
txEncoder (ProvenLeaseCancel (TxHeader (Just cId) sender feeAmount _ timestamp) (LeaseCancel id) proofs) = do
  putPrefix LeaseCancelID V_2
  putChainId cId
  put sender
  putInt64 feeAmount
  putInt64 timestamp
  putByteString id
txEncoder (ProvenMassPayment (TxHeader _ sender feeAmount _ timestamp) (MassPayment asset pmts att) proofs) = do
  putPrefix MassPaymentID V_1
  put sender
  put asset
  putInt16be (fromIntegral $ Prelude.length pmts)
  mapM putPayment pmts
  putInt64 feeAmount
  putInt64 timestamp
  putInt16be (fromIntegral $ Data.ByteString.length att)
  putByteString att
  put proofs
txEncoder (ProvenCreateAlias (TxHeader _ sender feeAmount _ timestamp) (CreateAlias alias) proofs) = do
  putPrefix CreateAliasID V_2
  put sender
  put alias
  putInt64 feeAmount
  putInt64 timestamp
  put proofs
txEncoder (ProvenSponsorFee (TxHeader _ sender feeAmount _ timestamp) (SponsorFee asset minFee) proofs) = do
  putPrefix SponsorFeeID V_1
  put SponsorFeeID
  putVersion V_1
  put sender
  put asset
  putInt64 minFee
  putInt64 feeAmount
  putInt64 timestamp
  put proofs
txEncoder (ProvenData (TxHeader _ sender feeAmount _ timestamp) (Data ents) proofs) = do
  putPrefix DataID V_1
  put sender
  putInt16be (fromIntegral $ Prelude.length ents)
  mapM_ putEntry ents
  putInt64 timestamp
  putInt64 feeAmount
  put proofs
txEncoder (ProvenSetScript (TxHeader (Just cId) sender feeAmount _ timestamp) (SetScript maybeScript) proofs) = do
  putPrefix SetScriptID V_1
  putChainId cId
  put sender
  case maybeScript of
    Nothing -> putWord8 0
    Just bs -> do
      putWord8 1
      (putInt16be . fromIntegral) (Data.ByteString.length bs)
      putByteString bs
  putInt64 feeAmount
  putInt64 timestamp
  put proofs
txEncoder (ProvenExchange (TxHeader _ sender feeAmount _ timestamp) (Exchange o1 o2 amount price buyFee sellFee) proofs) = do
  putPrefix ExchangeID V_2
  let o1bs = runPut $ putSignedOrder o1
      o1len = Data.ByteString.length o1bs
      o2bs = runPut $ putSignedOrder o1
      o2len = Data.ByteString.length o1bs
  putInt32be (fromIntegral o1len)
  putOrderMark o1
  putByteString o1bs
  putInt32be (fromIntegral o2len)
  putOrderMark o2
  putByteString o2bs
  putInt64 price
  putInt64 amount
  putInt64 buyFee
  putInt64 sellFee
  putInt64 feeAmount
  putInt64 timestamp
  put proofs
  where
    putOrderMark (SignedOrder _ _) = putWord8 1
    putOrderMark _                 = return ()

putInt64 :: Putter Int
putInt64 = putInt64be . fromIntegral

putPrefix :: TxID -> TxVersion -> Put
putPrefix id ver = do
  putWord8 0
  put id
  putVersion ver

putVersion :: TxVersion -> Put
putVersion V_1 = putWord8 1
putVersion V_2 = putWord8 2

putEntry :: Putter DataEntry
putEntry (IntEntry k v) = do
  putText k
  putWord8 0
  putInt16be (fromIntegral v)
putEntry (BoolEntry k v) = do
  putText k
  putWord8 1
  if v
    then putWord8 1
    else putWord8 0
putEntry (BinEntry k v) = do
  putText k
  putWord8 2
  putInt16be (fromIntegral $ Data.ByteString.length v)
  putByteString v
putEntry (StrEntry k v) = do
  putText k
  putWord8 3
  putText v

putPayment :: Putter Payment
putPayment (Payment recipient amount) = do
  put recipient
  putInt64 amount

putText :: Putter Text
putText txt = do
  putInt16be len
  putByteString (encodeUtf8 txt)
  where
    len = fromIntegral $ Data.Text.length txt

putChainId :: Putter Char
putChainId cId = putWord8 (BS.c2w cId)

putOrder :: Putter Order
putOrder so@(SignedOrder _ _) = putSignedOrder so
putOrder po@(ProvenOrder _ _) = putProvenOrder po

putSignedOrder :: Putter Order
putSignedOrder (SignedOrder payload signature) = do
  putOrderPayload payload
  put signature

putProvenOrder :: Putter Order
putProvenOrder (ProvenOrder payload proofs) = do
  putWord8 2
  putOrderPayload payload
  put proofs

putOrderPayload :: Putter OrderPayload
putOrderPayload (OrderPayload sender matcher (AssetPair priceAsset amountAsset) side price amount fee exp timestamp)
  = do
  put sender
  put matcher
  put amountAsset
  put priceAsset
  putOrderType side
  putInt64 price
  putInt64 amount
  putInt64 timestamp
  putInt64 exp
  putInt64 fee

putOrderType BUY = putWord8 0
putOrderType SELL = putWord8 1