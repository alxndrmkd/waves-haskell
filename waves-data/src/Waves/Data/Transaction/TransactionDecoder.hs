module Waves.Data.Transaction.TransactionDecoder where

import           Control.Monad                        (replicateM)
import           Data.ByteString
import qualified Data.ByteString.Internal             as BS (c2w, w2c)
import           Data.Serialize
import           Data.Text
import           Data.Text.Encoding
import           Waves.Crypto
import           Waves.Data.AddressOrAlias
import           Waves.Data.Asset
import           Waves.Data.Common
import           Waves.Data.Order
import           Waves.Data.Transaction.TransactionID
import           Waves.Data.Transaction.Types

txDecoder :: TxID -> Int -> Get Tx
txDecoder GenesisID _ = do
  timestamp <- getInt64
  recipient <- get
  amount <- getInt64
  return $ Genesis recipient amount timestamp
txDecoder PaymentID _ = do
  timestamp <- getInt64
  sender <- get
  recipient <- get
  amount <- getInt64
  feeAmount <- getInt64
  let header = TxHeader Nothing sender feeAmount Waves timestamp
      payment = Payment recipient amount
  signature <- get
  return $ SignedPayment header payment signature
txDecoder TransferID 1 = do
  signature <- get
  skip 1
  (header, transfer) <- transferParseBase
  return $ SignedTransfer header transfer signature
txDecoder TransferID 2 = do
  (header, transfer) <- transferParseBase
  ProvenTransfer header transfer <$> get
txDecoder IssueID 1 = do
  signature <- get
  skip 1
  (header, issue) <- issueParseBase
  return $ SignedIssue header issue signature
txDecoder IssueID 2 = do
  chainId <- BS.w2c <$> getWord8
  (header, Issue n d q dec r _) <- issueParseBase
  scriptOpt <- getScriptOpt
  proofs <- get
  return $ ProvenIssue (setHeaderChainId chainId header) (Issue n d q dec r scriptOpt) proofs
  where getScriptOpt = do
          nonEmpty <- (1 ==) . fromIntegral <$> getWord8
          if nonEmpty
            then do
              len <- fromIntegral <$> getInt16be
              bs <- getByteString len
              return $ Just bs
            else return Nothing
txDecoder ReissueID 1 = do
  signature <- get
  skip 1 --typeId
  (header, reissue) <- reissueParseBase
  return $ SignedReissue header reissue signature
txDecoder ReissueID 2 = do
  skip 1 --chainId
  (header, reissue) <- reissueParseBase
  proofs <- get
  return $ ProvenReissue header reissue proofs
txDecoder BurnID 1 = do
  (header, burn) <- burnParseBase
  signature <- get
  return $ SignedBurn header burn signature
txDecoder BurnID 2 = do
  chainId <- BS.w2c <$> getWord8
  (header, burn) <- burnParseBase
  proofs <- get
  return $ ProvenBurn (setHeaderChainId chainId header) burn proofs
txDecoder LeaseID 1 = do
  (header, lease) <- leaseParseBase
  signature <- get
  return $ SignedLease header lease signature
txDecoder LeaseID 2 = do
  skip 1 -- placeholder
  (header, lease) <- leaseParseBase
  proofs <- get
  return $ ProvenLease header lease proofs
txDecoder LeaseCancelID 1 = do
  (header, leaseCancel) <- leaseCancelParseBase
  signature <- get
  return $ SignedLeaseCancel header leaseCancel signature
txDecoder LeaseCancelID 2 = do
  chainId <- BS.w2c <$> getWord8
  (header, leaseCancel) <- leaseCancelParseBase
  proofs <- get
  return $ ProvenLeaseCancel (setHeaderChainId chainId header) leaseCancel proofs
txDecoder CreateAliasID 1 = do
  (header, ca) <- createAliasParseBase
  signature <- get
  return $ SignedCreateAlias header ca signature
txDecoder CreateAliasID 2 = do
  (header, ca) <- createAliasParseBase
  proofs <- get
  return $ ProvenCreateAlias header ca proofs
txDecoder SponsorFeeID 1 = do
  skip 2
  sender <- get
  asset <- get
  minFee <- getInt64
  feeAmount <- getInt64
  timestamp <- getInt64
  let header = TxHeader Nothing sender feeAmount Waves timestamp
      sponsor = SponsorFee asset minFee
  proofs <- get
  return $ ProvenSponsorFee header sponsor proofs
txDecoder MassPaymentID 1 = do
  skip 1
  sender <- get
  asset <- get
  cnt <- fromIntegral <$> getInt16be
  pmts <- replicateM cnt getPayment
  timestamp <- getInt64
  feeAmount <- getInt64
  len <- fromIntegral <$> getInt16be
  att <- getByteString len
  proofs <- get
  let chainId = chainIdFromAddressOrAlias (ptRecipient $ Prelude.head pmts)
      header = TxHeader (Just chainId) sender feeAmount Waves timestamp
      massPayment = MassPayment asset pmts att
  return $ ProvenMassPayment header massPayment proofs
  where getPayment = do
          recipient <- get
          amount <- getInt64
          return $ Payment recipient amount
txDecoder DataID 1 = do
  sender <- get
  cnt <- fromIntegral <$> getInt16be
  ents <- replicateM cnt getEntry
  timestamp <- getInt64
  feeAmount <- getInt64
  let header = TxHeader Nothing sender feeAmount Waves timestamp
      data' = Data ents
  proofs <- get
  return $ ProvenData header data' proofs
txDecoder SetScriptID 1 = do
  cId <- get
  sender <- get
  empty <- (0 ==) <$> getWord8
  maybeScript <-
    if empty
      then return Nothing
      else do
        len <- fromIntegral <$> getInt16be
        Just <$> getByteString len
  feeAmount <- getInt64
  timestamp <- getInt64
  let header = TxHeader (Just cId) sender feeAmount Waves timestamp
      setScript = SetScript maybeScript
  proofs <- get
  return $ ProvenSetScript header setScript proofs
txDecoder ExchangeID 1 = do
  skip 8
  o1 @ (SignedOrder ord _) <- parseSignedOrder
  o2 <- parseSignedOrder
  price <- getInt64
  amount <- getInt64
  buyFee <- getInt64
  sellFee <- getInt64
  feeAmount <- getInt64
  timestamp <- getInt64
  let header = TxHeader Nothing (ordMatcher ord) feeAmount Waves timestamp
      exchange = Exchange o1 o2 amount price buyFee sellFee
  signature <- get
  return $ SignedExchange header exchange signature
txDecoder ExchangeID 2 = do
  skip 4
  o1 <- parseOrderFromTxBytes
  skip 4
  o2 <- parseOrderFromTxBytes
  price <- getInt64
  amount <- getInt64
  buyFee <- getInt64
  sellFee <- getInt64
  feeAmount <- getInt64
  timestamp <- getInt64
  let header = TxHeader Nothing (getMatcher o1) feeAmount Waves timestamp
      exchange = Exchange o1 o2 amount price buyFee sellFee
  proofs <- get
  return $ ProvenExchange header exchange proofs
  where getMatcher (SignedOrder ord _) = ordMatcher ord
        getMatcher (ProvenOrder ord _) = ordMatcher ord
txDecoder id v = fail $ "Unknow tx (type, version) - (" ++ show id ++ ", " ++ show v ++ ")"

transferParseBase :: Get (TxHeader, Transfer)
transferParseBase = do
  sender <- label "Sender" get
  asset <- label "asset" get
  feeAsset <- label "fee asset" get
  timestamp <- label "timestamp" getInt64
  amount <- getInt64
  feeAmount <- label (show sender) getInt64
  recipient <- label (show sender) get
  attLen <- fromIntegral <$> getInt16be
  att <- getByteString attLen
  let chainId = chainIdFromAddressOrAlias recipient
      header = TxHeader (Just chainId) sender feeAmount feeAsset timestamp
      transfer = Transfer recipient asset amount att
  return (header, transfer)

issueParseBase :: Get (TxHeader, Issue)
issueParseBase = do
  sender <- get
  name <- getText
  desc <- getText
  quantity <- getInt64
  decimals <- fromIntegral <$> getWord8
  reissuable <- (1 ==) <$> getWord8
  feeAmount <- getInt64
  timestamp <- getInt64
  let header = TxHeader Nothing sender feeAmount Waves timestamp
      issue = Issue name desc quantity decimals reissuable Nothing
  return (header, issue)

reissueParseBase :: Get (TxHeader, Reissue)
reissueParseBase = do
  sender <- get
  asset <- get
  quantity <- getInt64
  reissuable <- (1 ==) <$> getWord8
  feeAmount <- getInt64
  timestamp <- getInt64
  let header = TxHeader Nothing sender feeAmount Waves timestamp
      reissue = Reissue asset quantity reissuable
  return (header, reissue)

burnParseBase :: Get (TxHeader, Burn)
burnParseBase = do
  sender <- get
  asset <- get
  quantity <- getInt64
  feeAmount <- getInt64
  timestamp <- getInt64
  let header = TxHeader Nothing sender feeAmount Waves timestamp
      burn = Burn asset quantity
  return (header, burn)

leaseParseBase :: Get (TxHeader, Lease)
leaseParseBase = do
  sender <- get
  recipient <- get
  leaseAmount <- getInt64
  feeAmount <- getInt64
  timestamp <- getInt64
  let chainId = chainIdFromAddressOrAlias recipient
      header = TxHeader (Just chainId) sender feeAmount Waves timestamp
      lease = Lease leaseAmount recipient
  return (header, lease)

leaseCancelParseBase :: Get (TxHeader, LeaseCancel)
leaseCancelParseBase = do
  sender <- get
  feeAmount <- getInt64
  timestamp <- getInt64
  leaseId <- getByteString 32
  let header = TxHeader Nothing sender feeAmount Waves timestamp
      leaseCancel = LeaseCancel leaseId
  return (header, leaseCancel)

createAliasParseBase :: Get (TxHeader, CreateAlias)
createAliasParseBase = do
  sender <- get
  skip 2
  alias <- get
  feeAmount <- getInt64
  timestamp <- getInt64
  let header = TxHeader Nothing sender feeAmount Waves timestamp
      ca = CreateAlias alias
  return (header, ca)

getText = do
  len <- fromIntegral <$> getInt16be
  decodeUtf8 <$> getByteString len

getEntry :: Get DataEntry
getEntry = do
  key <- getText
  typeId <- getWord8
  case typeId of
    0 -> IntEntry key . fromIntegral <$> getInt64be
    1 -> BoolEntry key . (1 ==) <$> getWord8
    2 -> do
      len <- fromIntegral <$> getInt16be
      BinEntry key <$> getByteString len
    3 -> StrEntry key <$> getText
    i -> fail $ "Unknown value type id - " ++ show i

parseOrderFromTxBytes :: Get Order
parseOrderFromTxBytes = do
  ver <- lookAhead getWord8
  if ver == 1
    then do
      skip 1
      parseSignedOrder
    else parseProvenOrder

parseSignedOrder :: Get Order
parseSignedOrder = do
  sender <- get
  matcher <- get
  amountAsset <- get
  priceAsset <- get
  ordType <- getOrderType
  price <- getInt64
  amount <- getInt64
  timestamp <- getInt64
  expiration <- getInt64
  matcherFee <- getInt64
  signature <- get
  return $ SignedOrder (OrderPayload sender matcher (AssetPair priceAsset amountAsset) ordType price amount matcherFee expiration timestamp) signature

parseProvenOrder :: Get Order
parseProvenOrder = do
  skip 1
  sender <- get
  matcher <- get
  amountAsset <- get
  priceAsset <- get
  ordType <- getOrderType
  price <- getInt64
  amount <- getInt64
  timestamp <- getInt64
  expiration <- getInt64
  matcherFee <- getInt64
  proofs <- get
  return $ ProvenOrder (OrderPayload sender matcher (AssetPair priceAsset amountAsset) ordType price amount matcherFee expiration timestamp) proofs

getOrderType :: Get OrderType
getOrderType = do
  otb <- getWord8
  case otb of
    0 -> return BUY
    1 -> return SELL
    _ -> fail "Unknown order type"

getInt64 :: Get Int
getInt64 = fromIntegral <$> getInt64be
