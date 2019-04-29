module Waves.Data.Transaction.Types where

import           Control.Monad             (replicateM)
import           Data.ByteString
import           Data.Serialize
import           Data.Text
import           Data.Text.Encoding
import           Data.Word
import           Waves.Crypto
import           Waves.Data.AddressOrAlias
import           Waves.Data.Asset
import           Waves.Data.Common
import           Waves.Data.Order
import           Waves.Data.Proofs

data TxHeader = TxHeader
  { txChainId   :: !(Maybe Char)
  , txSender    :: !PublicKey
  , txfeeAmount :: !Int
  , txfeeAsset  :: !Asset
  , txTimestamp :: !Timestamp
  } deriving (Show)

setHeaderChainId :: Char -> TxHeader -> TxHeader
setHeaderChainId cId (TxHeader _ s amount asset ts)
  = TxHeader (Just cId) s amount asset ts

data Payment = Payment
  { ptRecipient :: !AddressOrAlias
  , ptAmount    :: !Int
  } deriving (Show)

data Transfer = Transfer
  { trRecipient  :: !AddressOrAlias
  , trAsset      :: !Asset
  , trAmount     :: !Int
  , trAttachment :: !ByteString
  } deriving (Show)

data Issue = Issue
  { isName        :: !Text
  , isDescription :: !Text
  , isQuantity    :: !Int
  , isDecimals    :: !Int
  , isReissuable  :: !Bool
  , isScript      :: !(Maybe ByteString)
  } deriving (Show)

data Reissue = Reissue
  { riAssetID    :: !AssetID
  , riQuantity   :: !Int
  , riReissuable :: !Bool
  } deriving (Show)

data Burn = Burn
  { brAssetID  :: !AssetID
  , brQuantity :: !Int
  } deriving (Show)

data SponsorFee = SponsorFee
  { sfAssetID            :: !AssetID
  , sfMinSponsorAssetFee :: !Int
  } deriving (Show)

data Lease = Lease
  { leAmount    :: !Int
  , leRecipient :: !AddressOrAlias
  } deriving (Show)

newtype LeaseCancel = LeaseCancel
  { lcLeaseID :: ByteString
  } deriving (Show)

data MassPayment = MassPayment
  { mtAsset      :: !Asset
  , mtTransfers  :: ![Payment]
  , mtAttachment :: !ByteString
  } deriving (Show)

newtype CreateAlias = CreateAlias
  { caAlias :: Alias
  } deriving (Show)

data Exchange = Exchange
  { exBuyOrder       :: !Order
  , exSellOrder      :: !Order
  , exAmount         :: !Int
  , exPrice          :: !Int
  , exBuyMatcherFee  :: !Int
  , exSellMatcherFee :: !Int
  } deriving (Show)

data DataEntry
  = IntEntry !Text !Int
  | BinEntry !Text !ByteString
  | StrEntry !Text !Text
  | BoolEntry !Text !Bool
  deriving (Show)

newtype Data = Data [DataEntry] deriving (Show)

newtype SetScript = SetScript
  { ssScript  :: Maybe ByteString
  } deriving (Show)

data UnsignedTx
  = UPayment !TxHeader !Payment
  | UTransfer !TxHeader !Transfer
  | UIssue !TxHeader !Issue
  | UReissue !TxHeader !Reissue
  | UBurn !TxHeader !Burn
  | ULease !TxHeader !Lease
  | ULeaseCancel !TxHeader !LeaseCancel
  | UCreateAlias !TxHeader !CreateAlias
  | UMassPayment !TxHeader !MassPayment
  | USponsorFee !TxHeader !SponsorFee
  | UExchange !TxHeader !Exchange
  | UData !TxHeader !Data
  | USetScript !TxHeader !SetScript

data Tx
  = Genesis !Address !Int !Timestamp
  | SignedPayment !TxHeader !Payment !Signature
  | SignedTransfer !TxHeader !Transfer !Signature
  | SignedIssue !TxHeader !Issue !Signature
  | SignedReissue !TxHeader !Reissue !Signature
  | SignedBurn !TxHeader !Burn !Signature
  | SignedLease !TxHeader !Lease !Signature
  | SignedLeaseCancel !TxHeader !LeaseCancel !Signature
  | SignedCreateAlias !TxHeader !CreateAlias !Signature
  | SignedExchange !TxHeader !Exchange !Signature
  | ProvenTransfer !TxHeader !Transfer !Proofs
  | ProvenIssue !TxHeader !Issue !Proofs
  | ProvenReissue !TxHeader !Reissue !Proofs
  | ProvenBurn !TxHeader !Burn !Proofs
  | ProvenLease !TxHeader !Lease !Proofs
  | ProvenLeaseCancel !TxHeader !LeaseCancel !Proofs
  | ProvenMassPayment !TxHeader !MassPayment !Proofs
  | ProvenCreateAlias !TxHeader !CreateAlias !Proofs
  | ProvenSponsorFee !TxHeader !SponsorFee !Proofs
  | ProvenExchange !TxHeader !Exchange !Proofs
  | ProvenData !TxHeader !Data !Proofs
  | ProvenSetScript !TxHeader !SetScript !Proofs
  deriving (Show)
