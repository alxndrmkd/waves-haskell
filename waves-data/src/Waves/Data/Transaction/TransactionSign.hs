module Waves.Data.Transaction.TransactionSign where

import           Data.ByteString
import           Data.Serialize
import           Waves.Crypto
import           Waves.Data.Transaction.Types
import           Waves.Data.Proofs

txSign :: MonadRandom m => UnsignedTx -> PrivateKey -> m Tx
txSign utx @ (UPayment header payload) xprv
  = do
  let bytes = txBodyBytes utx
  signature <- sign xprv bytes
  return (SignedPayment header payload signature)
txSign utx @ (UTransfer header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenTransfer header payload (Proofs [proof]))
txSign utx @ (UIssue header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenIssue header payload (Proofs [proof]))
txSign utx @ (UReissue header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenReissue header payload (Proofs [proof]))
txSign utx @ (UBurn header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenBurn header payload (Proofs [proof]))
txSign utx @ (ULease header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenLease header payload (Proofs [proof]))
txSign utx @ (ULeaseCancel header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenLeaseCancel header payload (Proofs [proof]))
txSign utx @ (UCreateAlias header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenCreateAlias header payload (Proofs [proof]))
txSign utx @ (UMassPayment header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenMassPayment header payload (Proofs [proof]))
txSign utx @ (USponsorFee header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenSponsorFee header payload (Proofs [proof]))
txSign utx @ (UExchange header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenExchange header payload (Proofs [proof]))
txSign utx @ (UData header payload) xprv
  =do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenData header payload (Proofs [proof]))
txSign utx @ (USetScript header payload) xprv
  = do
  let bytes = txBodyBytes utx
  proof <- proveBytes xprv bytes
  return (ProvenSetScript header payload (Proofs [proof]))

txSigVerify :: Tx -> PublicKey -> Bool
txSigVerify = undefined

txBodyBytes :: UnsignedTx -> ByteString
txBodyBytes = runPut . txBodyBytesPut

signBytes :: MonadRandom m => PrivateKey -> ByteString -> m Signature
signBytes = sign

proveBytes :: MonadRandom m => PrivateKey -> ByteString -> m Proof
proveBytes xprv bs
  = do
  signature <- sign xprv bs
  return $ ProofBytes (signatureBytes signature)

txBodyBytesPut :: UnsignedTx -> Put
txBodyBytesPut = fail "non-implemented"