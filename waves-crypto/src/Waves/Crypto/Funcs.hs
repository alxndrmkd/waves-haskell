module Waves.Crypto.Funcs where

import qualified Crypto.Elliptics.Curve25519 as C
import           Crypto.Hash
import           Crypto.Random
import           Data.ByteArray              (ByteArrayAccess, convert)
import qualified Data.ByteString.Char8       as BS
import           Data.Maybe
import           Waves.Crypto.Base
import           Waves.Crypto.Types

fastHash :: (ByteArrayAccess a) => a -> Digest Blake2b_256
fastHash = hashWith Blake2b_256

fastHashBS :: (ByteArrayAccess a) => a -> BS.ByteString
fastHashBS = convert . fastHash

secureHash :: (ByteArrayAccess a) => a -> Digest Keccak_256
secureHash a = hashWith Keccak_256 (fastHash a)

secureHashBS :: (ByteArrayAccess a) => a -> BS.ByteString
secureHashBS = convert . secureHash

sign :: (MonadRandom m) => PrivateKey -> BS.ByteString -> m Signature
sign xprv msg = C.сurve25519Sign xprv msg <$> getRandomBytes 64

verifySignature :: PublicKey -> Signature -> BS.ByteString -> Bool
verifySignature = C.curve25519Verify

createKeyPair :: BS.ByteString -> KeyPair
createKeyPair = C.keyPairgen . convert . hashWith SHA256
