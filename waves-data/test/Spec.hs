import AddressOrAliasSpec
import ProofSpec

import           Test.QuickCheck

main :: IO ()
main = 
  quickCheck addressFromStringEqualsAddressFromPK >>
  quickCheck proofRoundTrip >>
  quickCheck proofsRoundTrip
