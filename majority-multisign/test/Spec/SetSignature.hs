module Spec.SetSignature (deployedWith, tests) where

import Control.Monad (void)
import Data.Monoid (Last (..))
import MajorityMultiSign (endpoints)
import MajorityMultiSign.Contracts (initialize)
import MajorityMultiSign.OnChain (validatorAddress, validatorHash)
import MajorityMultiSign.Schema (
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignValidatorParams (..),
  SetSignaturesParams (..),
 )
import Ledger (Address)
import Ledger.Contexts (pubKeyHash)
import Plutus.Contract.Request qualified as Contract
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass)
import Spec.ContractExtra (dataAtComputedAddress)
import Test.Tasty (TestTree, testGroup)
import PlutusTx.Prelude hiding ((==))
import Prelude ((==))

tests :: TestTree
tests =
  testGroup
    "SetSignature"
    [ checkPredicate
        "Valid sign"
        ( assertNoFailedTransactions
            .&&. dataAtComputedAddress
              @MajorityMultiSignDatum
              (endpoints <* Contract.waitNSlots 5)
              (walletInstanceTag $ knownWallet 2)
              getAddressFromWriter
              (== walletsToDatum (knownWallet <$> [3..7]))
        )
        init
    ]

getAddressFromWriter :: Last AssetClass -> Maybe Address
getAddressFromWriter (Last (Just a)) = Just $ validatorAddress $ MajorityMultiSignValidatorParams a
getAddressFromWriter _ = Nothing

deployer :: Wallet
deployer = knownWallet 1

walletsToKeys :: [Wallet] -> [PubKeyHash]
walletsToKeys = fmap $ pubKeyHash . walletPubKey

walletsToDatum :: [Wallet] -> MajorityMultiSignDatum
walletsToDatum = MajorityMultiSignDatum . walletsToKeys

init :: EmulatorTrace ()
init = do
  h <- activateContractWallet deployer (endpoints <* Contract.waitNSlots 5)
  callEndpoint @"Initialize" h $ walletsToDatum $ knownWallet <$> [2..6]
  void $ waitNSlots 2

deployedWith :: [Wallet] -> EmulatorTrace MajorityMultiSignIdentifier
deployedWith ws = do
  h <- activateContractWallet deployer endpoints
  callEndpoint @"Initialize" h $ walletsToDatum ws
  void $ waitNSlots 2

  (Last mAsset) <- observableState h
  oneshotAsset <- maybe (throwError $ GenericError "Failed to initialize, no asset created") return mAsset

  let params = MajorityMultiSignValidatorParams oneshotAsset
  return $ MajorityMultiSignIdentifier (validatorHash params) oneshotAsset

setSignaturesTrace :: EmulatorTrace ()
setSignaturesTrace = do
  mmsID <- deployedWith $ knownWallet <$> [2..6]

  h <- activateContractWallet (knownWallet 2) endpoints
  
  callEndpoint @"SetSignatures" h $ SetSignaturesParams mmsID $ walletsToKeys $ knownWallet <$> [3..7]
  void $ waitNSlots 2
