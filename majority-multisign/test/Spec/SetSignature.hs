module Spec.SetSignature (deployedWith, tests) where

import Control.Monad (void)
import Data.Monoid (Last (..))
import Ledger (Address, PrivateKey)
import Ledger.Contexts (pubKeyHash)
import MajorityMultiSign (endpoints)
import MajorityMultiSign.OnChain (validatorAddress, validatorHash)
import MajorityMultiSign.Schema (
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignValidatorParams (..),
  SetSignaturesParams (..),
 )
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx.Prelude hiding ((==))
import Spec.ContractExtra (dataAtComputedAddress)
import Test.Tasty (TestTree, testGroup)
import Wallet.Emulator.Wallet (WalletId (..), signPrivateKeys)
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
              endpoints
              (walletInstanceTag $ knownWallet 1)
              getAddressFromWriter
              (== walletsToDatum (knownWallet <$> [3 .. 7]))
        )
        setSignaturesTrace
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

deployedWith :: [Wallet] -> EmulatorTrace MajorityMultiSignIdentifier
deployedWith ws = do
  h <- activateContractWallet deployer endpoints
  callEndpoint @"Initialize" h $ walletsToDatum ws
  void $ waitNSlots 2

  (Last mAsset) <- observableState h
  oneshotAsset <- maybe (throwError $ GenericError "Failed to initialize, no asset created") return mAsset

  let params = MajorityMultiSignValidatorParams oneshotAsset
  return $ MajorityMultiSignIdentifier (validatorHash params) oneshotAsset

getPrivateKey :: Wallet -> PrivateKey
getPrivateKey (Wallet (MockWallet prv)) = prv
getPrivateKey (Wallet (XPubWallet _)) = error ()

setSignaturesTrace :: EmulatorTrace ()
setSignaturesTrace = do
  mmsID <- deployedWith $ knownWallet <$> [2 .. 6]

  h <- activateContractWallet (knownWallet 2) endpoints

  Emulator.setSigningProcess (knownWallet 2) $ signPrivateKeys $ getPrivateKey . knownWallet <$> [3 .. 7]
  callEndpoint @"SetSignatures" h $ SetSignaturesParams mmsID (walletsToKeys $ knownWallet <$> [3 .. 7]) (walletPubKey . knownWallet <$> [3 .. 7])
  void $ waitNSlots 2
