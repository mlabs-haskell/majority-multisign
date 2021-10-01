module Spec.SetSignature (tests) where

import Data.Monoid (Last (..))
import MajorityMultiSign (endpoints)
import MajorityMultiSign.Schema (
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
 )
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import Test.Tasty (TestTree, testGroup)
import Wallet.Emulator.Wallet (knownWallet)
import PlutusTx.Prelude

tests :: TestTree
tests =
  testGroup
    "SetSignature"
    []

deployer :: Wallet
deployer = knownWallet 1

deployedWith :: [Wallet] -> EmulatorTrace MajorityMultiSignIdentifier
deployedWith ws = do
  h <- activateContractWallet deployer endpoints
  callEndpoint @"Initialize" h $ MajorityMultiSignDatum $ pubKeyHash . walletPubKey <$> ws

  (Last mAsset) <- observableState h
  asset <- fromMaybe (throwError $ GenericError "Failed to initialize, no asset created") return mAsset

  let params = MajorityMultiSignValidatorParams asset
  return $ MajorityMultiSignIdentifier (validatorHash params) asset
