module Spec.Integration (tests, checkPredicateMMS) where

import Control.Lens ((.~))
import Data.Default (def)
import Data.Function ((&))
import Data.Map qualified as Map
import Ledger (AssetClass, Datum (..), PubKeyHash, Value)
import Ledger qualified
import Ledger.Scripts qualified as Scripts
import MajorityMultiSign.Contracts (multiSignTokenName)
import MajorityMultiSign.OnChain (validatorHashFromIdentifier)
import MajorityMultiSign.Schema (MajorityMultiSignDatum (..), MajorityMultiSignIdentifier (..))
import Plutus.Contract.Test qualified as Test
import Plutus.Trace qualified as Trace
import Plutus.Trace.Emulator qualified as Emulator
import Plutus.V1.Ledger.Tx (Tx (..))
import Plutus.V1.Ledger.Value (assetClass, assetClassValue)
import PlutusTx qualified
import PlutusTx.Prelude
import Spec.IntegrationWrappers ()
import Test.Tasty (TestTree, testGroup)
import Prelude qualified as P

tests :: TestTree
tests =
  testGroup
    "Integration"
    []

multisignTokenAssetClass :: AssetClass
multisignTokenAssetClass = assetClass "aaaa" multiSignTokenName

exampleMMS :: MajorityMultiSignIdentifier
exampleMMS = MajorityMultiSignIdentifier multisignTokenAssetClass

multisignExampleDatum :: Datum
multisignExampleDatum = Datum $ PlutusTx.toBuiltinData $ MajorityMultiSignDatum []

emuConfig :: Emulator.EmulatorConfig
emuConfig =
  addressValueOptions
    []
    [(validatorHashFromIdentifier exampleMMS, assetClassValue multisignTokenAssetClass 1, multisignExampleDatum)]

checkPredicateMMS :: P.String -> Test.TracePredicate -> Trace.EmulatorTrace () -> TestTree
checkPredicateMMS = Test.checkPredicateOptions $ Test.defaultCheckOptions & Test.emulatorConfig .~ emuConfig

addressValueOptions :: [(Ledger.PubKeyHash, Value)] -> [(Scripts.ValidatorHash, Value, Datum)] -> Emulator.EmulatorConfig
addressValueOptions walletAllocs validatorAllocs = Emulator.EmulatorConfig (Right [tx]) def def
  where
    tx :: Tx
    tx =
      P.mempty
        { txOutputs =
            fmap (\(pkh, val) -> Ledger.TxOut (Ledger.pubKeyHashAddress pkh) val Nothing) walletAllocs
              <> fmap (\(vh, val, d) -> Ledger.TxOut (Ledger.scriptHashAddress vh) val $ Just $ Scripts.datumHash d) validatorAllocs
        , txData =
            Map.fromList $
              (\(_, _, d) -> (Scripts.datumHash d, d)) <$> validatorAllocs
        , txMint = foldMap snd walletAllocs <> foldMap (\(_, v, _) -> v) validatorAllocs
        }
