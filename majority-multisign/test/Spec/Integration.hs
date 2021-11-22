module Spec.Integration (tests) where

import Control.Lens ((.~))
import Control.Monad (void)
import Data.Default (def)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (AssetClass, Datum (Datum), PubKeyHash, Value)
import Ledger qualified
import Ledger.Scripts qualified as Scripts
import MajorityMultiSign.Contracts (multiSignTokenName)
import MajorityMultiSign.OnChain (validatorHashFromIdentifier)
import MajorityMultiSign.Schema (
  MajorityMultiSignDatum (MajorityMultiSignDatum),
  MajorityMultiSignIdentifier (MajorityMultiSignIdentifier),
 )
import Plutus.Contract (Contract, ContractError (WalletError), Empty)
import Plutus.Contract.Test qualified as Test
import Plutus.Trace qualified as Trace
import Plutus.Trace.Emulator qualified as Emulator
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Tx (Tx (txData, txMint, txOutputs))
import Plutus.V1.Ledger.Value (assetClass, assetClassValue)
import PlutusTx qualified
import PlutusTx.Prelude
import Spec.IntegrationWrappers (
  IntegrationParams (IntegrationParams, mmsId, ownPubKey, pubKeys),
  bypassContract,
  correctContract,
 )
import Test.Tasty (TestTree, testGroup)
import Wallet.Emulator.Error (WalletAPIError (ValidationError))
import Prelude qualified as P

-- Since we can't test multisignature with emulator trace, we'll be using a single signer
signer :: Test.Wallet
signer = Test.knownWallet 1

nonSigner :: Test.Wallet
nonSigner = Test.knownWallet 2

signerPkh :: PubKeyHash
signerPkh = Ledger.pubKeyHash $ Test.walletPubKey signer

correctContract' :: Contract () Empty ContractError ()
correctContract' = correctContract integrationParams

bypassContract' :: Contract () Empty ContractError ()
bypassContract' = bypassContract integrationParams

makeExpectedError :: [Text] -> ContractError
makeExpectedError xs = WalletError (ValidationError (Ledger.ScriptFailure (Scripts.EvaluationError xs "CekEvaluationFailure")))

expectedBypassError :: ContractError
expectedBypassError = makeExpectedError ["Missing Multisign Asset", "PT5"]

expectedUnsignedError :: ContractError
expectedUnsignedError = makeExpectedError ["Not enough signatures", "PT5"]

tests :: TestTree
tests =
  testGroup
    "Integration"
    [ checkPredicateMMS
        "Main multisign check"
        (Test.assertDone correctContract' (Emulator.walletInstanceTag signer) (const True) "Couldn't mint value")
        (traceWrapper correctContract')
    , checkPredicateMMS
        "Invalid multisign check"
        (Test.assertContractError bypassContract' (Emulator.walletInstanceTag signer) (P.== expectedBypassError) "Minted value incorrectly")
        (traceWrapper bypassContract')
    , checkPredicateMMS
        "Missing signer check"
        (Test.assertContractError correctContract' (Emulator.walletInstanceTag nonSigner) (P.== expectedUnsignedError) "Minted value incorrectly")
        (nonSignerTraceWrapper correctContract')
    ]

multisignTokenAssetClass :: AssetClass
multisignTokenAssetClass = assetClass "aaaa" multiSignTokenName

exampleMMS :: MajorityMultiSignIdentifier
exampleMMS = MajorityMultiSignIdentifier multisignTokenAssetClass

integrationParams :: IntegrationParams
integrationParams =
  IntegrationParams
    { mmsId = exampleMMS
    , ownPubKey = Test.walletPubKey signer
    , pubKeys = [Test.walletPubKey signer]
    }

multisignExampleDatum :: Datum
multisignExampleDatum = Datum $ PlutusTx.toBuiltinData $ MajorityMultiSignDatum [signerPkh]

emuConfig :: Emulator.EmulatorConfig
emuConfig =
  addressValueOptions
    [(signerPkh, lovelaceValueOf 1_000_000)]
    [(validatorHashFromIdentifier exampleMMS, assetClassValue multisignTokenAssetClass 1, multisignExampleDatum)]

checkPredicateMMS :: P.String -> Test.TracePredicate -> Trace.EmulatorTrace () -> TestTree
checkPredicateMMS = Test.checkPredicateOptions $ Test.defaultCheckOptions & Test.emulatorConfig .~ emuConfig

traceWrapper :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
traceWrapper contract = do
  _ <- Emulator.activateContractWallet signer contract
  void $ Emulator.waitNSlots 3

nonSignerTraceWrapper :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
nonSignerTraceWrapper contract = do
  _ <- Emulator.activateContractWallet nonSigner contract
  void $ Emulator.waitNSlots 3

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
