module Spec.Integration (tests) where

import Control.Lens ((.~))
import Control.Monad (void)
import Data.Default (def)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (AssetClass, Datum (Datum), PubKeyHash, Value)
import Ledger qualified
import Ledger.CardanoWallet qualified as CardanoWallet
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
import Wallet.Emulator.Wallet (walletMockWallet)
import Prelude qualified as P

-- Since we can't test multisignature with emulator trace, we'll be using a single signer
signer :: Test.Wallet
signer = Test.knownWallet 1

nonSigner :: Test.Wallet
nonSigner = Test.knownWallet 2

signerPkh :: PubKeyHash
signerPkh = Test.walletPubKeyHash signer

correctContract' :: Contract () Empty ContractError ()
correctContract' = correctContract integrationParams

correctContract0 :: Contract () Empty ContractError ()
correctContract0 = correctContract integrationParams{pubKeys = []}

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
        [signerPkh]
        "Main multisign check"
        (Test.assertDone correctContract' (Emulator.walletInstanceTag signer) (const True) "Couldn't mint value")
        (traceWrapper correctContract')
    , checkPredicateMMS
        []
        "Zero signers check"
        (Test.assertDone correctContract0 (Emulator.walletInstanceTag signer) (const True) "Couldn't mint value")
        (traceWrapper correctContract0)
    , checkPredicateMMS
        [signerPkh]
        "Invalid multisign check"
        (Test.assertContractError bypassContract' (Emulator.walletInstanceTag signer) (P.== expectedBypassError) "Minted value incorrectly")
        (traceWrapper bypassContract')
    , checkPredicateMMS
        [signerPkh]
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
    , ownPubKey = walletPubKey signer
    , pubKeys = [walletPubKey signer]
    }

checkPredicateMMS :: [PubKeyHash] -> P.String -> Test.TracePredicate -> Trace.EmulatorTrace () -> TestTree
checkPredicateMMS signers = Test.checkPredicateOptions $ Test.defaultCheckOptions & Test.emulatorConfig .~ emuConfig signers

multisignExampleDatum :: [PubKeyHash] -> Datum
multisignExampleDatum = Datum . PlutusTx.toBuiltinData . MajorityMultiSignDatum

emuConfig :: [PubKeyHash] -> Emulator.EmulatorConfig
emuConfig signers =
  addressValueOptions
    [(signerPkh, lovelaceValueOf 1_000_000)]
    [(validatorHashFromIdentifier exampleMMS, assetClassValue multisignTokenAssetClass 1, multisignExampleDatum signers)]

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

-- TODO: Once walletPubKey is re-added to Plutus.Contract.Test import it thence
-- and remove this definition (https://github.com/input-output-hk/plutus-apps/pull/105).
walletPubKey :: Test.Wallet -> Ledger.PubKey
walletPubKey w =
  CardanoWallet.pubKey $
    fromMaybe
      ( P.error $
          "Wallet.Emulator.Wallet.walletPubKey: Wallet "
            <> P.show w
            <> " is not a mock wallet"
      )
      $ walletMockWallet w
