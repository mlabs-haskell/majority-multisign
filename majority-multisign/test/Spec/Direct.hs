{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Direct (tests) where

import Data.ByteString qualified as ByteString
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Ratio ((%))
import Data.Semigroup (sconcat)
import Data.Semigroup.Foldable.Class (Foldable1 (fold1))
import Data.String (IsString)
import Data.Text qualified as Text
import Data.Word (Word8)
import Ledger (
  PaymentPubKey (PaymentPubKey),
  PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),
  paymentPubKeyHash,
 )
import Ledger.Crypto (PubKey (PubKey))
import MajorityMultiSign.Contracts (multiSignTokenName)
import MajorityMultiSign.OnChain (mkValidator, validator, isUnderSizeLimitCompiled)
import MajorityMultiSign.Schema qualified as Schema
import Plutus.V1.Ledger.Api (fromBytes)
import Plutus.V1.Ledger.Scripts (Validator (getValidator))
import Plutus.V1.Ledger.Value (AssetClass, Value, assetClass, assetClassValue)
import PlutusTx qualified
import PlutusTx.List.Natural qualified as Natural
import PlutusTx.Natural (nat)
import Test.Plutus.ContextBuilder (
  ContextBuilder,
  Purpose (ForSpending),
  ValidatorUTXO (ValidatorUTXO),
  signedWith,
  validatorOutput,
 )
import Test.QuickCheck (Gen, oneof, shrinkList, sublistOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Script.Property (scriptProperty)
import Test.Tasty.Plutus.Script.Size (fitsInto, fitsOnChain, kbytes)
import Test.Tasty.Plutus.Script.Unit (
  shouldValidate,
  shouldn'tValidate,
 )
import Test.Tasty.Plutus.TestData (
  Generator (GenForSpending),
  Methodology (Methodology),
  Outcome (Fail, Pass),
  TestData (SpendingTest),
  TestItems (
    ItemsForSpending,
    spendCB,
    spendDatum,
    spendOutcome,
    spendRedeemer,
    spendValue
  ),
 )
import Test.Tasty.Plutus.TestScript (toTestValidator, mkTestValidatorUnsafe)
import Test.Tasty.Plutus.WithScript (WithScript, withTestScript)
import Prelude hiding (pred)

deriving newtype instance IsString PaymentPubKeyHash

tests :: TestTree
tests =
  testGroup
    "Direct"
    [ test "use" $
        do
          testFullUse "by zero signatories" []
          testFullUse "by a single signatory" ["4615"]
          testFullUse "by three signatories" ["4615", "0000", "d00d"]
          testPartialUse
            "by two outa three signatories"
            True
            ["4615", "d00d"]
            ["4615", "0000", "d00d"]
          testPartialUse
            "by one outa three signatories"
            False
            ["d00d"]
            ["4615", "0000", "d00d"]
          testPartialUse "by a single missing signatory" False [] ["4615"]
          testPartialUse
            "by a single repeated minority signatory"
            False
            ["4615"]
            ["4615", "4615", "4615", "0000", "d00d"]
          testPartialUse
            "by a single repeated transaction signatory"
            False
            ["4615", "4615", "4615"]
            ["4615", "0000", "d00d"]
    , test "update" $
        do
          testUpdate "add a lone signatory" True ["4615"] ["4615"] []
          testUpdate
            "add another signatory"
            True
            ["4615", "0000"]
            ["4615", "0000"]
            ["4615"]
          testUpdate
            "add an absent signatory"
            False
            ["4615"]
            ["4615", "0000"]
            ["4615"]
          testUpdate
            "drop one of two signatories"
            True
            ["4615"]
            ["4615"]
            ["4615", "0000"]
          testUpdate "drop the last signatory" False [] [] ["4615"]
          testUpdate
            "voluntarily drop the last signatory"
            True
            ["4615"]
            []
            ["4615"]
          testUpdate
            "drop one of three signatories"
            True
            ["4615", "d00d"]
            ["4615", "d00d"]
            ["4615", "d00d", "0000"]
          testUpdate
            "drop two of three signatories, one voluntarily"
            True
            ["4615", "d00d"]
            ["4615"]
            ["4615", "d00d", "0000"]
          testUpdate
            "drop two of three signatories, neither voluntarily"
            False
            ["4615"]
            ["4615"]
            ["4615", "d00d", "0000"]
          testUpdate
            "replace one of two signatories"
            True
            ["4615", "d00d"]
            ["4615", "d00d"]
            ["4615", "0000"]
    , test "bad context" $
        do
          testMissingOutputUse
            "use with a missing transaction output and a single signatory"
            ("4615" :| [])
          testMissingOutputUse
            "use with a missing transaction output and three signatories"
            ("4615" :| ["0000", "d00d"])
          testMissingOutputUpdate
            "update with a missing transaction output and a single signatory"
            ("4615" :| [])
            ["4615", "d00d"]
          testMissingOutputUpdate
            "update with a missing transaction output and three signatories"
            ("4615" :| ["0000", "d00d"])
            ["4615", "d00d"]
    , test "property" $
        do
          testProperty
            "non-repeating PubKeyHashes"
            transactionSignatories
            possibleNewSignatories
            possibleDatumSignatories
          testProperty
            "up to 50 potentially repeating PubKeyHashes in datum and redeemer"
            transactionSignatories
            (cycled50 possibleNewSignatories)
            (cycled50 possibleDatumSignatories)
          testProperty
            "up to 100 potentially repeating PubKeyHashes in datum"
            transactionSignatories
            possibleNewSignatories
            (cycled100 possibleDatumSignatories)
          testProperty
            "up to 100 potentially repeating PubKeyHashes in datum and redeemer"
            transactionSignatories
            (cycled100 possibleNewSignatories)
            (cycled100 possibleDatumSignatories)
          testProperty
            "up to 100 potentially repeating PubKeyHashes in datum, redeemer, and transaction signatures"
            (cycled100 transactionSignatories)
            (cycled100 possibleNewSignatories)
            (cycled100 possibleDatumSignatories)
    , testGroup
        "size"
        [ fitsOnChain "validator" validatorScript
        , fitsInto "validator" (kbytes [nat| 5 |]) validatorScript
        ]
    ]
  where
    test desc =
      withTestScript
        desc
        ( mkTestValidatorUnsafe @Schema.MajorityMultiSignDatum @Schema.MajorityMultiSignRedeemer
            ( $$(PlutusTx.compile [||mkValidator||])
                `PlutusTx.applyCode` isUnderSizeLimitCompiled
                `PlutusTx.applyCode` PlutusTx.liftCode initialParams
            )
            $$(PlutusTx.compile [||toTestValidator||])
        )
    validatorScript = getValidator (validator initialParams)

arbitraryDatumFrom :: [PaymentPubKeyHash] -> Gen Schema.MajorityMultiSignDatum
arbitraryDatumFrom sigs =
  Schema.MajorityMultiSignDatum . Natural.take Schema.maximumSigners
    <$> sublistOf sigs

arbitraryTransactionFrom ::
  [PaymentPubKeyHash] ->
  [PaymentPubKeyHash] ->
  [PaymentPubKeyHash] ->
  Gen
    ( Schema.MajorityMultiSignDatum
    , Schema.MajorityMultiSignRedeemer
    , Value
    , ContextBuilder TestPurpose
    )
arbitraryTransactionFrom currentSigs newSigs knownSigs = do
  datum <- arbitraryDatumFrom knownSigs
  redeemer <- arbitraryRedeemerFrom newSigs
  let value = oneshotValue
      context =
        fold1 (pays :| (signedWith' <$> currentSigs))
      pays = case PlutusTx.fromData (PlutusTx.toData redeemer) of
        Just Schema.UseSignaturesAct -> validatorOutput "payout" (ValidatorUTXO datum value)
        Just (Schema.UpdateKeysAct keys) ->
          validatorOutput "MMS" (ValidatorUTXO (Schema.MajorityMultiSignDatum keys) value)
        Nothing -> error "Unexpected redeemer type"
  pure (datum, redeemer, value, context)

shrinkTransaction ::
  ( Schema.MajorityMultiSignDatum
  , Schema.MajorityMultiSignRedeemer
  , Value
  , ContextBuilder TestPurpose
  ) ->
  [ ( Schema.MajorityMultiSignDatum
    , Schema.MajorityMultiSignRedeemer
    , Value
    , ContextBuilder TestPurpose
    )
  ]
shrinkTransaction (datum, redeemer, value, context) =
  [ (datum', redeemer', value, context)
  | datum' <- shrinkDatum datum
  , redeemer' <- shrinkRedeemer redeemer
  ]

shrinkDatum :: Schema.MajorityMultiSignDatum -> [Schema.MajorityMultiSignDatum]
shrinkDatum Schema.MajorityMultiSignDatum {signers} =
  Schema.MajorityMultiSignDatum <$> shrinkList (const []) signers

arbitraryRedeemerFrom :: [PaymentPubKeyHash] -> Gen Schema.MajorityMultiSignRedeemer
arbitraryRedeemerFrom sigs =
  oneof
    [ pure Schema.UseSignaturesAct
    , Schema.UpdateKeysAct . Natural.take Schema.maximumSigners <$> sublistOf sigs
    ]

shrinkRedeemer ::
  Schema.MajorityMultiSignRedeemer ->
  [Schema.MajorityMultiSignRedeemer]
shrinkRedeemer Schema.UseSignaturesAct = []
shrinkRedeemer (Schema.UpdateKeysAct keys) =
  Schema.UpdateKeysAct <$> shrinkList (const []) keys

cycled50 :: [a] -> [a]
cycled50 = take 50 . cycle

cycled100 :: [a] -> [a]
cycled100 = take 100 . cycle

transactionSignatories :: [PaymentPubKeyHash]
transactionSignatories = byteHash <$> [0 .. 20]

possibleDatumSignatories :: [PaymentPubKeyHash]
possibleDatumSignatories = byteHash <$> [5 .. 25]

possibleNewSignatories :: [PaymentPubKeyHash]
possibleNewSignatories = byteHash <$> [10 .. 30]

byteHash :: Word8 -> PaymentPubKeyHash
byteHash = paymentPubKeyHash . PaymentPubKey . PubKey . fromBytes . ByteString.singleton

type TestPurpose = 'ForSpending Schema.MajorityMultiSignDatum Schema.MajorityMultiSignRedeemer

testFullUse :: String -> [PaymentPubKeyHash] -> WithScript TestPurpose ()
testFullUse description signatories =
  testPartialUse description True signatories signatories

testPartialUse ::
  String ->
  Bool ->
  [PaymentPubKeyHash] ->
  [PaymentPubKeyHash] ->
  WithScript TestPurpose ()
testPartialUse description positive currentSignatories knownSignatories =
  (if positive then shouldValidate else shouldn'tValidate)
    description
    (SpendingTest datum Schema.UseSignaturesAct mempty)
    ( fold1
        ( validatorOutput "MMS" (ValidatorUTXO datum oneshotValue)
            :| (signedWith' <$> currentSignatories)
        )
    )
  where
    datum = Schema.MajorityMultiSignDatum knownSignatories

testUpdate ::
  String ->
  Bool ->
  [PaymentPubKeyHash] ->
  [PaymentPubKeyHash] ->
  [PaymentPubKeyHash] ->
  WithScript TestPurpose ()
testUpdate description positive currentSignatories newSignatories knownSignatories =
  (if positive then shouldValidate else shouldn'tValidate)
    description
    (SpendingTest oldDatum (Schema.UpdateKeysAct newSignatories) mempty)
    ( fold1
        ( validatorOutput "MMS" (ValidatorUTXO newDatum oneshotValue)
            :| (signedWith' <$> currentSignatories)
        )
    )
  where
    oldDatum = Schema.MajorityMultiSignDatum knownSignatories
    newDatum = Schema.MajorityMultiSignDatum newSignatories

testMissingOutputUse :: String -> NonEmpty PaymentPubKeyHash -> WithScript TestPurpose ()
testMissingOutputUse description signatories =
  shouldn'tValidate
    description
    (SpendingTest datum Schema.UseSignaturesAct mempty)
    (sconcat $ signedWith' <$> signatories)
  where
    datum = Schema.MajorityMultiSignDatum (toList signatories)

testMissingOutputUpdate ::
  String ->
  NonEmpty PaymentPubKeyHash ->
  [PaymentPubKeyHash] ->
  WithScript TestPurpose ()
testMissingOutputUpdate description signatories newSignatories =
  shouldn'tValidate
    description
    (SpendingTest oldDatum (Schema.UpdateKeysAct newSignatories) mempty)
    (sconcat $ signedWith' <$> signatories)
  where
    oldDatum = Schema.MajorityMultiSignDatum (toList signatories)

testProperty ::
  String ->
  [PaymentPubKeyHash] ->
  [PaymentPubKeyHash] ->
  [PaymentPubKeyHash] ->
  WithScript TestPurpose ()
testProperty desc currentSignatories newSignatories knownSignatories =
  scriptProperty
    desc
    ( GenForSpending
        (Methodology (arbitraryTransactionFrom currentSignatories newSignatories knownSignatories) shrinkTransaction)
        grade
    )
  where
    grade ::
      (Schema.MajorityMultiSignDatum, Schema.MajorityMultiSignRedeemer, Value, ContextBuilder TestPurpose) ->
      TestItems TestPurpose
    grade (datum@Schema.MajorityMultiSignDatum {signers}, redeemer, value, context)
      | length (currentSignatories `intersection` signers)
          < ceiling (length (nub signers) % 2) =
        ItemsForSpending
          { spendDatum = datum
          , spendRedeemer = redeemer
          , spendValue = value
          , spendCB = context
          , spendOutcome = Fail
          }
    grade (datum, redeemer@Schema.UseSignaturesAct, value, context) =
      ItemsForSpending
        { spendDatum = datum
        , spendRedeemer = redeemer
        , spendValue = value
        , spendCB = context
        , spendOutcome = Pass
        }
    grade (datum@Schema.MajorityMultiSignDatum {signers}, redeemer@(Schema.UpdateKeysAct keys), value, context)
      | keys `subset` (currentSignatories ++ signers) =
        ItemsForSpending
          { spendDatum = datum
          , spendRedeemer = redeemer
          , spendValue = value
          , spendCB = context
          , spendOutcome = Pass
          }
      | otherwise =
        ItemsForSpending
          { spendDatum = datum
          , spendRedeemer = redeemer
          , spendValue = value
          , spendCB = context
          , spendOutcome = Fail
          }
    intersection xs = nub . filter (`elem` xs)
    subset xs ys = all (`elem` ys) xs

{-# INLINEABLE initialParams #-}
initialParams :: Schema.MajorityMultiSignValidatorParams
initialParams = Schema.MajorityMultiSignValidatorParams oneshotAsset

oneshotAsset :: AssetClass
oneshotAsset = assetClass oneshotCS multiSignTokenName
  where
    oneshotCS = "feed"

oneshotValue :: Value
oneshotValue = assetClassValue oneshotAsset 1

signedWith' :: PaymentPubKeyHash -> ContextBuilder p
signedWith' ppkh = signedWith name pkh
  where
    pkh = unPaymentPubKeyHash ppkh
    name = Text.pack (show pkh)
