{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.ByteString qualified as ByteString
import Data.List (nub, sort, (\\))
import Data.Ratio ((%))
import Data.Word (Word8)
import Ledger.Crypto (PubKey (PubKey), PubKeyHash, pubKeyHash)
import MajorityMultiSign.Contracts (multiSignTokenName)
import MajorityMultiSign.OnChain (mkValidator, validatorHash)
import MajorityMultiSign.Schema qualified as Schema
import Plutus.V1.Ledger.Api (fromBytes)
import Plutus.V1.Ledger.Scripts (mkValidatorScript)
import Plutus.V1.Ledger.Value (AssetClass, Value, assetClass, assetClassValue)
import PlutusTx qualified
import Test.QuickCheck (Arbitrary (arbitrary, shrink), oneof, shrinkList, sublistOf)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  ExternalType (ScriptType),
  Input (Input),
  Purpose (ForSpending),
  addDatum,
  input,
  paysSelf,
  signedWith,
 )
import Test.Tasty.Plutus.Script.Property (scriptProperty)
import Test.Tasty.Plutus.Script.Unit (TestData (SpendingTest), WithScript, shouldValidate, shouldn'tValidate, toTestValidator, withValidator)
import Test.Tasty.Plutus.TestData (Example (Bad, Good), fromArbitrarySpending, static)
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Majority Multisign"
    [ test "use" $
        do
          testFullUse "by zero signatories" []
          testFullUse "by a single signatory" ["4615"]
          testFullUse "by three signatories" ["4615", "0000", "d00d"]
          testPartialUse "by two outa three signatories" True ["4615", "d00d"] ["4615", "0000", "d00d"]
          testPartialUse "by one outa three signatories" False ["d00d"] ["4615", "0000", "d00d"]
          testPartialUse "by a single missing signatory" False [] ["4615"]
    , test "update" $
        do
          testUpdate "add a lone signatory" True ["4615"] ["4615"] []
          testUpdate "add another signatory" True ["4615", "0000"] ["4615", "0000"] ["4615"]
          testUpdate "add an absent signatory" False ["4615"] ["4615", "0000"] ["4615"]
          testUpdate "drop one of two signatories" True ["4615"] ["4615"] ["4615", "0000"]
          testUpdate "drop the last signatory" False [] [] ["4615"]
          testUpdate "voluntarily drop the last signatory" True ["4615"] [] ["4615"]
          testUpdate "drop one of three signatories" True ["4615", "d00d"] ["4615", "d00d"] ["4615", "d00d", "0000"]
          testUpdate "drop two of three signatories, one voluntarily" True ["4615", "d00d"] ["4615"] ["4615", "d00d", "0000"]
          testUpdate "drop two of three signatories, neither voluntarily" False ["4615"] ["4615"] ["4615", "d00d", "0000"]
          testUpdate "replace one of two signatories" True ["4615", "d00d"] ["4615", "d00d"] ["4615", "0000"]
    , test "property" $
        let grade :: Schema.MajorityMultiSignDatum -> Schema.MajorityMultiSignRedeemer -> Value -> Example
            grade Schema.MajorityMultiSignDatum {signers} _redeemer _value
              | length (transactionSignatories `intersection` signers)
                  < ceiling (length signers % 2) =
                Bad
            grade _datum Schema.UseSignaturesAct {} _value = Good
            grade
              Schema.MajorityMultiSignDatum {signers}
              Schema.UpdateKeysAct {keys}
              _value
                | let newKeys = keys \\ signers
                  , newKeys `subset` transactionSignatories =
                  Good
                | otherwise = Bad
            mkContext :: TestData 'ForSpending -> ContextBuilder 'ForSpending
            mkContext (SpendingTest datum redeemer val) =
              foldr signedAnd id transactionSignatories $
                input (oneshotInput datum) <> extraContext
              where
                signedAnd sig rest = (signedWith sig <>) . rest
                extraContext = case PlutusTx.fromData (PlutusTx.toData redeemer) of
                  Just Schema.UseSignaturesAct -> paysSelf val datum
                  Just Schema.UpdateKeysAct {keys}
                    | let newDatum = Schema.MajorityMultiSignDatum keys ->
                      paysSelf val newDatum <> addDatum newDatum
                  Nothing -> error "Unexpected redeemer type"
         in scriptProperty
              "matches expectations"
              (fromArbitrarySpending grade $ static oneshotValue)
              mkContext
    ]
  where
    intersection xs = nub . sort . filter (`elem` xs)
    subset xs ys = all (`elem` ys) xs
    test desc =
      withValidator
        desc
        ( mkValidatorScript
            ( $$(PlutusTx.compile [||toTestValidator . mkValidator||])
                `PlutusTx.applyCode` PlutusTx.liftCode initialParams
            )
        )

instance Arbitrary Schema.MajorityMultiSignDatum where
  arbitrary = Schema.MajorityMultiSignDatum <$> sublistOf possibleDatumSignatories
  shrink Schema.MajorityMultiSignDatum{signers} =
    Schema.MajorityMultiSignDatum <$> shrinkList (const []) signers

instance Arbitrary Schema.MajorityMultiSignRedeemer where
  arbitrary =
    oneof
      [ pure Schema.UseSignaturesAct
      , Schema.UpdateKeysAct <$> sublistOf possibleNewSignatories
      ]
  shrink Schema.UseSignaturesAct = []
  shrink Schema.UpdateKeysAct {keys} =
    Schema.UpdateKeysAct <$> shrinkList (const []) keys

transactionSignatories :: [PubKeyHash]
transactionSignatories = byteHash <$> [0 .. 20]

possibleDatumSignatories :: [PubKeyHash]
possibleDatumSignatories = byteHash <$> [5 .. 25]

possibleNewSignatories :: [PubKeyHash]
possibleNewSignatories = byteHash <$> [10 .. 30]

byteHash :: Word8 -> PubKeyHash
byteHash = pubKeyHash . PubKey . fromBytes . ByteString.singleton

testFullUse :: String -> [PubKeyHash] -> WithScript 'ForSpending ()
testFullUse description signatories = testPartialUse description True signatories signatories

testPartialUse :: String -> Bool -> [PubKeyHash] -> [PubKeyHash] -> WithScript 'ForSpending ()
testPartialUse description positive currentSignatories knownSignatories =
  (if positive then shouldValidate else shouldn'tValidate)
    description
    (SpendingTest datum Schema.UseSignaturesAct mempty)
    ( maybe
        id
        (<>)
        (foldMap (Just . signedWith) currentSignatories)
        (input (oneshotInput datum) <> paysSelf oneshotValue datum)
    )
  where
    datum = Schema.MajorityMultiSignDatum knownSignatories

testUpdate :: String -> Bool -> [PubKeyHash] -> [PubKeyHash] -> [PubKeyHash] -> WithScript 'ForSpending ()
testUpdate description positive currentSignatories newSignatories knownSignatories =
  (if positive then shouldValidate else shouldn'tValidate)
    description
    (SpendingTest oldDatum (Schema.UpdateKeysAct newSignatories) mempty)
    ( maybe
        id
        (<>)
        (foldMap (Just . signedWith) currentSignatories)
        ( input (oneshotInput oldDatum)
            <> paysSelf oneshotValue newDatum
            <> addDatum newDatum
        )
    )
  where
    oldDatum = Schema.MajorityMultiSignDatum knownSignatories
    newDatum = Schema.MajorityMultiSignDatum newSignatories

{-# INLINEABLE initialParams #-}
initialParams :: Schema.MajorityMultiSignValidatorParams
initialParams = Schema.MajorityMultiSignValidatorParams oneshotAsset

oneshotAsset :: AssetClass
oneshotAsset = assetClass oneshotCS multiSignTokenName
  where
    oneshotCS = "feed"

oneshotValue :: Value
oneshotValue = assetClassValue oneshotAsset 1

oneshotInput :: forall datum. PlutusTx.ToData datum => datum -> Input
oneshotInput datum = Input (ScriptType (validatorHash initialParams) (PlutusTx.toBuiltinData datum)) oneshotValue
