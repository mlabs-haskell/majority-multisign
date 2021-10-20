{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import MajorityMultiSign.Contracts (multiSignTokenName)
import MajorityMultiSign.OnChain (mkValidator, validatorHash)
import MajorityMultiSign.Schema qualified as Schema
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Scripts (mkValidatorScript)
import Plutus.V1.Ledger.Value (AssetClass, Value, assetClass, assetClassValue)
import PlutusTx qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Plutus.Context (ExternalType (ScriptType), Input (Input), Purpose (ForSpending), addDatum, input, paysSelf, signedWith)
import Test.Tasty.Plutus.Script.Unit (TestData (SpendingTest), WithScript, shouldValidate, shouldn'tValidate, toTestValidator, withValidator)
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Majority Multisign"
    [ withValidator
        "init"
        ( mkValidatorScript
            ( $$(PlutusTx.compile [||wrap||])
                `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkValidator||])
                                        `PlutusTx.applyCode` PlutusTx.liftCode initialParams
                                     )
            )
        )
        $ do
          testFullUse "zero signatories' use" []
          testFullUse "single signatory's use" ["4615"]
          testFullUse "three signatories' use" ["4615", "0000", "d00d"]
          testPartialUse "two outa three signatories' use" True ["4615", "d00d"] ["4615", "0000", "d00d"]
          testPartialUse "one outa three signatories' use" False ["d00d"] ["4615", "0000", "d00d"]
          testPartialUse "use by a single missing signatory" False [] ["4615"]
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
    ]
  where
    wrap = toTestValidator

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
        ( input (Input (ScriptType (validatorHash initialParams) (PlutusTx.toBuiltinData datum)) oneshotValue)
            <> paysSelf oneshotValue datum
        )
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
        ( input (Input (ScriptType (validatorHash initialParams) (PlutusTx.toBuiltinData oldDatum)) oneshotValue)
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
