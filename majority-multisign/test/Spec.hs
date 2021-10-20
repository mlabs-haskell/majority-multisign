{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import MajorityMultiSign.Contracts (multiSignTokenName)
import MajorityMultiSign.OnChain (mkValidator)
import MajorityMultiSign.Schema qualified as Schema
import Plutus.V1.Ledger.Scripts (mkValidatorScript)
import Plutus.V1.Ledger.Value (assetClass)
import PlutusTx qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Plutus.WithScript (toTestValidator, withValidator)
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
                `PlutusTx.applyCode` ($$(PlutusTx.compile [||mkValidator||])
                                        `PlutusTx.applyCode` PlutusTx.liftCode initialParams
                                     )
            )
        )
        $ do
          pure ()
    ]
  where
    wrap = toTestValidator

{-# INLINEABLE initialParams #-}
initialParams :: Schema.MajorityMultiSignValidatorParams
initialParams = Schema.MajorityMultiSignValidatorParams oneshotAsset
  where
    oneshotAsset = assetClass oneshotCS multiSignTokenName
    oneshotCS = undefined
