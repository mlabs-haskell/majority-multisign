module Spec.Golden (tests) where

import MajorityMultiSign.Schema qualified as Schema
import Spec.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Golden (goldenData, goldenJSON, goldenToSchema)

tests :: TestTree
tests =
  testGroup
    "Golden"
    [ testGroup
        "Data"
        [ goldenData @Schema.MajorityMultiSignDatum
        , goldenData @Schema.MajorityMultiSignIdentifier
        , goldenData @Schema.MajorityMultiSignRedeemer
        , goldenData @Schema.SetSignaturesParams
        ]
    , testGroup
        "JSON"
        [ goldenJSON @Schema.MajorityMultiSignDatum
        , goldenJSON @Schema.MajorityMultiSignIdentifier
        , goldenJSON @Schema.SetSignaturesParams
        ]
    , testGroup
        "ToSchema"
        [ goldenToSchema @Schema.MajorityMultiSignIdentifier
        ]
    ]
