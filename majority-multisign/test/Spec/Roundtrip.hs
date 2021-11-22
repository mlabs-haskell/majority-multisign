module Spec.Roundtrip (tests) where

import MajorityMultiSign.Schema qualified as Schema
import Spec.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Laws (dataLaws, jsonLaws)

tests :: TestTree
tests =
  testGroup
    "Round-trip"
    [ testGroup
        "Data"
        [ dataLaws @Schema.MajorityMultiSignDatum
        , dataLaws @Schema.MajorityMultiSignIdentifier
        , dataLaws @Schema.MajorityMultiSignRedeemer
        , dataLaws @Schema.SetSignaturesParams
        ]
    , testGroup
        "JSON"
        [ jsonLaws @Schema.MajorityMultiSignDatum
        , jsonLaws @Schema.MajorityMultiSignIdentifier
        , jsonLaws @Schema.SetSignaturesParams
        ]
    ]
