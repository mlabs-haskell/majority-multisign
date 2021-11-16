{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Roundtrip (tests) where

import Data.ByteString (ByteString)
import Ledger.Crypto (PubKey (PubKey), PubKeyHash, pubKeyHash)
import MajorityMultiSign.Schema qualified as Schema
import Plutus.V1.Ledger.Api (BuiltinByteString, toBuiltin)
import Plutus.V1.Ledger.Value (
  AssetClass,
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  assetClass,
 )
import Test.QuickCheck (Arbitrary (arbitrary), listOf, oneof)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Laws (dataLaws, jsonLaws)
import Prelude

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

instance Arbitrary Schema.MajorityMultiSignDatum where
  arbitrary = Schema.MajorityMultiSignDatum <$> listOf arbitrary

instance Arbitrary Schema.MajorityMultiSignIdentifier where
  arbitrary = Schema.MajorityMultiSignIdentifier <$> arbitrary

instance Arbitrary Schema.MajorityMultiSignRedeemer where
  arbitrary =
    oneof
      [ pure Schema.UseSignaturesAct
      , Schema.UpdateKeysAct <$> listOf arbitrary
      ]

instance Arbitrary Schema.SetSignaturesParams where
  arbitrary =
    Schema.SetSignaturesParams
      <$> arbitrary <*> listOf arbitrary <*> listOf arbitrary

instance Arbitrary PubKeyHash where
  arbitrary = pubKeyHash <$> arbitrary

instance Arbitrary PubKey where
  arbitrary = PubKey <$> genericArbitrary

instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin <$> arbitrary @ByteString

instance Arbitrary AssetClass where
  arbitrary = assetClass <$> arbitrary <*> arbitrary

instance Arbitrary CurrencySymbol where
  arbitrary = CurrencySymbol <$> arbitrary

instance Arbitrary TokenName where
  arbitrary = TokenName <$> arbitrary
