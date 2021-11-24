{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Instances () where

import MajorityMultiSign.Schema qualified as Schema
import Plutus.V1.Ledger.Value (AssetClass, assetClass)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, listOf, oneof)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Plutus.Instances ()
import Prelude (Applicative (pure, (<*>)), (<$>))

instance Arbitrary Schema.MajorityMultiSignDatum where
  arbitrary = Schema.MajorityMultiSignDatum <$> listOf arbitrary

instance Arbitrary Schema.MajorityMultiSignIdentifier where
  arbitrary = Schema.MajorityMultiSignIdentifier <$> arbitraryAssetClass

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

arbitraryAssetClass :: Gen AssetClass
arbitraryAssetClass = assetClass <$> arbitrary <*> arbitrary
