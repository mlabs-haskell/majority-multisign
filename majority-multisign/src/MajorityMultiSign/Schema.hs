{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- record-dot-preprocessor creates code that violates this warning, disable for this file
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
-- disabling orphans warning to be able to write ToArgument instances
{-# OPTIONS_GHC -Wno-orphans #-}
-- core is not inlineable without this
{-# OPTIONS_GHC -fno-specialise #-}

module MajorityMultiSign.Schema (
  MajorityMultiSign,
  MajorityMultiSignDatum (MajorityMultiSignDatum, signers),
  MajorityMultiSignIdentifier (MajorityMultiSignIdentifier, asset),
  MajorityMultiSignRedeemer (UpdateKeysAct, UseSignaturesAct),
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (MajorityMultiSignValidatorParams, asset),
  SetSignaturesParams (SetSignaturesParams, mmsIdentifier, newKeys, pubKeys),
  getMinSigners,
  naturalLength,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Extras (encodeByteString)
import Data.Functor.Foldable (Fix (Fix))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Semigroup (Sum (Sum, getSum))
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Ledger.Crypto (PubKey)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Endpoint, type (.\/))
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol (CurrencySymbol), TokenName (TokenName))
import PlutusTx qualified
import PlutusTx.NatRatio (NatRatio, ceiling, frac, fromNatural)
import PlutusTx.Natural (Natural, nat)
import PlutusTx.Prelude hiding (Eq, decodeUtf8)
import Schema (
  FormArgumentF (FormHexF, FormMaybeF, FormObjectF, FormStringF),
  FormSchema (FormSchemaMaybe, FormSchemaObject),
  ToArgument (toArgument),
  ToSchema (toSchema),
 )
import Prelude (Eq, Show)

{-# INLINEABLE signReq #-}

-- | Signing proportion required
signReq :: NatRatio
signReq = [frac| (1, 2) |] -- 0.5

{-# INLINEABLE naturalLength #-}

-- | A count of the items in a `Foldable` is always 'Natural'.
naturalLength :: Foldable t => t a -> Natural
naturalLength = getSum . foldMap (Sum . const [nat| 1 |])

{-# INLINEABLE getMinSigners #-}

-- | Given a list of Signers, gets the minimum number of signers needed for a transaction to be valid
getMinSigners :: [a] -> Natural
getMinSigners = ceiling . (signReq *) . fromNatural . naturalLength

-- | Data type used to identify a majority multisign validator (the asset needed to call it)
newtype MajorityMultiSignIdentifier = MajorityMultiSignIdentifier
  { asset :: AssetClass
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, OpenApi.ToSchema, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignIdentifier
PlutusTx.makeLift ''MajorityMultiSignIdentifier

instance ToSchema MajorityMultiSignIdentifier where
  toSchema =
    FormSchemaObject [("asset", FormSchemaMaybe (toSchema @AssetClass))]

instance ToArgument MajorityMultiSignIdentifier where
  toArgument MajorityMultiSignIdentifier {asset} =
    Fix $ FormObjectF [("asset", toArgument (Just asset))]

-- | Params to the majority multisign validator, as the asset class of the `MajorityMultiSignDatum`
newtype MajorityMultiSignValidatorParams = MajorityMultiSignValidatorParams
  { asset :: AssetClass
  }

instance ToArgument a => ToArgument (Maybe a) where
  toArgument = Fix . FormMaybeF (toSchema @a) . fmap toArgument

deriving anyclass instance ToArgument AssetClass

instance ToArgument CurrencySymbol where
  toArgument (CurrencySymbol builtinBS) =
    Fix $ FormHexF $ Just $ Text.unpack $ encodeByteString $ fromBuiltin builtinBS

instance ToArgument TokenName where
  toArgument (TokenName builtinBS) =
    Fix $ FormStringF $ Just $ Text.unpack $ decodeUtf8 $ fromBuiltin builtinBS

PlutusTx.makeLift ''MajorityMultiSignValidatorParams

{- | Datum held by the validator, storing the pub keys of the signatures needed
  This is also used as the params to the initialize endpoint
-}
newtype MajorityMultiSignDatum = MajorityMultiSignDatum
  { signers :: [PubKeyHash]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignDatum

-- | Redeemer of the validator, allowing for simple use (not modifying datum), or key updating
data MajorityMultiSignRedeemer
  = UseSignaturesAct
  | UpdateKeysAct [PubKeyHash]
  deriving stock (Eq, Show)

PlutusTx.unstableMakeIsData ''MajorityMultiSignRedeemer

-- | Params to the set signature endpoint
data SetSignaturesParams = SetSignaturesParams
  { mmsIdentifier :: MajorityMultiSignIdentifier
  , newKeys :: [PubKeyHash]
  , pubKeys :: [PubKey]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''SetSignaturesParams

data MajorityMultiSign

instance Scripts.ValidatorTypes MajorityMultiSign where
  type DatumType MajorityMultiSign = MajorityMultiSignDatum
  type RedeemerType MajorityMultiSign = MajorityMultiSignRedeemer

type MajorityMultiSignSchema =
  Endpoint "Initialize" MajorityMultiSignDatum
    .\/ Endpoint "SetSignatures" SetSignaturesParams
