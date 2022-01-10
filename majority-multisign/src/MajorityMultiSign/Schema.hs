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
  maximumSigners,
) where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Extras (encodeByteString)
import Data.Functor.Foldable (Fix (Fix))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Ledger.Crypto (PubKey)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Endpoint, type (.\/))
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol (CurrencySymbol), TokenName (TokenName))
import PlutusTx qualified
import PlutusTx.List.Natural qualified as Natural
import PlutusTx.NatRatio (NatRatio, ceiling, frac, fromNatural)
import PlutusTx.Natural (Natural, nat)
import PlutusTx.Prelude hiding (Eq, decodeUtf8, (<$>), (<*>))
import Schema (
  FormArgumentF (FormHexF, FormMaybeF, FormObjectF, FormStringF),
  FormSchema (FormSchemaMaybe, FormSchemaObject),
  ToArgument (toArgument),
  ToSchema (toSchema),
 )
import Prelude (Eq, Show, (<$>), (<*>))

{-# INLINEABLE signReq #-}

-- | Signing proportion required
signReq :: NatRatio
signReq = [frac| (1, 2) |] -- 0.5

{-# INLINEABLE maximumSigners #-}

-- | Maximum number of signers allowed
maximumSigners :: Natural
maximumSigners = [nat| 10 |]

{-# INLINEABLE getMinSigners #-}

-- | Given a list of Signers, gets the minimum number of signers needed for a transaction to be valid
getMinSigners :: [a] -> Natural
getMinSigners = ceiling . (signReq *) . fromNatural . Natural.length

-- | Data type used to identify a majority multisign validator (the asset needed to call it)
newtype MajorityMultiSignIdentifier = MajorityMultiSignIdentifier
  { asset :: AssetClass
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON MajorityMultiSignIdentifier where
  parseJSON = Aeson.withObject "MajorityMultiSignIdentifier" $
    \val -> MajorityMultiSignIdentifier <$> val .: "asset"

instance ToJSON MajorityMultiSignIdentifier where
  toJSON MajorityMultiSignIdentifier {asset} = Aeson.object ["asset" .= asset]

instance OpenApi.ToSchema MajorityMultiSignIdentifier

PlutusTx.makeIsDataIndexed ''MajorityMultiSignIdentifier [('MajorityMultiSignIdentifier, 0)]
PlutusTx.makeLift ''MajorityMultiSignIdentifier

instance ToSchema MajorityMultiSignIdentifier where
  toSchema =
    FormSchemaObject [("asset", FormSchemaMaybe (toSchema @AssetClass))]

instance ToArgument MajorityMultiSignIdentifier where
  toArgument MajorityMultiSignIdentifier {asset} =
    Fix $
      FormObjectF
        [
          ( "asset"
          , Fix $ FormMaybeF (toSchema @AssetClass) $ Just $ toArgument asset
          )
        ]

-- | Params to the majority multisign validator, as the asset class of the `MajorityMultiSignDatum`
newtype MajorityMultiSignValidatorParams = MajorityMultiSignValidatorParams
  { asset :: AssetClass
  }

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

instance FromJSON MajorityMultiSignDatum where
  parseJSON = Aeson.withObject "MajorityMultiSignDatum" $
    \val -> MajorityMultiSignDatum <$> val .: "signers"

instance ToJSON MajorityMultiSignDatum where
  toJSON MajorityMultiSignDatum {signers} = Aeson.object ["signers" .= signers]

PlutusTx.makeIsDataIndexed
  ''MajorityMultiSignDatum
  [('MajorityMultiSignDatum, 0)]

-- | Redeemer of the validator, allowing for simple use (not modifying datum), or key updating
data MajorityMultiSignRedeemer
  = UseSignaturesAct
  | UpdateKeysAct [PubKeyHash]
  deriving stock (Eq, Show)

PlutusTx.makeIsDataIndexed
  ''MajorityMultiSignRedeemer
  [('UseSignaturesAct, 0), ('UpdateKeysAct, 1)]

-- | Params to the set signature endpoint
data SetSignaturesParams = SetSignaturesParams
  { mmsIdentifier :: MajorityMultiSignIdentifier
  , newKeys :: [PubKeyHash]
  , pubKeys :: [PubKey]
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON SetSignaturesParams where
  parseJSON = Aeson.withObject "SetSignaturesParams" $
    \val ->
      SetSignaturesParams
        <$> val .: "mmsIdentifier"
        <*> val .: "newKeys"
        <*> val .: "pubKeys"

instance ToJSON SetSignaturesParams where
  toJSON SetSignaturesParams {mmsIdentifier, newKeys, pubKeys} =
    Aeson.object
      [ "mmsIdentifier" .= mmsIdentifier
      , "newKeys" .= newKeys
      , "pubKeys" .= pubKeys
      ]

PlutusTx.makeIsDataIndexed ''SetSignaturesParams [('SetSignaturesParams, 0)]

data MajorityMultiSign

instance Scripts.ValidatorTypes MajorityMultiSign where
  type DatumType MajorityMultiSign = MajorityMultiSignDatum
  type RedeemerType MajorityMultiSign = MajorityMultiSignRedeemer

type MajorityMultiSignSchema =
  Endpoint "Initialize" MajorityMultiSignDatum
    .\/ Endpoint "SetSignatures" SetSignaturesParams
