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
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignRedeemer (..),
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (..),
  SetSignaturesParams (..),
  getMinSigners,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Extras (encodeByteString)
import Data.Functor.Foldable (Fix (..))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Ledger.Crypto (PubKey)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Endpoint, type (.\/))
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol (..), TokenName (..))
import PlutusTx qualified
import PlutusTx.NatRatio (NatRatio, ceiling, frac, fromNatural)
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude hiding (decodeUtf8, Eq)
import Prelude (Eq, Show)
import Schema (
  FormArgumentF (..),
  FormSchema (..),
  ToArgument (toArgument),
  ToSchema (toSchema),
 )

{-# INLINEABLE signReq #-}

-- | Signing proportion required
signReq :: NatRatio
signReq = [frac| (1, 2) |] -- 0.5

{-# INLINEABLE intToNatRatio #-}
intToNatRatio :: Integer -> NatRatio
intToNatRatio = fromNatural . toEnum @Natural

{-# INLINEABLE ceilNatRatioToInt #-}
ceilNatRatioToInt :: NatRatio -> Integer
ceilNatRatioToInt = fromEnum . ceiling

{-# INLINEABLE getMinSigners #-}

-- | Given a list of Signers, gets the minimum number of signers needed for a transaction to be valid
getMinSigners :: [a] -> Integer
getMinSigners = ceilNatRatioToInt . (signReq *) . intToNatRatio . length

-- | Data type used to identify a majority multisign validator (the asset needed to call it)
data MajorityMultiSignIdentifier = MajorityMultiSignIdentifier
  { asset :: AssetClass
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, OpenApi.ToSchema, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignIdentifier
PlutusTx.makeLift ''MajorityMultiSignIdentifier

instance ToSchema MajorityMultiSignIdentifier where
  toSchema =
    FormSchemaObject [ ("asset", FormSchemaMaybe (toSchema @AssetClass)) ]

instance ToArgument MajorityMultiSignIdentifier where
  toArgument MajorityMultiSignIdentifier {asset} =
    Fix $ FormObjectF [ ("asset", toArgument (Just asset)) ]

-- | Params to the majority multisign validator, as the asset class of the `MajorityMultiSignDatum`
data MajorityMultiSignValidatorParams = MajorityMultiSignValidatorParams
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
data MajorityMultiSignDatum = MajorityMultiSignDatum
  { signers :: [PubKeyHash]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignDatum

-- | Redeemer of the validator, allowing for simple use (not modifying datum), or key updating
data MajorityMultiSignRedeemer
  = UseSignaturesAct
  | UpdateKeysAct
      { keys :: [PubKeyHash]
      }
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
