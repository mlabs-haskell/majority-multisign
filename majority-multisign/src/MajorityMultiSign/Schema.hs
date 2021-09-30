{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- record-dot-preprocessor creates code that violates this warning, disable for this file
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module MajorityMultiSign.Schema (
  MajorityMultiSign,
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignRedeemer (..),
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (..),
  SetSignaturesParams (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Endpoint, type (.\/))
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx qualified

-- | Data type used to identify a majority multisign validator (the validator itself and the asset needed to call it)
data MajorityMultiSignIdentifier = MajorityMultiSignIdentifier
  { address :: ValidatorHash
  , asset :: AssetClass
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignIdentifier

-- | Params to the majority multisign validator, as the asset class of the `MajorityMultiSignDatum`
data MajorityMultiSignValidatorParams = MajorityMultiSignValidatorParams
  { asset :: AssetClass
  }

PlutusTx.makeLift ''MajorityMultiSignValidatorParams

{- | Datum held by the validator, storing the pub keys of the signatures needed
  This is also used as the params to the initialize endpoint
-}
data MajorityMultiSignDatum = MajorityMultiSignDatum
  { signers :: [PubKeyHash]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignDatum

-- | Redeemer of the validator, allowing for simple use (not modifying datum), or key updating
data MajorityMultiSignRedeemer
  = UseSignaturesAct
  | UpdateKeysAct
      { keys :: [PubKeyHash]
      }

PlutusTx.unstableMakeIsData ''MajorityMultiSignRedeemer

-- | Params to the set signature endpoint
data SetSignaturesParams = SetSignaturesParams
  { mmsIdentifier :: MajorityMultiSignIdentifier
  , newKeys :: [PubKeyHash]
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
