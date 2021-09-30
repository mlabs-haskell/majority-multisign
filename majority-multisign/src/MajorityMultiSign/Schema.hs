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
  SetSignatureParams (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Endpoint, type (.\/))
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx qualified
import PlutusTx.Prelude

data MajorityMultiSignIdentifier = MajorityMultiSignIdentifier
  { address :: ValidatorHash
  , asset :: AssetClass
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignIdentifier

data MajorityMultiSignValidatorParams = MajorityMultiSignValidatorParams
  { asset :: AssetClass
  }

PlutusTx.makeLift ''MajorityMultiSignValidatorParams

data MajorityMultiSignDatum = MajorityMultiSignDatum
  { signers :: [PubKeyHash]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''MajorityMultiSignDatum

data MajorityMultiSignRedeemer
  = UseSignaturesAct
  | UpdateKeyAct
      { newKey :: PubKeyHash
      , index :: Integer
      }

PlutusTx.unstableMakeIsData ''MajorityMultiSignRedeemer

data SetSignatureParams = SetSignatureParams
  { mmsIdentifier :: MajorityMultiSignIdentifier
  , currentKeys :: [PubKeyHash]
  , replaceIndex :: Integer
  , replaceKey :: PubKeyHash
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''SetSignatureParams

data MajorityMultiSign

instance Scripts.ValidatorTypes MajorityMultiSign where
  type DatumType MajorityMultiSign = MajorityMultiSignDatum
  type RedeemerType MajorityMultiSign = MajorityMultiSignRedeemer

type MajorityMultiSignSchema =
  Endpoint "Initialize" MajorityMultiSignDatum
    .\/ Endpoint "SetSignature" SetSignatureParams
