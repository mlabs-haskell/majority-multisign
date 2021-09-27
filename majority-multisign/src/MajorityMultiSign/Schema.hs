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
) where

import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.Contract (Endpoint)
import PlutusTx qualified
import PlutusTx.Prelude

data MajorityMultiSignIdentifier = MajorityMultiSignIdentifier
  { address :: ValidatorHash
  , asset :: AssetClass
  }

data MajorityMultiSignValidatorParams = MajorityMultiSignValidatorParams
  { asset :: AssetClass
  }

data MajorityMultiSignDatum = MajorityMultiSignDatum
  { signers :: [PubKeyHash]
  }

PlutusTx.unstableMakeIsData ''MajorityMultiSignDatum

data MajorityMultiSignRedeemer
  = UseSignaturesAct
  | UpdateKeyAct
      { newKey :: PubKeyHash
      , index :: Integer
      }

PlutusTx.unstableMakeIsData ''MajorityMultiSignRedeemer

data MajorityMultiSign

instance Scripts.ValidatorTypes MajorityMultiSign where
  type DatumType MajorityMultiSign = MajorityMultiSignDatum
  type RedeemerType MajorityMultiSign = MajorityMultiSignRedeemer

type MajorityMultiSignSchema =
  Endpoint "Initialize" MajorityMultiSignDatum
