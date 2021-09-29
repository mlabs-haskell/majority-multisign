{-# LANGUAGE AllowAmbiguousTypes #-}

module MajorityMultiSign.Contracts (initialize, submitSignedTxConstraintsWith, setSignature) where

import Cardano.Prelude ((<>))
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Kind (Type)
import Data.List.Extra (mconcatMap)
import Data.Monoid (Last (..))
import Data.Row (Row)
import Data.Void (Void)
import Ledger (
  AssetClass,
  TokenName,
  pubKeyHash,
  txId,
  validatorHash,
 )
import Ledger.Constraints (ScriptLookups, TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (
  Any,
  DatumType,
  RedeemerType,
 )
import MajorityMultiSign.OnChain (findUTxO, validator, validatorFromIdentifier)
import MajorityMultiSign.Schema (
  MajorityMultiSignDatum,
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignRedeemer (..),
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (..),
 )
import Playground.Contract (Tx)
import Plutus.Contract (
  Contract,
  ContractError,
  awaitTxConfirmed,
  ownPubKey,
  submitTxConstraintsWith,
  tell,
 )
import Plutus.Contracts.Currency (CurrencyError, currencySymbol, mintContract)
import Plutus.V1.Ledger.Api (
  Datum (..),
  PubKeyHash,
  Redeemer (..),
  ToData,
 )
import Plutus.V1.Ledger.Value (assetClass, assetClassValue)
import PlutusTx (toBuiltinData)
import PlutusTx.Prelude hiding ((<>))

multiSignTokenName :: TokenName
multiSignTokenName = "MajorityMultiSignDatum"

initialize :: MajorityMultiSignDatum -> Contract (Last AssetClass) MajorityMultiSignSchema CurrencyError ()
initialize dat = do
  pkh <- pubKeyHash <$> ownPubKey
  oneshotCS <- currencySymbol <$> mintContract pkh [(multiSignTokenName, 1)]
  let oneshotAsset :: AssetClass
      oneshotAsset = assetClass oneshotCS multiSignTokenName
      params :: MajorityMultiSignValidatorParams
      params = MajorityMultiSignValidatorParams oneshotAsset
      lookups = Constraints.otherScript $ validator params
      tx =
        Constraints.mustPayToOtherScript
          (validatorHash $ validator params)
          (Scripts.Datum $ toBuiltinData dat)
          (assetClassValue oneshotAsset 1)
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  tell $ Last $ Just oneshotAsset

submitSignedTxConstraintsWith ::
  forall (a :: Type) (w :: Type) (s :: Row Type).
  ( ToData (RedeemerType a)
  , ToData (DatumType a)
  ) =>
  MajorityMultiSignIdentifier ->
  [PubKeyHash] ->
  ScriptLookups Any ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  Contract w s ContractError Tx
submitSignedTxConstraintsWith mms keys lookups tx = do
  (utxoRef, datum) <- findUTxO mms
  let lookups' :: ScriptLookups Any
      lookups' = lookups <> Constraints.otherScript (validatorFromIdentifier mms)
      tx' :: TxConstraints BuiltinData BuiltinData
      tx' =
        bimap PlutusTx.toBuiltinData PlutusTx.toBuiltinData tx
          <> mconcatMap Constraints.mustBeSignedBy keys
          <> Constraints.mustSpendScriptOutput utxoRef (Redeemer $ PlutusTx.toBuiltinData UseSignaturesAct)
          <> Constraints.mustPayToTheScript (getDatum datum) (assetClassValue mms.asset 1)
  submitTxConstraintsWith @Any lookups' tx'

setSignature ::
  forall (w :: Type).
  MajorityMultiSignIdentifier ->
  [PubKeyHash] ->
  Integer ->
  PubKeyHash ->
  Contract w MajorityMultiSignSchema ContractError ()
setSignature mms keys i key = do
  (utxoRef, datum) <- findUTxO mms
  let lookups = Constraints.otherScript (validatorFromIdentifier mms)
      tx =
        mconcatMap Constraints.mustBeSignedBy keys
          <> Constraints.mustSpendScriptOutput utxoRef (Redeemer $ PlutusTx.toBuiltinData $ UpdateKeyAct key i)
          <> Constraints.mustPayToTheScript (getDatum datum) (assetClassValue mms.asset 1)
  ledgerTx <- submitTxConstraintsWith @Any lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
