{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module MajorityMultiSign.Contracts (initialize, submitSignedTxConstraintsWith, setSignature, getValidSignSets) where

import Cardano.Prelude (div, subsequences, (<>))
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
import Ledger.Constraints.TxConstraints qualified as Constraints
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
  SetSignatureParams (..),
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
import Plutus.Contract.Types (mapError)
import Plutus.Contracts.Currency (CurrencyError (..), currencySymbol, mintContract)
import Plutus.V1.Ledger.Api (
  Datum (..),
  PubKeyHash,
  Redeemer (..),
  ToData,
 )
import Plutus.V1.Ledger.Value (assetClass, assetClassValue)
import PlutusTx (toBuiltinData)
import PlutusTx.Prelude hiding ((<>))

-- | Token name for the MajorityMultiSignDatum
multiSignTokenName :: TokenName
multiSignTokenName = "MajorityMultiSignDatum"

-- | Extracts the ContractError from a CurrencyError
unwrapCurErr :: CurrencyError -> ContractError
unwrapCurErr (CurContractError c) = c

{- | Mints the oneshot for a validator, sends it to the precalculated validator address with the correct datum.
  Writes the asset to observable state
-}
initialize :: MajorityMultiSignDatum -> Contract (Last AssetClass) MajorityMultiSignSchema ContractError ()
initialize dat = do
  pkh <- pubKeyHash <$> ownPubKey
  oneshotCS <- mapError unwrapCurErr $ currencySymbol <$> mintContract pkh [(multiSignTokenName, 1)]
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

{- | Gets all minimal sets of keys that would pass validation. For a 5 key system, this will generate 10 sets.
 TODO: Optimise this, there is no need to generate all subsets and filter.
-}
getValidSignSets :: [PubKeyHash] -> [[PubKeyHash]]
getValidSignSets ps = filter ((== (length ps + 1) `div` 2) . length) $ subsequences ps

-- | Creates the constraint for signing, this scales as `getValidSignSets` does
makeSigningConstraint ::
  forall (a :: Type).
  [PubKeyHash] ->
  TxConstraints (RedeemerType a) (DatumType a)
makeSigningConstraint keys = Constraints.mustSatisfyAnyOf $ mconcatMap Constraints.mustBeSignedBy <$> getValidSignSets keys

{- | Wrapper for submitTxConstraintsWith that adds the lookups and constraints for using a majority multisign validator in the transaction
  Due to limitations of plutus and submitTxConstraintsWith, the lookups passed here must be generic, typed validators cannot be passed in directly,
    and must instead be converted to normal validators first.
-}
submitSignedTxConstraintsWith ::
  forall (a :: Type) (w :: Type) (s :: Row Type).
  ( ToData (RedeemerType a)
  , ToData (DatumType a)
  ) =>
  MajorityMultiSignIdentifier ->
  ScriptLookups Any ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  Contract w s ContractError Tx
submitSignedTxConstraintsWith mms lookups tx = do
  (utxoRef, datum, signerList) <- findUTxO mms
  let lookups' :: ScriptLookups Any
      lookups' = lookups <> Constraints.otherScript (validatorFromIdentifier mms)
      tx' :: TxConstraints BuiltinData BuiltinData
      tx' =
        bimap PlutusTx.toBuiltinData PlutusTx.toBuiltinData tx
          <> makeSigningConstraint @Any signerList
          <> Constraints.mustSpendScriptOutput utxoRef (Redeemer $ PlutusTx.toBuiltinData UseSignaturesAct)
          <> Constraints.mustPayToTheScript (getDatum datum) (assetClassValue mms.asset 1)
  submitTxConstraintsWith @Any lookups' tx'

-- | Sets one of the signatures in a multisign validator given enough signatures on the tx
setSignature ::
  forall (w :: Type).
  SetSignatureParams ->
  Contract w MajorityMultiSignSchema ContractError ()
setSignature SetSignatureParams {..} = do
  (utxoRef, datum, signerList) <- findUTxO mmsIdentifier
  let lookups = Constraints.otherScript (validatorFromIdentifier mmsIdentifier)
      tx =
        makeSigningConstraint @Any signerList
          <> Constraints.mustSpendScriptOutput utxoRef (Redeemer $ PlutusTx.toBuiltinData $ UpdateKeyAct replaceKey replaceIndex)
          <> Constraints.mustPayToTheScript (getDatum datum) (assetClassValue mmsIdentifier.asset 1)
  ledgerTx <- submitTxConstraintsWith @Any lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
