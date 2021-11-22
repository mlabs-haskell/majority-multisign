{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module MajorityMultiSign.Contracts (
  getValidSignSets,
  initialize,
  multiSignTokenName,
  setSignatures,
  submitSignedTxConstraintsWith,
) where

import Cardano.Prelude (foldMap, rightToMaybe, subsequences, (<>))
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Kind (Type)
import Data.List ((\\))
import Data.Map qualified as Map
import Data.Monoid (Last (Last))
import Data.Row (Row)
import Data.Text (Text)
import Data.Void (Void)
import Ledger (
  AssetClass,
  ChainIndexTxOut,
  TokenName,
  TxOutRef,
  pubKeyHash,
  txId,
  validatorHash,
 )
import Ledger qualified
import Ledger.Constraints (ScriptLookups, TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Crypto (PubKey)
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (
  Any,
  DatumType,
  RedeemerType,
 )
import MajorityMultiSign.OnChain (
  validator,
  validatorFromIdentifier,
  validatorHashFromIdentifier,
 )
import MajorityMultiSign.Schema (
  MajorityMultiSignDatum (MajorityMultiSignDatum),
  MajorityMultiSignIdentifier,
  MajorityMultiSignRedeemer (UpdateKeysAct, UseSignaturesAct),
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (MajorityMultiSignValidatorParams),
  SetSignaturesParams (SetSignaturesParams, mmsIdentifier, newKeys, pubKeys),
  getMinSigners,
  maximumSigners,
  naturalLength,
 )
import Playground.Contract (Tx)
import Plutus.Contract (
  Contract,
  ContractError (OtherError),
  awaitTxConfirmed,
  submitTxConstraintsWith,
  tell,
  throwError,
  utxosAt,
 )
import Plutus.Contract.Types (mapError)
import Plutus.Contracts.Currency (CurrencyError (CurContractError), currencySymbol, mintContract)
import Plutus.V1.Ledger.Api (
  Datum (Datum, getDatum),
  PubKeyHash,
  Redeemer (Redeemer),
  ToData,
  fromBuiltinData,
 )
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, assetClassValueOf)
import PlutusTx (toBuiltinData)
import PlutusTx.Prelude hiding (foldMap, (<>))

-- | Token name for the MajorityMultiSignDatum
multiSignTokenName :: TokenName
multiSignTokenName = "MajorityMultiSignDatum"

-- | Extracts the ContractError from a CurrencyError
unwrapCurErr :: CurrencyError -> ContractError
unwrapCurErr (CurContractError c) = c

{- | Mints the oneshot for a validator, sends it to the precalculated validator address with the correct datum.
  Writes the asset to observable state
-}
initialize ::
  PubKeyHash ->
  MajorityMultiSignDatum ->
  Contract (Last AssetClass) MajorityMultiSignSchema ContractError ()
initialize pkh dat = do
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
  tell $ Last $ Just oneshotAsset
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

{- | Gets all minimal sets of keys that would pass validation. For a 5 key system, this will generate 10 sets.
 TODO: Optimise this, there is no need to generate all subsets and filter.
-}
getValidSignSets :: [PubKeyHash] -> [[PubKeyHash]]
getValidSignSets ps = filter ((== minSigCount) . naturalLength) $ subsequences ps
  where
    minSigCount = getMinSigners ps

-- | Creates the constraint for signing, this scales as `getValidSignSets` does
makeSigningConstraint ::
  forall (a :: Type).
  [[PubKeyHash]] ->
  TxConstraints (RedeemerType a) (DatumType a)
makeSigningConstraint keyOptions = Constraints.mustSatisfyAnyOf $ foldMap Constraints.mustBeSignedBy <$> keyOptions

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
  [PubKey] ->
  ScriptLookups Any ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  Contract w s ContractError Tx
submitSignedTxConstraintsWith mms pubKeys lookups tx = do
  (txOutData, datum, signerList) <- findUTxO mms
  let keyOptions :: [[PubKeyHash]]
      keyOptions = getValidSignSets signerList
      lookups' :: ScriptLookups Any
      lookups' =
        lookups
          <> Constraints.otherScript (validatorFromIdentifier mms)
          <> Constraints.unspentOutputs (uncurry Map.singleton txOutData)
          <> foldMap Constraints.pubKey pubKeys
      tx' :: TxConstraints BuiltinData BuiltinData
      tx' =
        bimap PlutusTx.toBuiltinData PlutusTx.toBuiltinData tx
          <> makeSigningConstraint @Any keyOptions
          <> Constraints.mustSpendScriptOutput (fst txOutData) (Redeemer $ PlutusTx.toBuiltinData UseSignaturesAct)
          <> Constraints.mustPayToOtherScript (validatorHashFromIdentifier mms) datum (assetClassValue mms.asset 1)

  unless (sufficientPubKeys pubKeys [] keyOptions) $
    throwError $ OtherError "Insufficient pub keys given"
  unless (naturalLength pubKeys <= maximumSigners) $
    throwError $ OtherError "Too many signers given"

  submitTxConstraintsWith @Any lookups' tx'

subset :: forall (a :: Type). Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

sufficientPubKeys :: [PubKey] -> [PubKeyHash] -> [[PubKeyHash]] -> Bool
sufficientPubKeys pubKeys req opts = subset req pkhs && any (`subset` pkhs) opts
  where
    pkhs = pubKeyHash <$> pubKeys

-- | Updates all keys in the multisign given authority
setSignatures ::
  forall (w :: Type).
  SetSignaturesParams ->
  Contract w MajorityMultiSignSchema ContractError ()
setSignatures SetSignaturesParams {mmsIdentifier, newKeys, pubKeys} = do
  (txOutData, _, signerList) <- findUTxO mmsIdentifier

  let keyOptions :: [[PubKeyHash]]
      keyOptions = getValidSignSets signerList
      datum :: Datum
      datum = Datum $ PlutusTx.toBuiltinData $ MajorityMultiSignDatum newKeys
      newKeysDiff :: [PubKeyHash]
      newKeysDiff = newKeys \\ signerList
      lookups =
        Constraints.otherScript (validatorFromIdentifier mmsIdentifier)
          <> Constraints.unspentOutputs (uncurry Map.singleton txOutData)
          <> foldMap Constraints.pubKey pubKeys
      tx =
        makeSigningConstraint @Any keyOptions
          <> foldMap Constraints.mustBeSignedBy newKeysDiff
          <> Constraints.mustSpendScriptOutput (fst txOutData) (Redeemer $ PlutusTx.toBuiltinData $ UpdateKeysAct newKeys)
          <> Constraints.mustPayToOtherScript (validatorHashFromIdentifier mmsIdentifier) datum (assetClassValue mmsIdentifier.asset 1)

  unless (sufficientPubKeys pubKeys newKeysDiff keyOptions) $
    throwError $ OtherError "Insufficient pub keys given"
  unless (naturalLength newKeys <= maximumSigners) $
    throwError $ OtherError "Too many new signers given"

  ledgerTx <- submitTxConstraintsWith @Any lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

{- | Finds the UTxO in a majority multisign containing the asset,
 and returns it, its datum and the signer list
-}
findUTxO ::
  forall (w :: Type) (s :: Row Type).
  MajorityMultiSignIdentifier ->
  Contract w s ContractError ((TxOutRef, ChainIndexTxOut), Datum, [PubKeyHash])
findUTxO mms = do
  utxos <- utxosAt $ Ledger.scriptHashAddress $ validatorHashFromIdentifier mms
  let utxoFiltered = Map.toList $ Map.filter valid utxos
      valid =
        (> 0)
          . flip assetClassValueOf mms.asset
          . Ledger.txOutValue
          . Ledger.toTxOut
  case utxoFiltered of
    [(txOutRef, txOut)] ->
      maybeToError "Couldn't extract datum" $ do
        datum <- getChainIndexTxOutDatum txOut
        typedDatum <- fromBuiltinData @MajorityMultiSignDatum $ getDatum datum
        return ((txOutRef, txOut), datum, typedDatum.signers)
    _ -> throwError $ OtherError "Couldn't find UTxO"

-- | fromJust that gives a Contract error
maybeToError ::
  forall (w :: Type) (s :: Row Type) (a :: Type).
  Text ->
  Maybe a ->
  Contract w s ContractError a
maybeToError err = maybe (throwError $ OtherError err) return

-- | Extracts the datum from a ChainIndexTxOut
getChainIndexTxOutDatum :: ChainIndexTxOut -> Maybe Datum
getChainIndexTxOutDatum Ledger.PublicKeyChainIndexTxOut {} = Nothing
getChainIndexTxOutDatum Ledger.ScriptChainIndexTxOut {_ciTxOutDatum = eDatum} = rightToMaybe eDatum
