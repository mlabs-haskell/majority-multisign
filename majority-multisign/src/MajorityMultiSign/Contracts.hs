{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module MajorityMultiSign.Contracts (initialize, multiSignTokenName, submitSignedTxConstraintsWith, setSignatures, getValidSignSets) where

import Cardano.Prelude (Eq, ceiling, foldMap, fromIntegral, subsequences, (*), (<>))
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Kind (Type)
import Data.List ((\\))
import Data.Map qualified as Map
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
import Ledger.Crypto (PubKey)
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (
  Any,
  DatumType,
  RedeemerType,
 )
import MajorityMultiSign.OnChain (findUTxO, validator, validatorFromIdentifier, validatorHashFromIdentifier)
import MajorityMultiSign.Schema (
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignRedeemer (..),
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (..),
  SetSignaturesParams (..),
  signReq,
 )
import Playground.Contract (Tx)
import Plutus.Contract (
  Contract,
  ContractError (..),
  awaitTxConfirmed,
  ownPubKey,
  submitTxConstraintsWith,
  tell,
  throwError,
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
import PlutusTx.Prelude hiding (Eq, foldMap, (*), (<>))

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
  tell $ Last $ Just oneshotAsset
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

{- | Gets all minimal sets of keys that would pass validation. For a 5 key system, this will generate 10 sets.
 TODO: Optimise this, there is no need to generate all subsets and filter.
-}
getValidSignSets :: [PubKeyHash] -> [[PubKeyHash]]
getValidSignSets ps = filter ((== minSigCount) . length) $ subsequences ps
  where
    minSigCount = ceiling $ fromIntegral (length ps) * signReq

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
          <> Constraints.mustPayToTheScript (getDatum datum) (assetClassValue mms.asset 1)

  unless (sufficientPubKeys pubKeys [] keyOptions) $ throwError $ OtherError "Insufficient pub keys given"

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

  unless (sufficientPubKeys pubKeys newKeysDiff keyOptions) $ throwError $ OtherError "Insufficient pub keys given"

  ledgerTx <- submitTxConstraintsWith @Any lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
