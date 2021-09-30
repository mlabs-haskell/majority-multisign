{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module MajorityMultiSign.OnChain (
  findUTxO,
  validator,
  validatorAddress,
  validatorFromIdentifier,
) where

import Cardano.Prelude (rightToMaybe)
import Data.Kind (Type)
import Data.List.Extra (firstJust)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text)
import Ledger (Address, ChainIndexTxOut (..), Datum (..), PubKeyHash, ScriptContext (..), scriptHashAddress, txSignedBy)
import Ledger qualified
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts qualified as TypedScripts
import MajorityMultiSign.Schema (
  MajorityMultiSign,
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignRedeemer (..),
  MajorityMultiSignValidatorParams (..),
 )
import Plutus.Contract (
  Contract,
  ContractError (..),
  throwError,
  utxosAt,
 )
import Plutus.V1.Ledger.Api (TxInfo (..), TxOut (..), TxOutRef)
import Plutus.V1.Ledger.Contexts (findDatumHash)
import Plutus.V1.Ledger.Value (assetClassValueOf)
import PlutusTx qualified
import PlutusTx.Builtins (divideInteger, greaterThanEqualsInteger, lessThanEqualsInteger)
import PlutusTx.Prelude hiding (take)

-- Replaces a value in a list by index
{-# INLINEABLE replaceIndex #-}
-- Must be implemented with direct recursion as onchain list manipulation is not well supported in plutus.
replaceIndex :: Integer -> PubKeyHash -> [PubKeyHash] -> [PubKeyHash]
replaceIndex _ _ [] = []
replaceIndex i x' (x : xs)
  | i `lessThanEqualsInteger` 0 = x' : xs
  | otherwise = x : replaceIndex (i -1) x' xs

{-# INLINEABLE mkValidator #-}
mkValidator ::
  MajorityMultiSignValidatorParams ->
  MajorityMultiSignDatum ->
  MajorityMultiSignRedeemer ->
  ScriptContext ->
  Bool
mkValidator params dat red ctx =
  hasCorrectToken params ctx (getExpectedDatum red dat)
    && isSufficientlySigned dat ctx

-- | Calculates the expected output datum from the current datum and the redeemer
{-# INLINEABLE getExpectedDatum #-}
getExpectedDatum :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> MajorityMultiSignDatum
getExpectedDatum UseSignaturesAct datum = datum
getExpectedDatum UpdateKeyAct {..} datum = datum {signers = replaceIndex index newKey $ signers datum}

-- | Checks the script has the correct token (containing the asset we want), forwards it to the right place, and has the datum we expect
{-# INLINEABLE hasCorrectToken #-}
hasCorrectToken :: MajorityMultiSignValidatorParams -> ScriptContext -> MajorityMultiSignDatum -> Bool
hasCorrectToken MajorityMultiSignValidatorParams {..} ctx expectedDatum =
  isJust assetTxOut
    && (assetTxOut >>= txOutDatumHash) == findDatumHash (Datum $ PlutusTx.toBuiltinData expectedDatum) (scriptContextTxInfo ctx)
  where
    continuing :: [TxOut]
    continuing = Ledger.getContinuingOutputs ctx

    checkAsset :: TxOut -> Maybe TxOut
    checkAsset txOut = if assetClassValueOf (txOutValue txOut) asset > 0 then Just txOut else Nothing

    assetTxOut :: Maybe TxOut
    assetTxOut = firstJust checkAsset continuing

-- | Checks the validator is signed by more than half of the signers on the datum
{-# INLINEABLE isSufficientlySigned #-}
isSufficientlySigned :: MajorityMultiSignDatum -> ScriptContext -> Bool
isSufficientlySigned MajorityMultiSignDatum {..} ctx = length signersPresent `greaterThanEqualsInteger` ((length signers + 1) `divideInteger` 2)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    signersPresent :: [PubKeyHash]
    signersPresent = filter (txSignedBy info) signers

inst :: MajorityMultiSignValidatorParams -> TypedScripts.TypedValidator MajorityMultiSign
inst params =
  TypedScripts.mkTypedValidator @MajorityMultiSign
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TypedScripts.wrapValidator @MajorityMultiSignDatum @MajorityMultiSignRedeemer

validator :: MajorityMultiSignValidatorParams -> Scripts.Validator
validator = TypedScripts.validatorScript . inst

validatorAddress :: MajorityMultiSignValidatorParams -> Ledger.Address
validatorAddress = Ledger.scriptAddress . validator

-- | Gets the validator from an identifier
validatorFromIdentifier :: MajorityMultiSignIdentifier -> Scripts.Validator
validatorFromIdentifier MajorityMultiSignIdentifier {asset} = validator $ MajorityMultiSignValidatorParams asset

-- | Finds the UTxO in a majority multisign containing the asset, and returns it and its datum
findUTxO :: forall (w :: Type) (s :: Row Type). MajorityMultiSignIdentifier -> Contract w s ContractError (TxOutRef, Datum)
findUTxO mms = do
  utxos <- utxosAt $ scriptHashAddress mms.address
  let utxoFiltered = Map.toList $ Map.filter ((> 0) . flip assetClassValueOf mms.asset . txOutValue . Ledger.toTxOut) utxos
  case utxoFiltered of
    [(txOutRef, txOut)] ->
      maybeToError "Couldn't extract datum" $
        (txOutRef,) <$> getChainIndexTxOutDatum txOut
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
getChainIndexTxOutDatum PublicKeyChainIndexTxOut {} = Nothing
getChainIndexTxOutDatum ScriptChainIndexTxOut {_ciTxOutDatum = eDatum} = rightToMaybe eDatum
