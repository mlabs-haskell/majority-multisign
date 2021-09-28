{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module MajorityMultiSign.OnChain (
  address,
  validatorAddress,
) where

import Cardano.Prelude (fromInteger)
import Data.Kind (Type)
import Data.List.Extra (firstJust)
import Ledger (Address, Datum (..), PubKeyHash, ScriptContext (..), txSignedBy)
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
import Plutus.V1.Ledger.Api (TxInfo (..), TxOut (..))
import Plutus.V1.Ledger.Contexts (findDatumHash)
import Plutus.V1.Ledger.Value (assetClassValueOf)
import PlutusTx qualified
import PlutusTx.Builtins (divideInteger, greaterThanEqualsInteger, lessThanEqualsInteger)
import PlutusTx.Prelude hiding (take)

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

{-# INLINEABLE getExpectedDatum #-}
getExpectedDatum :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> MajorityMultiSignDatum
getExpectedDatum UseSignaturesAct datum = datum
getExpectedDatum UpdateKeyAct {..} datum = datum {signers = replaceIndex index newKey $ signers datum}

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
