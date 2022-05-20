{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module MajorityMultiSign.OnChain (
  checkMultisigned,
  mkValidator,
  validator,
  validatorAddress,
  validatorFromIdentifier,
  validatorHash,
  validatorHashFromIdentifier,
) where

import Data.List.Extra (firstJust)
import Ledger (Address, Datum (Datum), PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (scriptContextTxInfo), txSignedBy)
import Ledger qualified
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts qualified as TypedScripts
import MajorityMultiSign.Schema (
  MajorityMultiSign,
  MajorityMultiSignDatum (MajorityMultiSignDatum, signers),
  MajorityMultiSignIdentifier (MajorityMultiSignIdentifier, asset),
  MajorityMultiSignRedeemer (UpdateKeysAct, UseSignaturesAct),
  MajorityMultiSignValidatorParams (MajorityMultiSignValidatorParams, asset),
  getMinSigners,
  maximumSigners,
 )
import Plutus.V1.Ledger.Api (TxOut (txOutDatumHash, txOutValue))
import Plutus.V1.Ledger.Contexts (TxInInfo (txInInfoResolved), TxInfo (txInfoInputs), findDatumHash)
import Plutus.V1.Ledger.Value (assetClassValueOf)
import PlutusTx qualified
import PlutusTx.List.Natural qualified as Natural
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude

{-# INLINEABLE mkValidator #-}
mkValidator ::
  MajorityMultiSignValidatorParams ->
  MajorityMultiSignDatum ->
  MajorityMultiSignRedeemer ->
  ScriptContext ->
  Bool
mkValidator params dat red ctx =
  hasCorrectToken params ctx (getExpectedDatum red dat)
    && isSufficientlySigned red dat ctx
    && isUnderSizeLimit red dat

{-# INLINEABLE removeSigners #-}
removeSigners :: [PaymentPubKeyHash] -> [PaymentPubKeyHash] -> [PaymentPubKeyHash]
removeSigners [] _ = []
removeSigners xs [] = xs -- Not strictly needed, but more efficient
removeSigners (x : xs) ys = if x `elem` ys then removeSigners xs ys else x : removeSigners xs ys

-- | Calculates the expected output datum from the current datum and the redeemer
{-# INLINEABLE getExpectedDatum #-}
getExpectedDatum :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> MajorityMultiSignDatum
getExpectedDatum UseSignaturesAct datum = datum
getExpectedDatum (UpdateKeysAct keys) datum = datum {signers = keys}

-- | Checks if, when setting new signatures, all new keys have signed the transaction
{-# INLINEABLE hasNewSignatures #-}
hasNewSignatures :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> ScriptContext -> Bool
hasNewSignatures UseSignaturesAct _ _ = True
hasNewSignatures (UpdateKeysAct keys) MajorityMultiSignDatum {signers} ctx =
  all (txSignedBy (scriptContextTxInfo ctx) . unPaymentPubKeyHash) $ keys `removeSigners` signers

-- | Checks the script has the correct token (containing the asset we want), forwards it to the right place, and has the datum we expect
{-# INLINEABLE hasCorrectToken #-}
hasCorrectToken :: MajorityMultiSignValidatorParams -> ScriptContext -> MajorityMultiSignDatum -> Bool
hasCorrectToken MajorityMultiSignValidatorParams {asset} ctx expectedDatum =
  case result of
    Left errMsg -> trace errMsg False
    Right () -> True
  where
    continuing :: [TxOut]
    continuing = Ledger.getContinuingOutputs ctx

    checkAsset :: TxOut -> Maybe TxOut
    checkAsset txOut = if assetClassValueOf (txOutValue txOut) asset > 0 then Just txOut else Nothing

    justOrErr :: BuiltinString -> Maybe a -> Either BuiltinString a
    justOrErr errMsg = maybe (Left errMsg) Right

    result :: Either BuiltinString ()
    result = do
      assetTxOut <- justOrErr "Couldn't find asset" $ firstJust checkAsset continuing
      datumHash <- justOrErr "Continuing output does not have datum" $ txOutDatumHash assetTxOut
      expectedDatumHash <-
        justOrErr "Datum map does not have expectedDatum" $
          findDatumHash (Datum $ PlutusTx.toBuiltinData expectedDatum) (scriptContextTxInfo ctx)
      unless (datumHash == expectedDatumHash) $
        Left "Incorrect output datum"

-- | External function called by other contracts to ensure multisigs present
{-# INLINEABLE checkMultisigned #-}
checkMultisigned :: MajorityMultiSignIdentifier -> ScriptContext -> Bool
checkMultisigned MajorityMultiSignIdentifier {asset} ctx =
  traceIfFalse "Missing Multisign Asset" $
    any containsAsset inputs
  where
    inputs :: [TxInInfo]
    inputs = txInfoInputs $ scriptContextTxInfo ctx

    containsAsset :: TxInInfo -> Bool
    containsAsset = (> 0) . flip assetClassValueOf asset . txOutValue . txInInfoResolved

-- | Checks the validator is signed by more than half of the signers on the datum
{-# INLINEABLE isSufficientlySigned #-}
isSufficientlySigned :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> ScriptContext -> Bool
isSufficientlySigned red dat@MajorityMultiSignDatum {signers} ctx =
  traceIfFalse "Not enough signatures" (Natural.length signersPresent >= minSigners)
    && traceIfFalse "Missing signatures from new keys" (hasNewSignatures red dat ctx)
  where
    signersPresent, signersUnique :: [PaymentPubKeyHash]
    signersPresent = filter (txSignedBy (scriptContextTxInfo ctx) . unPaymentPubKeyHash) signersUnique
    signersUnique = nub signers
    minSigners :: Natural
    minSigners = getMinSigners signersUnique

-- | Checks the validator datum fits under the size limit
{-# INLINEABLE isUnderSizeLimit #-}
isUnderSizeLimit :: MajorityMultiSignRedeemer -> MajorityMultiSignDatum -> Bool
isUnderSizeLimit UseSignaturesAct MajorityMultiSignDatum {signers} =
  traceIfFalse "Datum too large" (Natural.length signers <= maximumSigners)
isUnderSizeLimit (UpdateKeysAct keys) MajorityMultiSignDatum {signers} =
  traceIfFalse "Datum too large" (Natural.length signers <= maximumSigners)
    && traceIfFalse "Redeemer too large" (Natural.length keys <= maximumSigners)

inst :: MajorityMultiSignValidatorParams -> TypedScripts.TypedValidator MajorityMultiSign
inst params =
  TypedScripts.mkTypedValidator @MajorityMultiSign
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TypedScripts.wrapValidator @MajorityMultiSignDatum @MajorityMultiSignRedeemer

validator :: MajorityMultiSignValidatorParams -> Scripts.Validator
validator = TypedScripts.validatorScript . inst

validatorHash :: MajorityMultiSignValidatorParams -> Scripts.ValidatorHash
validatorHash = TypedScripts.validatorHash . inst

validatorAddress :: MajorityMultiSignValidatorParams -> Address
validatorAddress = Ledger.scriptAddress . validator

-- | Gets the validator from an identifier
validatorFromIdentifier :: MajorityMultiSignIdentifier -> Scripts.Validator
validatorFromIdentifier MajorityMultiSignIdentifier {asset} = validator $ MajorityMultiSignValidatorParams asset

-- | Gets the validator hash from an identifier
validatorHashFromIdentifier :: MajorityMultiSignIdentifier -> Scripts.ValidatorHash
validatorHashFromIdentifier MajorityMultiSignIdentifier {asset} = validatorHash $ MajorityMultiSignValidatorParams asset
