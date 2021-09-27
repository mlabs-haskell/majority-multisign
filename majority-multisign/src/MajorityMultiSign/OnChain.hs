{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module MajorityMultiSign.OnChain where

import Control.Monad.Extra (mconcatMapM)
import Data.List (genericLength)
import Data.Monoid (First (..))
import Ledger (Address, PubKeyHash, ScriptContext (..), txSignedBy)
import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as TxConstraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts qualified as TypedScripts
import MajorityMultiSign.Schema (
  MajorityMultiSign,
  MajorityMultiSignDatum (..),
  MajorityMultiSignIdentifier (..),
  MajorityMultiSignRedeemer (..),
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (..),
 )
import Plutus.Contract (
  Contract,
  ContractError,
 )
import Plutus.V1.Ledger.Api (TxInfo (..), TxOut (..))
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified

{-# INLINEABLE mkValidator #-}
mkValidator ::
  MajorityMultiSignValidatorParams ->
  MajorityMultiSignDatum ->
  MajorityMultiSignRedeemer ->
  ScriptContext ->
  Bool
mkValidator params dat red ctx =
  hasCorrectToken params ctx dat
    && isSufficientlySigned dat ctx

{-# INLINEABLE hasCorrectToken #-}
hasCorrectToken :: MajorityMultiSignValidatorParams -> ScriptContext -> MajorityMultiSignDatum -> Bool
hasCorrectToken MajorityMultiSignValidatorParams {..} ctx expectedDatum =
  isJust inputTxOut
    && (txOutAddress <$> inputTxOut) == (txOutAddress <$> outputTxOut)
    && (txOutDatumHash <$> outputTxOut) == Just (Just $ datumHash expectedDatum)
  where
    checkAsset :: TxOut -> First TxOut
    checkAsset txOut = First $ if assetClassValueOf asset (txOutValue txOut) > 0 then txOutAddress txOut else Nothing

    getAssetTxOut :: [TxOut] -> Maybe TxOut
    getAssetTxOut = getFirst . mconcatMapM checkAsset

    getAssetAddress :: [TxOut] -> Maybe Address
    getAssetAddress txOuts = txOutAddress <$> getAssetTxOut txOuts

    inputTxOut :: Maybe TxOut
    inputTxOut = getAssetTxOut $ txInInfoResolved <$> txInfoInputs (scriptContextTxInfo ctx)
    outputTxOut :: Maybe TxOut
    outputTxOut = getAssetTxOut $ txInfoOutputs $ scriptContextTxInfo ctx  

{-# INLINEABLE isSufficientlySigned #-}
isSufficientlySigned :: MajorityMultiSignDatum -> ScriptContext -> Bool
isSufficientlySigned MajorityMultiSignDatum {..} ctx = genericLength signersPresent >= (genericLength signers / 2)
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

validator :: Scripts.Validator
validator = TypedScripts.validatorScript inst

address :: Ledger.Address
address = Ledger.scriptAddress validator
