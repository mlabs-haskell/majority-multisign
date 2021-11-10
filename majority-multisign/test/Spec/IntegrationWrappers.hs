{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.IntegrationWrappers (
  contract,
  mintingPolicy,
  mintingPolicySymbol,
) where

import Control.Monad (void)
import Data.Void (Void)
import Ledger (PubKey, ScriptContext (..))
import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as TxConstraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts qualified as TypedScripts
import MajorityMultiSign.Contracts (submitSignedTxConstraintsWith)
import MajorityMultiSign.OnChain (checkMultisigned)
import MajorityMultiSign.Schema
import Plutus.Contract (Contract, ContractError (..), awaitTxConfirmed, ownPubKey)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude

data IntegrationParams = IntegrationParams
  { mmsId :: MajorityMultiSignIdentifier
  , pubKeys :: [PubKey]
  }

contract :: IntegrationParams -> Contract w s ContractError ()
contract IntegrationParams {mmsId, pubKeys} = do
  pkh <- Ledger.pubKeyHash <$> ownPubKey
  let value = Value.singleton (mintingPolicySymbol mmsId) "Token" 1
      lookups = Constraints.mintingPolicy $ mintingPolicy mmsId
      tx = TxConstraints.mustMintValue value <> TxConstraints.mustPayToPubKey pkh value
  ledgerTx <- submitSignedTxConstraintsWith @Void mmsId pubKeys lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

{-# INLINEABLE mkPolicy #-}
mkPolicy ::
  MajorityMultiSignIdentifier ->
  () ->
  ScriptContext ->
  Bool
mkPolicy mmsId () = checkMultisigned mmsId

mintingPolicy :: MajorityMultiSignIdentifier -> TypedScripts.MintingPolicy
mintingPolicy mms =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||TypedScripts.wrapMintingPolicy @() . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode mms

mintingPolicySymbol :: MajorityMultiSignIdentifier -> Ledger.CurrencySymbol
mintingPolicySymbol = Ledger.scriptCurrencySymbol . mintingPolicy
