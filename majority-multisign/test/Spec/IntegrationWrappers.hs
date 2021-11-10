{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.IntegrationWrappers (
  IntegrationParams (..),
  bypassContract,
  correctContract,
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
import Plutus.Contract (Contract, ContractError (..), awaitTxConfirmed, ownPubKey, submitTxConstraintsWith)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude

data IntegrationParams = IntegrationParams
  { mmsId :: MajorityMultiSignIdentifier
  , pubKeys :: [PubKey]
  }

correctContract :: IntegrationParams -> Contract w s ContractError ()
correctContract IntegrationParams {mmsId, pubKeys} = do
  pkh <- Ledger.pubKeyHash <$> ownPubKey
  let value = Value.singleton (mintingPolicySymbol mmsId) "Token" 1
      lookups = Constraints.mintingPolicy $ mintingPolicy mmsId
      tx = TxConstraints.mustMintValue value <> TxConstraints.mustPayToPubKey pkh value
  ledgerTx <- submitSignedTxConstraintsWith @Void mmsId pubKeys lookups tx
  void $ awaitTxConfirmed $ Ledger.txId ledgerTx

-- | Attempts to mint a value without invoking the multisign contract at all - should always fail
bypassContract :: IntegrationParams -> Contract w s ContractError ()
bypassContract IntegrationParams {mmsId} = do
  pkh <- Ledger.pubKeyHash <$> ownPubKey
  let value = Value.singleton (mintingPolicySymbol mmsId) "Token" 1
      lookups = Constraints.mintingPolicy $ mintingPolicy mmsId
      tx = TxConstraints.mustMintValue value <> TxConstraints.mustPayToPubKey pkh value
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
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
