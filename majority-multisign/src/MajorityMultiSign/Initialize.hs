module MajorityMultiSign.Initialize (initialize) where

import Control.Monad (void)
import Data.Monoid (Last (..))
import Data.Void (Void)
import Ledger (
  AssetClass,
  TokenName,
  pubKeyHash,
  txId,
  validatorHash,
 )
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import MajorityMultiSign.OnChain (validator)
import MajorityMultiSign.Schema (
  MajorityMultiSignDatum,
  MajorityMultiSignSchema,
  MajorityMultiSignValidatorParams (..),
 )
import Plutus.Contract (
  Contract,
  awaitTxConfirmed,
  ownPubKey,
  submitTxConstraintsWith,
  tell,
 )
import Plutus.Contracts.Currency (CurrencyError, currencySymbol, mintContract)
import Plutus.V1.Ledger.Value (assetClass, assetClassValue)
import PlutusTx (toBuiltinData)
import PlutusTx.Prelude

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
