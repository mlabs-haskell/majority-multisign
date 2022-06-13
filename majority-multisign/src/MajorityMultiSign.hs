module MajorityMultiSign (endpoints) where

import Control.Monad (forever, void)
import Data.Monoid (Last (Last))
import Ledger (PaymentPubKeyHash)
import MajorityMultiSign.Contracts (initialize, setSignatures)
import MajorityMultiSign.Schema (MajorityMultiSignSchema)
import Plutus.Contract (Contract, ContractError, endpoint, selectList)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx.Prelude

-- | Plutus playground API
endpoints ::
  PaymentPubKeyHash ->
  Contract (Last AssetClass) MajorityMultiSignSchema ContractError ()
endpoints ownPubKeyHash =
  forever $
    selectList
      [ endpoint @"Initialize" (void . initialize ownPubKeyHash toObsState)
      , endpoint @"SetSignatures" setSignatures
      ]
  where
    toObsState :: Maybe (AssetClass -> Last AssetClass)
    toObsState = Just $ Last . Just
