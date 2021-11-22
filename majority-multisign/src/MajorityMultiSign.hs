module MajorityMultiSign (endpoints) where

import Control.Monad (forever)
import Data.Monoid (Last)
import MajorityMultiSign.Contracts (initialize, setSignatures)
import MajorityMultiSign.Schema (MajorityMultiSignSchema)
import Plutus.Contract (Contract, ContractError, endpoint, selectList)
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx.Prelude (($))

-- | Plutus playground API
endpoints ::
  PubKeyHash ->
  Contract (Last AssetClass) MajorityMultiSignSchema ContractError ()
endpoints ownPubKeyHash =
  forever $
    selectList
      [ endpoint @"Initialize" (initialize ownPubKeyHash)
      , endpoint @"SetSignatures" setSignatures
      ]
