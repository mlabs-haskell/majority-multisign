module MajorityMultiSign (endpoints) where

import Data.Monoid (Last)
import MajorityMultiSign.Contracts (initialize, setSignature)
import MajorityMultiSign.Schema (MajorityMultiSignSchema)
import Plutus.Contract (Contract, ContractError, endpoint, selectList)
import Plutus.V1.Ledger.Value (AssetClass)

endpoints :: Contract (Last AssetClass) MajorityMultiSignSchema ContractError ()
endpoints =
  selectList
    [ endpoint @"Initialize" initialize
    , endpoint @"SetSignature" setSignature
    ]