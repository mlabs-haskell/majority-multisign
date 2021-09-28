module MajorityMultiSign.Initialize (initialize) where

-- import Ledger qualified
-- import Ledger.Constraints qualified as Constraints
-- import Ledger.Constraints.TxConstraints qualified as TxConstraints
-- import Ledger.Scripts qualified as Scripts
-- import Ledger.Typed.Scripts qualified as TypedScripts
import MajorityMultiSign.Schema (
  MajorityMultiSignSchema,
 )
import Plutus.Contract (
  Contract,
  ContractError,
 )
import PlutusTx.Prelude

-- import Prelude qualified

initialize :: Contract () MajorityMultiSignSchema ContractError ()
initialize = return ()
