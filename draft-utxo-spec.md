# Majority Multi-sign Validator

This document describes the majority multi-sign project as a lower-level description of the EUTXO transaction behaviors required by the protocol.

Specifically this will describe all Monetary Policies, Validators, and the relative Native Tokens required for the protocol to function as described in the endpoint-spec.

Specifications for Monetary Policies will describe all native tokens controlled by that policy (by token name), as well as datum types associated with each token (ie. each token is presumed to be in a utxo with a value of exactly 1 token, the datum of this utxo will always be of the type specified). Minting and burning policies for each token (if they have separate logic), and the expected inputs and outputs of those transactions.

Specifications for Validators will describe the Datum / redeemer, validation rules (including Redeemer pattern matches) and the expected inputs and outputs of these transactions.

## Introduction - Project goals
The goal of this project is to provide a multi-key sign of authority for contracts that risk large capitol loss upon leaking of a key.  
The Validator will hold a series of keys and require a majority signing for use of the validator, be that for the intended external usecase, or for modifying the current key set.

The system will need to enable the following workflows
1. Set up with an initial set of pub keys, minting the state token
2. Use the validator to enforce a transaction to be signed by a majority of keys involved.
3. Change the current PubKeyHash set

## Specification Conventions
In order to simplify and clarify the specification, this document adopts a few basic conventions:

- forwarding minting policy pattern
- state/permission/signed/distributed/record token patterns
- Pagination pattern
- distributed list & distributed map patterns

This document describes the IDO project as a lower-level description of the EUTXO transaction behaviors required by the protocol.

Specifically this will describe all Monetary Policies, Validators, and the relative Native Tokens required for the protocol to function as described in the endpoint-spec, and how they relate to each other.

Specifications for Monetary Policies will describe all native tokens controlled by that policy (by token name), as well as datum types associated with each token (ie. each token is presumed to be in a UTXO with a value of exactly 1 token, the datum of this UTXO will always be of the same type as the Token name). Minting and burning policies for each token (if they have separate logic), and the expected inputs and outputs of those transactions. Combined with the Forwarding Monetary Policy and State-Token patterns, this allows us to verify the validity of a datum by the tokens within the UTXO which carries that datum.

Specifications for Validators will describe the Datum / redeemer, validation rules (including Redeemer pattern matches) and the expected inputs and outputs of these transactions.

Validator Datum and Redeemer types are documented using a state/action scheme, the datum will always reflect a State Token held by the validator script, which should be verified to match before trusting any explicit Datum argument. Action types are presented within the spec as Sum constructors with product fields for convenience, within the codebases the Sum constructors should carry product types as parameters, such that no safety issues occur during field access.

Types may be listed as primitives (though semantic newtypes should be used where possible), Or they may include types from Haskell's `base`, the Plutus Api, `plutus-extra`, the CommonTypes section, or types otherwise defined on other components.

Native tokens may be of several types

- Distributed tokens - tokens given to users may be fungible or non-fungible - these will always have a `$` prefix
- State tokens - used to manage and identify datum, either for a Validator script to use for scriptwide-state, or for a single user's state, in both cases, these are non-fungible State tokens 'for a user' generally indicates that the `address` or similar parameter must match that of the user.
- Permission Tokens - these tokens, by nature of being in a transaction, signify permission to perform a given type of action in some other component, this is used for extensible permission systems where validation of one action can be delegated to other scripts ad-hoc.
- Signed Tokens - These Tokens carry `SignedMessage tokenDatumType` rather than `tokenDatumType` and are signed by a designated wallet to signify off-chain authority on a particular topic. Typically used for data that would otherwise bottleneck the system.
- Record Tokens - This tokens represent a record of a past state as a witness to some fact onchain.
Many State and Permission tokens function as witnesses to prove some prior event or state, or to show the provenance of other transaction data.

note: The different Address types each indicate how transactions should be authenticated Address - This indicates a Script or a Wallet, we should verify that the transaction includes a utxo from this address OR is signed by the address PubKeyHash - This indicates a wallet, we should verify that the transaction is signed by this PubKeyHash if necessary ValidatorHash - This indicates a script, we should check for an included utxo from this address.

## Unanswered Questions

## Known issues
- `MajorityMultiSignDatum` uses a list of keys for its state - it is possible for this to expand to such a size that the datum becomes unusable. This should be bounded at an Offchain and Onchain level to a sensible value, such as 10.

## Notes

## Common Types
```haskell
data MajorityMultiSignIdentifier =
  MajorityMultiSignIdentifier 
    { asset :: AssetClass
    }
```

## One-Shot Tokens
These tokens are minted as unique NFTs, scripts which need them should be parameterized over their `AssetClass`.

Tools to mint these are currently in `Plutus.Contracts.Currency`

### MajorityMultiSignDatum Token
Purpose: This token is used to store the list of current pubkeys needed for signature.
It is minted as the contract is deployed, and sent to the resulting precomputed address by applying the `AssetClass` as a parameter to the `MajorityMultiSignValidator`

Token Type: This functions as a State token for the `MajorityMultiSignValidator`, as well as a permission token for any external contract using this project.  

Carries Datum:
```haskell
MajorityMultiSignDatum
  { signers :: [PubKeyHash]
  }
```
Initialized to
```haskell
MajorityMultiSignDatum -- provided by the initialize contract
```
Cannot be burned.  
Can only be minted once.  

Inputs:
- Fees in (from USER)

Outputs:
- Fees out reminder -> USER wallet
- MajorityMultiSignDatum Token (MINTED) -> MajorityMultiSignValidator address parameterised by the minted token `AssetClass`

## MajorityMultiSignValidator
Parameters:
```haskell
MajorityMultiSignValidatorParams
  { asset :: AssetClass
  }
```

Purpose: Ensures enough signatures for usage or changing of MajorityMultiSignDatum token

Datum: `MajorityMultiSignDatum`

Redeemer:
```haskell
MajorityMultiSignRedeemer
  = UseSignaturesAct
  | UpdateKeysAct
    { keys :: [PubKeyHash]
    }
```

Both actions in this validator require a majority signing of the keys present.  
They must all include the `MajorityMultiSignDatum` token, returning it back to the validator.  
They must all be signed by at least half of the keys in the `MajorityMultiSignDatum.signers`, rounded up.

### UseSignaturesAct
Purpose: Simply allow the `MajorityMultiSignDatum` to be used (and returned) given signatures provided. This is the usual usecase of this project.

Validation rules:
- More than half of `MajorityMultiSignDatum.signers` must sign the transaction
- `MajorityMultiSignDatum` must be sent back to `MajorityMultiSignValidator` unmodified

Inputs:
- Fees in from USER
- `MajorityMultiSignDatum` from `MajorityMultiSignValidator`

Outputs:
- Fees out raminder -> User wallet
- `MajorityMultiSignDatum` -> `MajorityMultiSignValidator` unmodified

### UpdateKeysAct
Purpose: Allows the key set in `MajorityMultiSignDatum` to be updated, given majority signature

Validation rules:
- More than half of `MajorityMultiSignDatum.signers` must sign the transaction
- `MajorityMultiSignDatum` must be sent back to `MajorityMultiSignValidator`
- `MajorityMultiSignDatum.signers` updated to `UpdateKeysAct.keys`

Inputs:
- Fees in from USER
- `MajorityMultiSignDatum` from `MajorityMultiSignValidator`

Outputs:
- Fees out raminder -> User wallet
- `MajorityMultiSignDatum` -> `MajorityMultiSignValidator` updated as above

## Contracts
### Initialise contract
Parameters:
```haskell
MajorityMultiSignDatum
```
This will use the `Plutus.Contracts.Currency` contract to mint the initial `MajorityMultiSignDatum` token, calculate the address of the validator using the given params, and send the token there with the correct default datum.

### `submitSignedTxConstraintsWith`
*Note: This is implemented not as a usual contract, but as a function that takes a transactions lookups and constraints, and adds everything needed to add the multisign validator to the transaction.*
Parameters: `MajorityMultiSignIdentifier -> [Pubkey] -> ScriptLookups Any -> TxConstraints (RedeemerType a) (DatumType a) -> Contract w s ContractError Tx`.

This will submit a transaction with the multisign elements added to the lookups and constraints. As such, the tail of the type signature matches that of `submitConstraintsWith`.
