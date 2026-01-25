# IMPL1-3 - Message Types

## Scope

- ADTs for BOLT #1 messages:
  - init (including init_tlvs: networks, remote_addr)
  - error, warning
  - ping, pong
  - peer_storage, peer_storage_retrieval
- Message envelope with type + payload + optional extension TLV.
- Feature bitset modeling and helpers.

## Work

- Define strict, UNPACKed fields where helpful.
- Add smart constructors for validated fields.
- Enforce padding/byte-aligned feature lengths.

## Tests

- Unit tests for constructors and invariants.
- Roundtrip tests for type-level encodings.

