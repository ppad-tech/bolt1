# IMPL1-4 - Message Codec Integration

## Scope

- Encode/decode for each BOLT #1 message type.
- Envelope encoding with optional extension TLV.
- Enforce unknown-type/extension behavior per spec.

## Work

- Decode known types with strict length checks.
- Unknown odd type: ignore.
- Unknown even type: fail (close connection).
- Invalid extension TLV: fail (close connection).

## Tests

- Known-good vectors per message type.
- Negative tests for invalid lengths and invalid TLV extension.
- Property tests for encode/decode invariants.

