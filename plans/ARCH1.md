# ARCH1 - ppad-bolt1 BOLT #1 Library Architecture

## Goals

- Provide a safe, total, and performant Haskell implementation of BOLT #1
  message encoding/decoding.
- Encode protocol invariants in types where practical, with smart
  constructors for validation.
- Keep dependencies minimal (base/bytestring/primitive, ppad-*).
- Provide clear error reporting for parse and protocol violations.

## Scope

- BOLT #1 message envelope, TLV stream, and defined messages:
  init, error, warning, ping, pong, peer_storage, peer_storage_retrieval.
- Fundamental types used by BOLT #1: big-endian u/s integers, truncated
  unsigned integers, BigSize, and fixed-size byte fields.

## Module Layout (proposed)

- Lightning.Protocol.BOLT1
  - High-level API and re-exports.

- Lightning.Protocol.BOLT1.Prim
  - Encoding/decoding primitives for:
    u16/u32/u64, s16/s32/s64, truncated unsigned ints, BigSize.
  - Minimal encoding checks, bounded size validation.

- Lightning.Protocol.BOLT1.TLV
  - TLV record and stream types.
  - Encode/decode and validation (ordering, minimal encoding, length bounds,
    unknown even behavior).

- Lightning.Protocol.BOLT1.Message
  - ADTs for BOLT #1 messages and message envelope.
  - Feature bitset types and init TLVs.
  - Smart constructors for messages with validation.

- Lightning.Protocol.BOLT1.Codec
  - Encode/decode for messages and envelopes.
  - Error types and mapping from decode failures to protocol errors.

## Error Model

- Parse errors:
  - non-minimal encoding, insufficient data, length mismatch, invalid TLV
    ordering, unknown even TLV, invalid extension.
- Protocol errors:
  - unknown even message type, invalid message length for known type.

Errors should be structured so callers can decide when to drop/close
connections vs. ignore a message, per spec.

## Performance Strategy

- Strict fields with UNPACK where it pays off.
- INLINE small encode/decode helpers.
- Prefer ByteString builders with manual sizing for small frames.
- Avoid intermediate allocations in TLV parsing by slicing input.

## Public API

- Total encode/decode functions returning Either error message.
- Types re-exported from a single module for consumers:
  message ADTs, TLV types, and common primitives.

## Testing and Benchmarking

- Unit tests from BOLT #1 vectors (BigSize, signed ints).
- Property tests for round-trip and minimal encodings.
- Benchmarks for encode/decode hot paths and allocation tracking.

