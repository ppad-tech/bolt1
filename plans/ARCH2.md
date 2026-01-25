# ARCH2 - BOLT #1 Stabilization Architecture Notes

## Goals

- Complete fundamental type coverage (signed + truncated unsigned ints).
- Tighten protocol validation (feature padding, message size limits).
- Clarify extension TLV handling in the API surface.
- Decompose the monolithic module into focused components while
  preserving existing public API via re-exports.

## Module Responsibilities

- Lightning.Protocol.BOLT1.Prim
  - Integer encoders/decoders, BigSize, truncated integers.
  - Minimality and bounds checks.

- Lightning.Protocol.BOLT1.TLV
  - TLV record/stream types and parsing strategies.
  - Extension TLV validation policy.

- Lightning.Protocol.BOLT1.Message
  - Message ADTs, feature bitset helpers, init TLVs.
  - Invariant enforcement for message constructors.

- Lightning.Protocol.BOLT1.Codec
  - Message payload encoding/decoding.
  - Envelope handling and message size limits.

- Lightning.Protocol.BOLT1
  - Re-export of public API for external consumers.

## Validation Strategy

- Reject non-minimal BigSize encodings.
- Reject unknown even TLVs in extensions unless the caller uses a
  raw/unsafe TLV decoder explicitly.
- Enforce byte-aligned init feature bitsets.
- Enforce maximum 65535 byte envelope size.

