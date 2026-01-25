# IMPL3 - BOLT #1 Stabilization Pass

## Phase 1: Fundamental Types + Tests

- Implement signed integers (s16/s32/s64) and truncated unsigned
  integers (tu16/tu32/tu64) per BOLT #1.
- Add test vectors from Appendix D (signed integers).
- Add negative tests for minimality/length rules.

## Phase 2: Validation Hardening

- Enforce `init` feature bitset padding/byte alignment.
- Enforce message size limits (type + payload + extension <= 65535).
- Maintain length overflow checks for all u16 length fields.

## Phase 3: Extension TLV Policy

- Ensure unknown even TLVs in extensions cause decode failure.
- Expose clear decode API surface for extensions (raw vs. validated).
- Add tests for unknown even TLV rejection and extension handling.

## Phase 4: Module Split

- Split `Lightning.Protocol.BOLT1` into:
  - `Lightning.Protocol.BOLT1.Prim`
  - `Lightning.Protocol.BOLT1.TLV`
  - `Lightning.Protocol.BOLT1.Message`
  - `Lightning.Protocol.BOLT1.Codec`
- Preserve the existing public API via re-exports.
- Update cabal + test imports accordingly.

## Independent Work Chunks

1) Fundamental type encoding/decoding + vectors/tests.
2) Validation hardening for init features + message size limits.
3) Extension TLV policy changes + test additions.
4) Module split and cabal/test updates.

