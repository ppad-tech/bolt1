# IMPL2 - Replace ByteString Builders

## Goal

Remove ByteString.Builder usage and replace with unsafe ByteString
construction primitives, with careful bounds/length handling.

## Scope

- Library code that currently uses `Data.ByteString.Builder` for encoding.
- Update tests if they depend on builder behavior or output ordering.

## Plan

1) Identify builder usage in `lib/` (e.g., `encodeU16/U32/U64`).
2) Replace with unsafe ByteString creation primitives:
   - Use `BS.create` or `BS.unsafeCreate` with explicit writes.
   - Prefer `poke`/`pokeByteOff` for big-endian layout.
   - Ensure bounds correctness and totality.
3) Remove `bytestring` builder imports and any now-unused deps.
4) Add/adjust tests for encode functions to ensure exact bytes.
5) Run tests to confirm no regressions.

## Notes

- Keep new helpers small and INLINE.
- Validate length fields before unsafe writes to avoid overflow.
- Avoid introducing new dependencies.

