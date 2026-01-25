# IMPL1-1 - Core Primitives

## Scope

- Big-endian unsigned/signed integers (u16/u32/u64, s16/s32/s64).
- Truncated unsigned integers (tu16/tu32/tu64) with minimal encoding.
- BigSize encode/decode with minimal checks.

## Work

- Implement encode/decode functions with total APIs.
- Add minimality and bounds validation.
- Provide strict, small helpers for hot paths.

## Tests

- Appendix A BigSize vectors.
- Appendix D signed integer vectors.
- Negative tests for non-minimal encodings.

