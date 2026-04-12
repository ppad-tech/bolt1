# IMPL5: Benchmark Suite Population

Populate the empty criterion (wall-time) and weigh (allocation) benchmark
suites with comprehensive coverage of encoding/decoding operations.

## Overview

Current state:
- `bench/Main.hs`: empty criterion suite (just `defaultMain []`)
- `bench/Weight.hs`: empty weigh suite (just `pure ()`)
- All types have NFData instances (ready for benchmarking)

## Benchmark Categories

### 1. Primitive Encoding (Prim.hs)

Fixed-size unsigned:
- `encodeU16`, `encodeU32`, `encodeU64`

Fixed-size signed:
- `encodeS8`, `encodeS16`, `encodeS32`, `encodeS64`

Truncated unsigned (variable-size):
- `encodeTu16`, `encodeTu32`, `encodeTu64`
- Test multiple size classes (0, small, medium, max)

Special encodings:
- `encodeMinSigned` (boundary cases: -128, 127, 128, -129, large)
- `encodeBigSize` (0, 252, 253, 65535, 65536, large)

### 2. Primitive Decoding (Prim.hs)

Fixed-size unsigned:
- `decodeU16`, `decodeU32`, `decodeU64`

Fixed-size signed:
- `decodeS8`, `decodeS16`, `decodeS32`, `decodeS64`

Truncated unsigned:
- `decodeTu16`, `decodeTu32`, `decodeTu64`

Special decodings:
- `decodeMinSigned`
- `decodeBigSize`

### 3. TLV Operations (TLV.hs)

Encoding:
- `encodeTlvRecord` (single record)
- `encodeTlvStream` (varying sizes: 1, 5, 20 records)

Decoding:
- `decodeTlvStreamRaw` (varying sizes)
- `decodeTlvStream` (with init validation)
- `decodeTlvStreamWith` (custom predicate)

Init TLV:
- `parseInitTlvs`
- `encodeInitTlvs`

### 4. Message Encoding (Codec.hs)

Individual messages:
- `encodeInit` (minimal, with TLVs)
- `encodeError` (minimal, with data)
- `encodeWarning`
- `encodePing` (minimal, with padding)
- `encodePong`
- `encodePeerStorage`
- `encodePeerStorageRetrieval`

Envelope:
- `encodeMessage` (each message type)
- `encodeEnvelope` (with/without extension TLVs)

### 5. Message Decoding (Codec.hs)

Individual messages:
- `decodeInit`
- `decodeError`
- `decodeWarning`
- `decodePing`
- `decodePong`
- `decodePeerStorage`
- `decodePeerStorageRetrieval`

Envelope:
- `decodeMessage`
- `decodeEnvelope`
- `decodeEnvelopeWith`

### 6. Round-Trip Benchmarks

Encode then decode for each message type:
- Measures combined codec performance
- Verifies no accumulation overhead

## Implementation Steps

Steps 1-3 can be done in parallel (independent files).

### Step 1: Create test fixtures module

Create `bench/Fixtures.hs` with:
- Sample ByteStrings of various sizes
- Pre-constructed message values (Init, Error, Ping, etc.)
- Sample TlvStream values
- Encoded message bytes for decode benchmarks

### Step 2: Implement criterion benchmarks (bench/Main.hs)

Structure:
```haskell
main = defaultMain
  [ bgroup "prim/encode" [...]
  , bgroup "prim/decode" [...]
  , bgroup "tlv/encode" [...]
  , bgroup "tlv/decode" [...]
  , bgroup "message/encode" [...]
  , bgroup "message/decode" [...]
  , bgroup "envelope" [...]
  , bgroup "roundtrip" [...]
  ]
```

Use `bench`, `nf`, `whnf` appropriately:
- `nf` for functions returning data structures
- `whnf` for functions returning strict values

### Step 3: Implement weigh benchmarks (bench/Weight.hs)

Structure:
```haskell
main = mainWith $ do
  func "encodeU16" encodeU16 0x1234
  func "encodeU32" encodeU32 0x12345678
  ...
  func "encodeInit/minimal" encodeInit minimalInit
  func "encodeInit/with-tlvs" encodeInit initWithTlvs
  ...
```

Track allocations for:
- Primitive encoders (to verify Builder overhead)
- Message encoders (to establish baselines)
- TLV stream operations (to verify accumulator behavior)

### Step 4: Update cabal file if needed

Verify `bench/Fixtures.hs` is included in both benchmark stanzas.
Add any missing `other-modules` entries.

### Step 5: Run and validate

```
cabal bench bolt1-bench
cabal bench bolt1-weigh
```

Verify:
- All benchmarks run without error
- Results are reasonable (no obvious performance cliffs)
- Allocation tracking captures expected patterns

## Test Data Sizes

Use consistent sizing for comparison:
- "minimal": smallest valid message
- "small": ~64 bytes payload
- "medium": ~1KB payload
- "large": ~16KB payload (approaching protocol limits)

## Notes

- Keep fixture generation pure (no IO in benchmark loops)
- Use `env` combinator for expensive setup if needed
- Consider adding `NOINLINE` to fixtures to prevent constant folding
- Document any surprising results in comments
