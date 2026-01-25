# Review: 80d0966

## Findings (ordered by severity)

- High: `encodeEnvelope` can append an extension TLV, but `decodeEnvelope` never
  parses or surfaces extensions, so extension data is lost and invalid
  extensions can’t be detected; this breaks roundtrips for any envelope using
  `mext` and contradicts the API comment about invalid extensions.
  (`lib/Lightning/Protocol/BOLT1.hs:512-517`,
  `lib/Lightning/Protocol/BOLT1.hs:638-655`)

- High: `decodeTlvStream` is hard-wired to treat only TLV types 1 and 3 as
  “known,” so any even TLV in other contexts (including future extension TLVs)
  will be rejected even when it should be accepted. This makes the exported TLV
  decoder unusable for anything besides `init_tlvs`.
  (`lib/Lightning/Protocol/BOLT1.hs:264-275`)

- Medium: Length fields are encoded with `fromIntegral (BS.length ...)` and no
  bounds checks, so payloads longer than 65535 bytes will wrap and produce
  invalid encodings instead of failing fast. This affects every `u16` length
  field encoder.
  (`lib/Lightning/Protocol/BOLT1.hs:435-487`)

- Medium: `decodeMessage` treats `MsgUnknown` odd types as
  `DecodeInsufficientBytes`, which is misleading and can cause clients calling
  `decodeMessage` directly to close/abort when the spec says unknown odd should
  be ignored.
  (`lib/Lightning/Protocol/BOLT1.hs:624-636`)

- Low: `unhex` uses `error` on invalid input, which is a partial failure path in
  tests; consider total helpers in tests for consistency with safety guidance.
  (`test/Main.hs:328-331`)

## Open questions / assumptions

- Should `decodeEnvelope` return extension TLVs (e.g., via `Envelope`), or should
  there be a separate decode API for validating and extracting extensions?
- Should `decodeTlvStream` accept a known-type predicate or map so it can be
  reused across message-specific TLVs?

