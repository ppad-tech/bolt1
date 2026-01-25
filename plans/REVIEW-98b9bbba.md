# Review: 98b9bbba

## Findings (ordered by severity)

- Medium: `decodeEnvelope` hard-codes `decodeTlvStreamWith (const False)`, so
  every even TLV type in extensions is treated as unknown and rejected. This
  makes it impossible to accept negotiated/known even extension TLVs in the
  future, and forces odd TLVs to be discarded even if a caller wants to
  preserve them. Consider a `decodeEnvelopeWith` that accepts an `isKnown`
  predicate (or returns raw extension bytes) to make extension handling
  configurable.
  (`lib/Lightning/Protocol/BOLT1/Codec.hs:300-320`)

- Low: `encodeMessage` can emit payloads that exceed the 65535 total message
  limit; only `encodeEnvelope` checks size. Since `encodeMessage` is exported,
  callers could bypass the limit unintentionally. If you want the API to enforce
  the spec by default, consider a size check there too.
  (`lib/Lightning/Protocol/BOLT1/Codec.hs:110-156`)

- Low: Tests still use `error` via `assertFailure'`, which is a partial failure
  path (even if only in tests). If you want to keep the “avoid partials”
  discipline, replace with `assertFailure` in an IO context or a total helper.
  (`test/Main.hs:356-362`)

## Open questions

- Should extension TLVs be configurable at the API boundary (predicate for
  known types), or should the library always reject all even extension types
  until specific ones are modeled?
- Should `encodeMessage` enforce the total size limit, or should size
  validation be strictly an envelope concern?

