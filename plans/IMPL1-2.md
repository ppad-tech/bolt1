# IMPL1-2 - TLV Streams

## Scope

- TLV record and TLV stream types.
- Encode/decode with BOLT #1 validation rules.

## Work

- Strictly increasing type ordering.
- Minimal encoding checks for type/length.
- Length bounds validation.
- Unknown even type fails; unknown odd skipped.

## Tests

- Appendix B vectors if available.
- Property tests for ordering/minimality.
- Negative tests for invalid lengths and unknown even types.

