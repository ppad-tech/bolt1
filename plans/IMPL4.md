# IMPL4 - Type Safety Improvements

## Phase 1: ChannelId Newtype

- Add `ChannelId` newtype to Message module.
- Add `channelId` smart constructor (validates 32 bytes).
- Add `allChannels` constant for the all-zeros channel ID.
- Update `Error` record: `errorChannelId :: !ChannelId`.
- Update `Warning` record: `warningChannelId :: !ChannelId`.
- Update Codec encode/decode functions for Error and Warning.
- Update tests to use new constructors.

## Phase 2: ChainHash Newtype

- Add `ChainHash` newtype to Message module.
- Add `chainHash` smart constructor (validates 32 bytes).
- Update `InitNetworks` variant: `InitNetworks ![ChainHash]`.
- Update TLV `parseInitTlvs` to produce `[ChainHash]`.
- Update TLV `encodeInitTlvs` to accept `[ChainHash]`.
- Update tests to use new constructors.

## Phase 3: Ordered TlvStream

- Add `tlvStream` smart constructor to TLV module.
- Add `unsafeTlvStream` for internal/trusted use.
- Hide `TlvStream` data constructor from public exports.
- Update re-exports in main module.
- Add tests for ordering validation.

## Phase 4: Documentation and Cleanup

- Add Haddock for new types and constructors.
- Update any examples in documentation.
- Verify all tests pass.
- Run benchmarks to ensure no performance regression.

## Independent Work Chunks

Phases 1-3 can be done in parallel:
- Phase 1 (ChannelId) touches Message + Codec + Error/Warning tests.
- Phase 2 (ChainHash) touches Message + TLV + Init tests.
- Phase 3 (TlvStream) touches TLV module + TLV tests.

Phase 4 depends on 1-3 completing.

## Test Updates

Each phase requires corresponding test updates:
- Phase 1: Error/Warning encode/decode tests.
- Phase 2: Init TLV parsing tests, network chain tests.
- Phase 3: TlvStream construction tests (valid ordering, rejection).

## Notes

- Keep `Eq`, `Show`, `NFData` instances for all new types.
- Consider `IsString` instance for `ChannelId`/`ChainHash` if hex
  literals are useful in tests (optional).
- Benchmark decode paths to verify no regression from added
  newtype unwrapping.
