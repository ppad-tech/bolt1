# ARCH3 - Type Safety Improvements

## Goals

- Encode fixed-size byte invariants into the type system.
- Enforce TLV stream ordering at construction time.
- Eliminate runtime validation where possible via smart constructors.

## New Types

### ChannelId

32-byte channel identifier used in Error and Warning messages.

```haskell
newtype ChannelId = ChannelId { unChannelId :: BS.ByteString }

channelId :: BS.ByteString -> Maybe ChannelId
channelId bs
  | BS.length bs == 32 = Just (ChannelId bs)
  | otherwise = Nothing

-- | The all-zeros channel ID (refers to all channels).
allChannels :: ChannelId
```

Replaces raw `ByteString` in `Error` and `Warning` message types.

### ChainHash

32-byte chain hash used in Init TLV networks field.

```haskell
newtype ChainHash = ChainHash { unChainHash :: BS.ByteString }

chainHash :: BS.ByteString -> Maybe ChainHash
chainHash bs
  | BS.length bs == 32 = Just (ChainHash bs)
  | otherwise = Nothing
```

`InitNetworks` changes from `[BS.ByteString]` to `[ChainHash]`.

### Ordered TlvStream

TLV streams must have strictly increasing type values. Currently
validated at decode time but not enforced at construction.

```haskell
newtype TlvStream = TlvStream { unTlvStream :: [TlvRecord] }

-- | Smart constructor that validates ordering.
tlvStream :: [TlvRecord] -> Maybe TlvStream

-- | Build from records known to be ordered (internal use).
unsafeTlvStream :: [TlvRecord] -> TlvStream
```

The raw constructor becomes internal; external code uses the smart
constructor or decode functions (which validate ordering).

## Module Changes

### Lightning.Protocol.BOLT1.Message

- Add `ChannelId` newtype + smart constructor + `allChannels`.
- Update `Error` and `Warning` to use `ChannelId`.
- Update `InitNetworks` to use `[ChainHash]`.
- Add `ChainHash` newtype + smart constructor.

### Lightning.Protocol.BOLT1.TLV

- Hide `TlvStream` constructor from public API.
- Export `tlvStream` smart constructor.
- Export `unsafeTlvStream` for internal/advanced use.
- Decoders already validate ordering; no changes needed there.

### Lightning.Protocol.BOLT1.Codec

- Update Error/Warning encode/decode to use `ChannelId`.
- Update Init TLV encode/decode to use `ChainHash`.

### Lightning.Protocol.BOLT1

- Re-export new types and constructors.

## Validation Strategy

- `ChannelId` and `ChainHash`: validate length at construction.
- `TlvStream`: validate strictly-increasing types at construction.
- Decoders produce validated types directly.
- Encoders accept only validated types (no runtime checks needed).

## API Impact

Breaking changes to:
- `Error` and `Warning` record fields (ByteString -> ChannelId).
- `InitNetworks` constructor (ByteString list -> ChainHash list).
- `TlvStream` constructor (now hidden; use smart constructor).

These are source-breaking but type-safe migrations.
