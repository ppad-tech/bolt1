# ppad-bolt1

[![](https://img.shields.io/hackage/v/ppad-bolt1?color=blue)](https://hackage.haskell.org/package/ppad-bolt1)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt1-lightblue)](https://docs.ppad.tech/bolt1)

A pure Haskell implementation of the Lightning Network base protocol,
[BOLT #1][bolt1]: the fundamental types and their encodings, the TLV
format, and the `init`, `error`, `warning`, `ping`, `pong`,
`peer_storage` and `peer_storage_retrieval` messages.

The fundamental types (channel ids, points, signatures, short channel
ids, bounded amounts, etc.) and the TLV machinery are shared by the
other ppad BOLT libraries.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  > import Lightning.Protocol.BOLT1
  >
  > -- a ping carrying an (odd, hence ignorable) extension record
  > let Just ext = tlv_stream [TlvRecord 101 "ext"]
  > let Right wire = encode_message (MsgPing (Ping 4 "" ext))
  > wire
  "\NUL\DC2\NUL\EOT\NUL\NULe\ETXext"
  >
  > -- unknown odd records are preserved, so re-encoding is exact
  > decode_message wire
  Right (MsgPing (Ping {ping_num_pong_bytes = 4, ping_ignored = "",
    ping_tlvs = TlvStream [TlvRecord {tlv_type = 101, tlv_value = "ext"}]}))
  >
  > -- unknown even message types must close the connection
  > decode_message "\NUL\DC4\NUL"
  Left (DecodeUnknownEvenType 20)
  >
  > -- amounts are bounded at 21M BTC and use checked arithmetic
  > let Just a = satoshi 1000
  > let Just b = satoshi 2500
  > sub_sat a b
  Nothing
  > fmap sat_to_msat (add_sat a b)
  Just (MilliSatoshi 3500000)
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bolt1](https://docs.ppad.tech/bolt1).

## Specification

This library implements BOLT #1 as of lightning/bolts commit
[1aadb719][spec] (2026-09-21); a copy lives in `etc/`.

Feature vectors (in `init`) are represented with [ppad-bolt9][bolt9].
The `remote_addr` address descriptor is kept as raw bytes.

## Performance

The aim is best-in-class performance for pure Haskell code. Run the
benchmarks with:

```
$ cabal bench
```

## Security

This library has received no security audit. It validates every field
the specification constrains when decoding, rejects unknown even message
and TLV types, and keeps secret types (preimages, per-commitment
secrets) out of `Show` output, comparing them in constant time.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-bolt1
```

to get a REPL for the main library.

[bolt1]: https://github.com/lightning/bolts/blob/master/01-messaging.md
[bolt9]: https://git.ppad.tech/bolt9
[spec]: https://github.com/lightning/bolts/tree/1aadb719b4007c4cea0ba6e36b08c4fb53788dee
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
