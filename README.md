# ppad-bolt1

[![](https://img.shields.io/hackage/v/ppad-bolt1?color=blue)](https://hackage.haskell.org/package/ppad-bolt1)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt1-lightblue)](https://docs.ppad.tech/bolt1)

A Haskell implementation of the Lightning Network base protocol (BOLT
#1), providing message and TLV encoding/decoding with validation.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > import qualified Data.ByteString as BS
  > import Lightning.Protocol.BOLT1
  >
  > let msg = MsgPingVal (Ping 10 "")
  > let ext = TlvStream [TlvRecord 101 "ext"]
  >
  > Right enc = encodeEnvelope msg (Just ext)
  > enc
  "\NUL\DC2\NUL\n\NUL\NULe\ETXext"
  >
  > decodeEnvelope enc
  Right (Just (MsgPingVal (Ping {pingNumPongBytes = 10, pingIgnored = ""})),
         Just (TlvStream [TlvRecord {tlvType = 101, tlvValue = "ext"}]))
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bolt1](https://docs.ppad.tech/bolt1).

## Performance

The aim is best-in-class performance for message encoding/decoding.
Benchmarks are available under `bench/` and can be run with:

```
$ cabal bench
```

## Security

This is a pre-release library that, at present, claims no security
properties whatsoever.

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

## Attribution

This library implements the Lightning Network BOLT #1 specification:
https://github.com/lightning/bolts/blob/master/01-messaging.md

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
