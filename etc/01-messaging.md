# BOLT #1: Base Protocol

## Overview

This protocol assumes an underlying authenticated and ordered transport mechanism that takes care of framing individual messages.
[BOLT #8](08-transport.md) specifies the canonical transport layer used in Lightning, though it can be replaced by any transport that fulfills the above guarantees.

The default TCP port depends on the network used. The most common networks are:

- Bitcoin mainnet with port number 9735 or the corresponding hexadecimal `0x2607`;
- Bitcoin testnet with port number 19735 (`0x4D17`);
- Bitcoin signet with port number 39735 (`0x9B37`).

The Unicode code point for LIGHTNING <sup>[1](#reference-1)</sup>, and the port convention try to follow the Bitcoin Core convention.

All data fields are unsigned big-endian unless otherwise specified.

## Table of Contents

  * [Connection Handling and Multiplexing](#connection-handling-and-multiplexing)
  * [Lightning Message Format](#lightning-message-format)
  * [Type-Length-Value Format](#type-length-value-format)
  * [Fundamental Types](#fundamental-types)
  * [Setup Messages](#setup-messages)
    * [The `init` Message](#the-init-message)
    * [The `error` and `warning` Messages](#the-error-and-warning-messages)
  * [Control Messages](#control-messages)
    * [The `ping` and `pong` Messages](#the-ping-and-pong-messages)
  * [Peer Storage](#peer-storage)
    * [The `peer_storage` and `peer_storage_retrieval` Messages](#the-peer_storage-and-peer_storage_retrieval-messages)
  * [Appendix A: BigSize Test Vectors](#appendix-a-bigsize-test-vectors)
  * [Appendix B: Type-Length-Value Test Vectors](#appendix-b-type-length-value-test-vectors)
  * [Appendix C: Message Extension](#appendix-c-message-extension)
  * [Appendix D: Signed Integers Test Vectors](#appendix-d-signed-integers-test-vectors)
  * [Acknowledgments](#acknowledgments)
  * [References](#references)
  * [Authors](#authors)

## Connection Handling and Multiplexing

Implementations MUST use a single connection per peer; channel messages (which include a channel ID) are multiplexed over this single connection.

## Lightning Message Format

After decryption, all Lightning messages are of the form:

1. `type`: a 2-byte big-endian field indicating the type of message
2. `payload`: a variable-length payload that comprises the remainder of
   the message and that conforms to a format matching the `type`
3. `extension`: an optional [TLV stream](#type-length-value-format)

The `type` field indicates how to interpret the `payload` field.
The format for each individual type is defined by a specification in this repository.
The type follows the _it's ok to be odd_ rule, so nodes MAY send _odd_-numbered types without ascertaining that the recipient understands it.

The messages are grouped logically into five groups, ordered by the most significant bit that is set:

  - Setup & Control (types `0`-`31`): messages related to connection setup, control, supported features, and error reporting (described below)
  - Channel (types `32`-`127`): messages used to setup and tear down micropayment channels (described in [BOLT #2](02-peer-protocol.md))
  - Commitment (types `128`-`255`): messages related to updating the current commitment transaction, which includes adding, revoking, and settling HTLCs as well as updating fees and exchanging signatures (described in [BOLT #2](02-peer-protocol.md))
  - Routing (types `256`-`511`): messages containing node and channel announcements, as well as any active route exploration (described in [BOLT #7](07-routing-gossip.md))
  - Custom (types `32768`-`65535`): experimental and application-specific messages

The size of the message is required by the transport layer to fit into a 2-byte unsigned int; therefore, the maximum possible size is 65535 bytes.

A sending node:
  - MUST NOT send an evenly-typed message not listed here without prior negotiation.
  - MUST NOT send evenly-typed TLV records in the `extension` without prior negotiation.
  - that negotiates an option in this specification:
    - MUST include all the fields annotated with that option.
  - When defining custom messages:
    - SHOULD pick a random `type` to avoid collision with other custom types.
    - SHOULD pick a `type` that doesn't conflict with other experiments listed in [this issue](https://github.com/lightningnetwork/lightning-rfc/issues/716).
    - SHOULD pick an odd `type` identifiers when regular nodes should ignore the
      additional data.
    - SHOULD pick an even `type` identifiers when regular nodes should reject
      the message and close the connection.

A receiving node:
  - upon receiving a message of _odd_, unknown type:
    - MUST ignore the received message.
  - upon receiving a message of _even_, unknown type:
    - MUST close the connection.
    - MAY fail the channels.
  - upon receiving a known message with insufficient length for the contents:
    - MUST close the connection.
    - MAY fail the channels.
  - upon receiving a message with an `extension`:
    - MAY ignore the `extension`.
    - Otherwise, if the `extension` is invalid:
      - MUST close the connection.
      - MAY fail the channels.

### Rationale

By default `SHA2` and Bitcoin public keys are both encoded as
big endian, thus it would be unusual to use a different endian for
other fields.

Length is limited to 65535 bytes by the cryptographic wrapping, and
messages in the protocol are never more than that length anyway.

The _it's ok to be odd_ rule allows for future optional extensions
without negotiation or special coding in clients. The _extension_ field
similarly allows for future expansion by letting senders include additional
TLV data. Note that an _extension_ field can only be added when the message
`payload` doesn't already fill the 65535 bytes maximum length.

Implementations may prefer to have message data aligned on an 8-byte
boundary (the largest natural alignment requirement of any type here);
however, adding a 6-byte padding after the type field was considered
wasteful: alignment may be achieved by decrypting the message into
a buffer with 6-bytes of pre-padding.

## Type-Length-Value Format

Throughout the protocol, a TLV (Type-Length-Value) format is used to allow for
the backwards-compatible addition of new fields to existing message types.

A `tlv_record` represents a single field, encoded in the form:

* [`bigsize`: `type`]
* [`bigsize`: `length`]
* [`length`: `value`]

A `tlv_stream` is a series of (possibly zero) `tlv_record`s, represented as the
concatenation of the encoded `tlv_record`s. When used to extend existing
messages, a `tlv_stream` is typically placed after all currently defined fields.

The `type` is encoded using the BigSize format. It functions as a
message-specific, 64-bit identifier for the `tlv_record` determining how the
contents of `value` should be decoded. `type` identifiers below 2^16 are
reserved for use in this specification. `type` identifiers greater than or equal
to 2^16 are available for custom records. Any record not defined in this
specification is considered a custom record. This includes experimental and
application-specific messages.

The `length` is encoded using the BigSize format signaling the size of
`value` in bytes.

The `value` depends entirely on the `type`, and should be encoded or decoded
according to the message-specific format determined by `type`.

### Requirements

The sending node:
 - MUST order `tlv_record`s in a `tlv_stream` by strictly-increasing `type`,
   hence MUST not produce more than a single TLV record with the same `type`
 - MUST minimally encode `type` and `length`.
 - When defining custom record `type` identifiers:
   - SHOULD pick random `type` identifiers to avoid collision with other
     custom types.
   - SHOULD pick odd `type` identifiers when regular nodes should ignore the
     additional data.
   - SHOULD pick even `type` identifiers when regular nodes should reject the
     full tlv stream containing the custom record.
 - SHOULD NOT use redundant, variable-length encodings in a `tlv_record`.

The receiving node:
 - if zero bytes remain before parsing a `type`:
   - MUST stop parsing the `tlv_stream`.
 - if a `type` or `length` is not minimally encoded:
   - MUST fail to parse the `tlv_stream`.
 - if decoded `type`s are not strictly-increasing (including situations when
   two or more occurrences of the same `type` are met):
   - MUST fail to parse the `tlv_stream`.
 - if `length` exceeds the number of bytes remaining in the message:
   - MUST fail to parse the `tlv_stream`.
 - if `type` is known:
   - MUST decode the next `length` bytes using the known encoding for `type`.
   - if `length` is not exactly equal to that required for the known encoding
     for `type`:
     - MUST fail to parse the `tlv_stream`.
   - if variable-length fields within the known encoding for `type` are not
     minimal:
     - MUST fail to parse the `tlv_stream`.
 - otherwise, if `type` is unknown:
   - if `type` is even:
     - MUST fail to parse the `tlv_stream`.
   - otherwise, if `type` is odd:
     - MUST discard the next `length` bytes.

### Rationale

The primary advantage in using TLV is that a reader is able to ignore new fields
that it does not understand, since each field carries the exact size of the
encoded element. Without TLV, even if a node does not wish to use a particular
field, the node is forced to add parsing logic for that field in order to
determine the offset of any fields that follow.

The strict monotonicity constraint ensures that all `type`s are unique and can
appear at most once. Fields that map to complex objects, e.g. vectors, maps, or
structs, should do so by defining the encoding such that the object is
serialized within a single `tlv_record`. The uniqueness constraint, among other
things, enables the following optimizations:
 - canonical ordering is defined independent of the encoded `value`s.
 - canonical ordering can be known at compile-time, rather than being determined
   dynamically at the time of encoding.
 - verifying canonical ordering requires less state and is less-expensive.
 - variable-size fields can reserve their expected size up front, rather than
   appending elements sequentially and incurring double-and-copy overhead.

The use of a bigsize for `type` and `length` permits a space savings for small
`type`s or short `value`s. This potentially leaves more space for application
data over the wire or in an onion payload.

All `type`s must appear in increasing order to create a canonical encoding of
the underlying `tlv_record`s. This is crucial when computing signatures over a
`tlv_stream`, as it ensures verifiers will be able to recompute the same message
digest as the signer. Note that the canonical ordering over the set of fields
can be enforced even if the verifier does not understand what the fields
contain.

Writers should avoid using redundant, variable-length encodings in a
`tlv_record` since this results in encoding the length twice and complicates
computing the outer length. As an example, when writing a variable length byte
array, the `value` should contain only the raw bytes and forgo an additional
internal length since the `tlv_record` already carries the number of bytes that
follow. On the other hand, if a `tlv_record` contains multiple, variable-length
elements then this would not be considered redundant, and is needed to allow the
receiver to parse individual elements from `value`.

## Fundamental Types

Various fundamental types are referred to in the message specifications:

* `byte`: an 8-bit byte
* `s8`: an 8-bit signed integer
* `u16`: a 2 byte unsigned integer
* `s16`: a 2 byte signed integer
* `u32`: a 4 byte unsigned integer
* `s32`: a 4 byte signed integer
* `u64`: an 8 byte unsigned integer
* `s64`: an 8 byte signed integer

Signed integers use standard big-endian two's complement representation
(see test vectors [below](#appendix-d-signed-integers-test-vectors)).

For the final value in TLV records, truncated integers may be used. Leading
zeros in truncated integers MUST be omitted:

* `tu16`: a 0 to 2 byte truncated unsigned integer
* `tu32`: a 0 to 4 byte truncated unsigned integer
* `tu64`: a 0 to 8 byte truncated unsigned integer

When used to encode amounts, the previous fields MUST comply with the upper
bound of 21 million BTC:

* satoshi amounts MUST be at most `0x000775f05a074000`
* milli-satoshi amounts MUST be at most `0x1d24b2dfac520000`

The following convenience types are also defined:

* `chain_hash`: a 32-byte chain identifier (see [BOLT #0](00-introduction.md#glossary-and-terminology-guide))
* `channel_id`: a 32-byte channel_id (see [BOLT #2](02-peer-protocol.md#definition-of-channel-id))
* `sha256`: a 32-byte SHA2-256 hash
* `signature`: a 64-byte bitcoin Elliptic Curve signature
* `bip340sig`: a 64-byte bitcoin Elliptic Curve Schnorr signature as per [BIP-340](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki)
* `point`: a 33-byte Elliptic Curve point (compressed encoding as per [SEC 1 standard](http://www.secg.org/sec1-v2.pdf#subsubsection.2.3.3))
* `short_channel_id`: an 8 byte value identifying a channel (see [BOLT #7](07-routing-gossip.md#definition-of-short_channel_id))
* `sciddir_or_pubkey`: either 9 or 33 bytes referencing or identifying a node
* `bigsize`: a variable-length, unsigned integer similar to Bitcoin's CompactSize encoding, but big-endian. Described in [BigSize](#appendix-a-bigsize-test-vectors).
* `utf8`: a byte as part of a UTF-8 string.

## Setup Messages

### The `init` Message

Once authentication is complete, the first message reveals the features
supported or required by this node, even if this is a reconnection.

[BOLT #9](09-features.md) specifies lists of features. Each feature is
generally represented by 2 bits. The least-significant bit is numbered 0,
which is _even_, and the next most significant bit is numbered 1, which is
_odd_. For historical reasons, features are divided into global and local
feature bitmasks.

A feature is *offered* if a peer set it in the `init` message for the current
connection (as either even or odd). A feature is *negotiated* if either both
peers offered it, or the local node offered it as even: it can assume the peer
supports it, as it did not disconnect as it would be required to do.

The `features` field MUST be padded to bytes with 0s.

1. type: 16 (`init`)
2. data:
   * [`u16`:`gflen`]
   * [`gflen*byte`:`globalfeatures`]
   * [`u16`:`flen`]
   * [`flen*byte`:`features`]
   * [`init_tlvs`:`tlvs`]

1. `tlv_stream`: `init_tlvs`
2. types:
    1. type: 1 (`networks`)
    2. data:
        * [`...*chain_hash`:`chains`]
    1. type: 3 (`remote_addr`)
    2. data:
        * [`...*byte`:`data`]

### The `error` and `warning` Messages

For simplicity of diagnosis, it's often useful to tell a peer that something
is incorrect.

1. type: 17 (`error`)
2. data:
   * [`channel_id`:`channel_id`]
   * [`u16`:`len`]
   * [`len*byte`:`data`]

1. type: 1 (`warning`)
2. data:
   * [`channel_id`:`channel_id`]
   * [`u16`:`len`]
   * [`len*byte`:`data`]

## Control Messages

### The `ping` and `pong` Messages

In order to allow for the existence of long-lived TCP connections, at times it
may be required that both ends keep alive the TCP connection at the application
level. Such messages also allow obfuscation of traffic patterns.

1. type: 18 (`ping`)
2. data:
    * [`u16`:`num_pong_bytes`]
    * [`u16`:`byteslen`]
    * [`byteslen*byte`:`ignored`]

1. type: 19 (`pong`)
2. data:
    * [`u16`:`byteslen`]
    * [`byteslen*byte`:`ignored`]

## Peer Storage

### The `peer_storage` and `peer_storage_retrieval` Messages

Nodes that advertise the `option_provide_storage` feature offer storing
arbitrary data for their peers.

1. type: 7 (`peer_storage`)
2. data:
    * [`u16`:`length`]
    * [`length*byte`:`blob`]

1. type: 9 (`peer_storage_retrieval`)
2. data:
    * [`u16`:`length`]
    * [`length*byte`:`blob`]

## Appendix A: BigSize Test Vectors

Values encoded with BigSize will produce an encoding of either 1, 3, 5, or 9
bytes depending on the size of the integer. The encoding is a piece-wise
function that takes a `uint64` value `x` and produces:

```
        uint8(x)                if x < 0xfd
        0xfd + be16(uint16(x))  if x < 0x10000
        0xfe + be32(uint32(x))  if x < 0x100000000
        0xff + be64(x)          otherwise.
```

### BigSize Decoding Tests

```json
[
    { "name": "zero", "value": 0, "bytes": "00" },
    { "name": "one byte high", "value": 252, "bytes": "fc" },
    { "name": "two byte low", "value": 253, "bytes": "fd00fd" },
    { "name": "two byte high", "value": 65535, "bytes": "fdffff" },
    { "name": "four byte low", "value": 65536, "bytes": "fe00010000" },
    { "name": "four byte high", "value": 4294967295, "bytes": "feffffffff" },
    { "name": "eight byte low", "value": 4294967296,
      "bytes": "ff0000000100000000" },
    { "name": "eight byte high", "value": 18446744073709551615,
      "bytes": "ffffffffffffffffff" }
]
```

## Appendix D: Signed Integers Test Vectors

```json
[
    { "value": 0, "bytes": "00" },
    { "value": 42, "bytes": "2a" },
    { "value": -42, "bytes": "d6" },
    { "value": 127, "bytes": "7f" },
    { "value": -128, "bytes": "80" },
    { "value": 128, "bytes": "0080" },
    { "value": -129, "bytes": "ff7f" },
    { "value": 15000, "bytes": "3a98" },
    { "value": -15000, "bytes": "c568" },
    { "value": 32767, "bytes": "7fff" },
    { "value": -32768, "bytes": "8000" },
    { "value": 32768, "bytes": "00008000" },
    { "value": -32769, "bytes": "ffff7fff" },
    { "value": 21000000, "bytes": "01406f40" },
    { "value": -21000000, "bytes": "febf90c0" },
    { "value": 2147483647, "bytes": "7fffffff" },
    { "value": -2147483648, "bytes": "80000000" },
    { "value": 2147483648, "bytes": "0000000080000000" },
    { "value": -2147483649, "bytes": "ffffffff7fffffff" },
    { "value": 500000000000, "bytes": "000000746a528800" },
    { "value": -500000000000, "bytes": "ffffff8b95ad7800" },
    { "value": 9223372036854775807, "bytes": "7fffffffffffffff" },
    { "value": -9223372036854775808, "bytes": "8000000000000000" }
]
```

## References

1. <a id="reference-1">http://www.unicode.org/charts/PDF/U2600.pdf</a>

![Creative Commons License](https://i.creativecommons.org/l/by/4.0/88x31.png "License CC-BY")
<br>
This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).
