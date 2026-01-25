{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT1
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Base protocol for the Lightning Network, per
-- [BOLT #1](https://github.com/lightning/bolts/blob/master/01-messaging.md).

module Lightning.Protocol.BOLT1 (
  -- * Message types
    Message(..)
  , MsgType(..)
  , msgTypeWord

  -- ** Setup messages
  , Init(..)
  , Error(..)
  , Warning(..)

  -- ** Control messages
  , Ping(..)
  , Pong(..)

  -- ** Peer storage
  , PeerStorage(..)
  , PeerStorageRetrieval(..)

  -- * TLV
  , TlvRecord(..)
  , TlvStream
  , unTlvStream
  , tlvStream
  , unsafeTlvStream
  , TlvError(..)
  , encodeTlvStream
  , decodeTlvStream
  , decodeTlvStreamWith
  , decodeTlvStreamRaw

  -- ** Init TLVs
  , InitTlv(..)

  -- * Message envelope
  , Envelope(..)

  -- * Encoding
  , EncodeError(..)
  , encodeMessage
  , encodeEnvelope

  -- * Decoding
  , DecodeError(..)
  , decodeMessage
  , decodeEnvelope
  , decodeEnvelopeWith

  -- * Primitive encoding
  , encodeU16
  , encodeU32
  , encodeU64
  , encodeS8
  , encodeS16
  , encodeS32
  , encodeS64
  , encodeTu16
  , encodeTu32
  , encodeTu64
  , encodeMinSigned
  , encodeBigSize

  -- * Primitive decoding
  , decodeU16
  , decodeU32
  , decodeU64
  , decodeS8
  , decodeS16
  , decodeS32
  , decodeS64
  , decodeTu16
  , decodeTu32
  , decodeTu64
  , decodeMinSigned
  , decodeBigSize
  ) where

-- Re-export from sub-modules
import Lightning.Protocol.BOLT1.Prim
import Lightning.Protocol.BOLT1.TLV
import Lightning.Protocol.BOLT1.Message
import Lightning.Protocol.BOLT1.Codec
