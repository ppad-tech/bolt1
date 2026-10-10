{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT1
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- The base protocol of the Lightning Network, per
-- [BOLT #1](https://github.com/lightning/bolts/blob/master/01-messaging.md):
-- fundamental types and their encodings, the TLV format, and the
-- @init@, @error@, @warning@, @ping@, @pong@, @peer_storage@ and
-- @peer_storage_retrieval@ messages.
--
-- The fundamental types and TLV machinery here are shared by the other
-- ppad BOLT libraries.

module Lightning.Protocol.BOLT1 (
  -- * Messages
    Message(..)
  , message_type
  , Init(..)
  , Error(..)
  , Warning(..)
  , Ping(..)
  , Pong(..)
  , ping_response
  , PeerStorage(..)
  , PeerStorageRetrieval(..)

  -- * Encoding and decoding messages
  , encode_message
  , decode_message
  , encode_envelope
  , decode_envelope
  , EncodeError(..)
  , DecodeError(..)

  -- ** Payloads
  , encode_init
  , decode_init
  , encode_error
  , decode_error
  , encode_warning
  , decode_warning
  , encode_ping
  , decode_ping
  , encode_pong
  , decode_pong
  , encode_peer_storage
  , decode_peer_storage
  , encode_peer_storage_retrieval
  , decode_peer_storage_retrieval

  -- * TLV
  , TlvRecord(..)
  , TlvStream
  , tlv_stream
  , un_tlv_stream
  , empty_tlv_stream
  , lookup_tlv
  , filter_tlv_stream
  , encode_tlv_stream
  , decode_tlv_stream
  , TlvError(..)

  -- * Fundamental types
  , ChainHash
  , chain_hash
  , un_chain_hash
  , ChannelId
  , channel_id
  , un_channel_id
  , all_channels
  , Signature
  , signature
  , un_signature
  , Point
  , point
  , un_point
  , PaymentHash
  , payment_hash
  , un_payment_hash
  , PaymentPreimage
  , payment_preimage
  , un_payment_preimage
  , PerCommitmentSecret
  , per_commitment_secret
  , un_per_commitment_secret
  , ShortChannelId(..)
  , short_channel_id
  , scid_block_height
  , scid_tx_index
  , scid_output_index

  -- ** Amounts
  , Satoshi
  , satoshi
  , un_satoshi
  , max_satoshi
  , MilliSatoshi
  , milli_satoshi
  , un_milli_satoshi
  , max_milli_satoshi
  , sat_to_msat
  , msat_to_sat
  , add_sat
  , sub_sat
  , add_msat
  , sub_msat

  -- * Primitive encodings
  -- | Decoders return the decoded value and the remaining input, except
  --   for the truncated integers, which occupy their whole input.
  , encode_u16
  , encode_u32
  , encode_u64
  , decode_u16
  , decode_u32
  , decode_u64
  , encode_s8
  , encode_s16
  , encode_s32
  , encode_s64
  , decode_s8
  , decode_s16
  , decode_s32
  , decode_s64
  , encode_tu16
  , encode_tu32
  , encode_tu64
  , decode_tu16
  , decode_tu32
  , decode_tu64
  , encode_bigsize
  , decode_bigsize
  , encode_u16_prefixed
  , decode_u16_prefixed
  , decode_chain_hash
  , decode_channel_id
  , decode_signature
  , decode_point
  , decode_payment_hash
  , decode_payment_preimage
  , decode_per_commitment_secret
  , encode_short_channel_id
  , decode_short_channel_id
  , encode_satoshi
  , decode_satoshi
  , encode_milli_satoshi
  , decode_milli_satoshi
  ) where

import Lightning.Protocol.BOLT1.Codec
import Lightning.Protocol.BOLT1.Message
import Lightning.Protocol.BOLT1.Prim
import Lightning.Protocol.BOLT1.TLV
