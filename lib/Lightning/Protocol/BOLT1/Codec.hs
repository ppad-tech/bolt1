{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT1.Codec
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Encoding and decoding of BOLT #1 messages.

module Lightning.Protocol.BOLT1.Codec (
    EncodeError(..)
  , DecodeError(..)

  , encode_envelope
  , decode_envelope

  , encode_message
  , decode_message

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

  , ping_response
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.Message
import Lightning.Protocol.BOLT1.Prim
import Lightning.Protocol.BOLT1.TLV
import qualified Lightning.Protocol.BOLT9 as BOLT9

-- | Why a message failed to encode.
data EncodeError
  = EncodeLengthOverflow
    -- ^ a length-prefixed field exceeds 65535 bytes
  | EncodeMessageTooLarge
    -- ^ the message (type and payload) exceeds 65535 bytes
  | EncodeInvalidTlvs
    -- ^ a record in a @_tlvs@ field duplicates a typed field
  deriving (Eq, Show, Generic)

instance NFData EncodeError

-- | Why a message failed to decode.
data DecodeError
  = DecodeInsufficientBytes
    -- ^ the input ended before a field did
  | DecodeTlvError !TlvError
    -- ^ the message's TLV stream is malformed
  | DecodeInvalidTlvValue !Word64
    -- ^ a known TLV record (of the given type) has a malformed value
  | DecodeUnknownEvenType !Word16
    -- ^ an unknown even message type; the connection must be closed
  | DecodeUnknownOddType !Word16
    -- ^ an unknown odd message type; the message must be ignored
  deriving (Eq, Show, Generic)

instance NFData DecodeError

-- framing --------------------------------------------------------------------

-- | Prefix a payload with its message type. Fails if the result
--   exceeds the 65535-byte message limit.
--
--   >>> encode_envelope 19 "\NUL\NUL"
--   Right "\NUL\DC3\NUL\NUL"
encode_envelope :: Word16 -> BS.ByteString -> Either EncodeError BS.ByteString
encode_envelope t payload
  | BS.length payload > 65533 = Left EncodeMessageTooLarge
  | otherwise                 = Right (encode_u16 t <> payload)

-- | Split a message into its type and payload.
--
--   >>> decode_envelope "\NUL\DC3\NUL\NUL"
--   Right (19,"\NUL\NUL")
decode_envelope :: BS.ByteString -> Either DecodeError (Word16, BS.ByteString)
decode_envelope bs = case decode_u16 bs of
  Just r  -> Right r
  Nothing -> Left DecodeInsufficientBytes

-- | Encode a BOLT #1 message, including its type.
--
--   >>> encode_message (MsgPong (Pong "" empty_tlv_stream))
--   Right "\NUL\DC3\NUL\NUL"
encode_message :: Message -> Either EncodeError BS.ByteString
encode_message m = do
  payload <- case m of
    MsgInit a                 -> encode_init a
    MsgError a                -> encode_error a
    MsgWarning a              -> encode_warning a
    MsgPing a                 -> encode_ping a
    MsgPong a                 -> encode_pong a
    MsgPeerStorage a          -> encode_peer_storage a
    MsgPeerStorageRetrieval a -> encode_peer_storage_retrieval a
  encode_envelope (message_type m) payload

-- | Decode a BOLT #1 message, including its type.
--
--   A type that BOLT #1 doesn't define yields 'DecodeUnknownEvenType'
--   or 'DecodeUnknownOddType'; callers dispatching messages from other
--   BOLTs should use 'decode_envelope' first.
--
--   >>> decode_message "\NUL\DC3\NUL\NUL"
--   Right (MsgPong (Pong {pong_ignored = "", pong_tlvs = TlvStream []}))
--   >>> decode_message "\NUL\DC4\NUL\NUL"
--   Left (DecodeUnknownEvenType 20)
decode_message :: BS.ByteString -> Either DecodeError Message
decode_message bs = do
  (t, payload) <- decode_envelope bs
  case t of
    16 -> MsgInit <$> decode_init payload
    17 -> MsgError <$> decode_error payload
    1  -> MsgWarning <$> decode_warning payload
    18 -> MsgPing <$> decode_ping payload
    19 -> MsgPong <$> decode_pong payload
    7  -> MsgPeerStorage <$> decode_peer_storage payload
    9  -> MsgPeerStorageRetrieval <$> decode_peer_storage_retrieval payload
    _ | even t    -> Left (DecodeUnknownEvenType t)
      | otherwise -> Left (DecodeUnknownOddType t)

-- helpers --------------------------------------------------------------------

prefixed :: BS.ByteString -> Either EncodeError BS.ByteString
prefixed bs = maybe (Left EncodeLengthOverflow) Right (encode_u16_prefixed bs)

need :: Maybe a -> Either DecodeError a
need = maybe (Left DecodeInsufficientBytes) Right
{-# INLINE need #-}

-- an extension stream in which no types are known
extension :: BS.ByteString -> Either DecodeError TlvStream
extension bs = either (Left . DecodeTlvError) Right
  (decode_tlv_stream (const False) bs)

-- init -----------------------------------------------------------------------

-- | Encode an 'Init' payload.
encode_init :: Init -> Either EncodeError BS.ByteString
encode_init (Init gf f nets addr tlvs) = do
  gf' <- prefixed (BOLT9.render gf)
  f'  <- prefixed (BOLT9.render f)
  let known = maybe [] (\cs -> [TlvRecord 1 (mconcat (map un_chain_hash cs))])
                nets
           <> maybe [] (\a -> [TlvRecord 3 a]) addr
  stream <- maybe (Left EncodeInvalidTlvs) Right
              (tlv_stream (known <> un_tlv_stream tlvs))
  pure (gf' <> f' <> encode_tlv_stream stream)

-- | Decode an 'Init' payload.
decode_init :: BS.ByteString -> Either DecodeError Init
decode_init bs = do
  (gf, r0) <- need (decode_u16_prefixed bs)
  (f, r1)  <- need (decode_u16_prefixed r0)
  stream   <- either (Left . DecodeTlvError) Right
                (decode_tlv_stream (\t -> t == 1 || t == 3) r1)
  nets <- traverse networks (lookup_tlv 1 stream)
  pure Init
    { init_global_features = BOLT9.parse gf
    , init_features        = BOLT9.parse f
    , init_networks        = nets
    , init_remote_addr     = lookup_tlv 3 stream
    , init_tlvs            = filter_tlv_stream (\t -> t /= 1 && t /= 3) stream
    }
  where
    networks v
      | BS.length v `rem` 32 /= 0 = Left (DecodeInvalidTlvValue 1)
      | otherwise = Right (chunks v)
    chunks !v = case decode_chain_hash v of
      Just (h, rest) -> h : chunks rest
      Nothing        -> []

-- error, warning -------------------------------------------------------------

-- | Encode an 'Error' payload.
encode_error :: Error -> Either EncodeError BS.ByteString
encode_error (Error cid dat tlvs) = do
  dat' <- prefixed dat
  pure (un_channel_id cid <> dat' <> encode_tlv_stream tlvs)

-- | Decode an 'Error' payload.
decode_error :: BS.ByteString -> Either DecodeError Error
decode_error bs = do
  (cid, r0) <- need (decode_channel_id bs)
  (dat, r1) <- need (decode_u16_prefixed r0)
  Error cid dat <$> extension r1

-- | Encode a 'Warning' payload.
encode_warning :: Warning -> Either EncodeError BS.ByteString
encode_warning (Warning cid dat tlvs) = do
  dat' <- prefixed dat
  pure (un_channel_id cid <> dat' <> encode_tlv_stream tlvs)

-- | Decode a 'Warning' payload.
decode_warning :: BS.ByteString -> Either DecodeError Warning
decode_warning bs = do
  (cid, r0) <- need (decode_channel_id bs)
  (dat, r1) <- need (decode_u16_prefixed r0)
  Warning cid dat <$> extension r1

-- ping, pong -----------------------------------------------------------------

-- | Encode a 'Ping' payload.
encode_ping :: Ping -> Either EncodeError BS.ByteString
encode_ping (Ping n ign tlvs) = do
  ign' <- prefixed ign
  pure (encode_u16 n <> ign' <> encode_tlv_stream tlvs)

-- | Decode a 'Ping' payload.
decode_ping :: BS.ByteString -> Either DecodeError Ping
decode_ping bs = do
  (n, r0)   <- need (decode_u16 bs)
  (ign, r1) <- need (decode_u16_prefixed r0)
  Ping n ign <$> extension r1

-- | Encode a 'Pong' payload.
encode_pong :: Pong -> Either EncodeError BS.ByteString
encode_pong (Pong ign tlvs) = do
  ign' <- prefixed ign
  pure (ign' <> encode_tlv_stream tlvs)

-- | Decode a 'Pong' payload.
decode_pong :: BS.ByteString -> Either DecodeError Pong
decode_pong bs = do
  (ign, r0) <- need (decode_u16_prefixed bs)
  Pong ign <$> extension r0

-- | The 'Pong' a node must send in reply to a 'Ping', if any. Per
--   BOLT #1, a ping requesting 65532 or more bytes is ignored.
--
--   >>> ping_response (Ping 4 "" empty_tlv_stream)
--   Just (Pong {pong_ignored = "\NUL\NUL\NUL\NUL", pong_tlvs = TlvStream []})
--   >>> ping_response (Ping 65532 "" empty_tlv_stream)
--   Nothing
ping_response :: Ping -> Maybe Pong
ping_response (Ping n _ _)
  | n >= 65532 = Nothing
  | otherwise  =
      Just (Pong (BS.replicate (fromIntegral n) 0x00) empty_tlv_stream)

-- peer storage ---------------------------------------------------------------

-- | Encode a 'PeerStorage' payload.
encode_peer_storage :: PeerStorage -> Either EncodeError BS.ByteString
encode_peer_storage (PeerStorage blob tlvs) = do
  blob' <- prefixed blob
  pure (blob' <> encode_tlv_stream tlvs)

-- | Decode a 'PeerStorage' payload.
decode_peer_storage :: BS.ByteString -> Either DecodeError PeerStorage
decode_peer_storage bs = do
  (blob, r0) <- need (decode_u16_prefixed bs)
  PeerStorage blob <$> extension r0

-- | Encode a 'PeerStorageRetrieval' payload.
encode_peer_storage_retrieval
  :: PeerStorageRetrieval -> Either EncodeError BS.ByteString
encode_peer_storage_retrieval (PeerStorageRetrieval blob tlvs) = do
  blob' <- prefixed blob
  pure (blob' <> encode_tlv_stream tlvs)

-- | Decode a 'PeerStorageRetrieval' payload.
decode_peer_storage_retrieval
  :: BS.ByteString -> Either DecodeError PeerStorageRetrieval
decode_peer_storage_retrieval bs = do
  (blob, r0) <- need (decode_u16_prefixed bs)
  PeerStorageRetrieval blob <$> extension r0
