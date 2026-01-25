{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Lightning.Protocol.BOLT1.Codec
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Message encoding and decoding for BOLT #1.

module Lightning.Protocol.BOLT1.Codec (
  -- * Encoding errors
    EncodeError(..)

  -- * Message encoding
  , encodeInit
  , encodeError
  , encodeWarning
  , encodePing
  , encodePong
  , encodePeerStorage
  , encodePeerStorageRetrieval
  , encodeMessage
  , encodeEnvelope

  -- * Decoding errors
  , DecodeError(..)

  -- * Message decoding
  , decodeInit
  , decodeError
  , decodeWarning
  , decodePing
  , decodePong
  , decodePeerStorage
  , decodePeerStorageRetrieval
  , decodeMessage
  , decodeEnvelope
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when, unless)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.Prim
import Lightning.Protocol.BOLT1.TLV
import Lightning.Protocol.BOLT1.Message

-- Encoding errors -------------------------------------------------------------

-- | Encoding errors.
data EncodeError
  = EncodeLengthOverflow   -- ^ Field length exceeds u16 max (65535 bytes)
  | EncodeMessageTooLarge  -- ^ Total message size exceeds 65535 bytes
  deriving stock (Eq, Show, Generic)

instance NFData EncodeError

-- Message encoding ------------------------------------------------------------

-- | Encode an Init message payload.
encodeInit :: Init -> Either EncodeError BS.ByteString
encodeInit (Init gf feat tlvs) = do
  gfLen <- maybe (Left EncodeLengthOverflow) Right (encodeLength gf)
  featLen <- maybe (Left EncodeLengthOverflow) Right (encodeLength feat)
  Right $ mconcat
    [ gfLen
    , gf
    , featLen
    , feat
    , encodeTlvStream (encodeInitTlvs tlvs)
    ]

-- | Encode an Error message payload.
encodeError :: Error -> Either EncodeError BS.ByteString
encodeError (Error cid dat) = do
  datLen <- maybe (Left EncodeLengthOverflow) Right (encodeLength dat)
  Right $ mconcat [cid, datLen, dat]

-- | Encode a Warning message payload.
encodeWarning :: Warning -> Either EncodeError BS.ByteString
encodeWarning (Warning cid dat) = do
  datLen <- maybe (Left EncodeLengthOverflow) Right (encodeLength dat)
  Right $ mconcat [cid, datLen, dat]

-- | Encode a Ping message payload.
encodePing :: Ping -> Either EncodeError BS.ByteString
encodePing (Ping numPong ignored) = do
  ignoredLen <- maybe (Left EncodeLengthOverflow) Right (encodeLength ignored)
  Right $ mconcat [encodeU16 numPong, ignoredLen, ignored]

-- | Encode a Pong message payload.
encodePong :: Pong -> Either EncodeError BS.ByteString
encodePong (Pong ignored) = do
  ignoredLen <- maybe (Left EncodeLengthOverflow) Right (encodeLength ignored)
  Right $ mconcat [ignoredLen, ignored]

-- | Encode a PeerStorage message payload.
encodePeerStorage :: PeerStorage -> Either EncodeError BS.ByteString
encodePeerStorage (PeerStorage blob) = do
  blobLen <- maybe (Left EncodeLengthOverflow) Right (encodeLength blob)
  Right $ mconcat [blobLen, blob]

-- | Encode a PeerStorageRetrieval message payload.
encodePeerStorageRetrieval
  :: PeerStorageRetrieval -> Either EncodeError BS.ByteString
encodePeerStorageRetrieval (PeerStorageRetrieval blob) = do
  blobLen <- maybe (Left EncodeLengthOverflow) Right (encodeLength blob)
  Right $ mconcat [blobLen, blob]

-- | Encode a message to its payload bytes.
encodeMessage :: Message -> Either EncodeError BS.ByteString
encodeMessage = \case
  MsgInitVal m                 -> encodeInit m
  MsgErrorVal m                -> encodeError m
  MsgWarningVal m              -> encodeWarning m
  MsgPingVal m                 -> encodePing m
  MsgPongVal m                 -> encodePong m
  MsgPeerStorageVal m          -> encodePeerStorage m
  MsgPeerStorageRetrievalVal m -> encodePeerStorageRetrieval m

-- | Encode a message as a complete envelope (type + payload + extension).
--
-- Per BOLT #1, the total message size must not exceed 65535 bytes.
encodeEnvelope :: Message -> Maybe TlvStream -> Either EncodeError BS.ByteString
encodeEnvelope msg mext = do
  payload <- encodeMessage msg
  let !typeBytes = encodeU16 (msgTypeWord (messageType msg))
      !extBytes = maybe BS.empty encodeTlvStream mext
      !result = mconcat [typeBytes, payload, extBytes]
  -- Per BOLT #1: message size must fit in 2 bytes (max 65535)
  when (BS.length result > 65535) $
    Left EncodeMessageTooLarge
  Right result

-- Decoding errors -------------------------------------------------------------

-- | Decoding errors.
data DecodeError
  = DecodeInsufficientBytes
  | DecodeInvalidLength
  | DecodeUnknownEvenType !Word16
  | DecodeUnknownOddType !Word16
  | DecodeTlvError !TlvError
  | DecodeInvalidChannelId
  | DecodeInvalidExtension !TlvError
  deriving stock (Eq, Show, Generic)

instance NFData DecodeError

-- Message decoding ------------------------------------------------------------

-- | Decode an Init message from payload bytes.
--
-- Returns the decoded message and any remaining bytes.
decodeInit :: BS.ByteString -> Either DecodeError (Init, BS.ByteString)
decodeInit !bs = do
  (gfLen, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                      (decodeU16 bs)
  unless (BS.length rest1 >= fromIntegral gfLen) $
    Left DecodeInsufficientBytes
  let !gf = BS.take (fromIntegral gfLen) rest1
      !rest2 = BS.drop (fromIntegral gfLen) rest1
  (fLen, rest3) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 rest2)
  unless (BS.length rest3 >= fromIntegral fLen) $
    Left DecodeInsufficientBytes
  let !feat = BS.take (fromIntegral fLen) rest3
      !rest4 = BS.drop (fromIntegral fLen) rest3
  -- Parse optional TLV stream (consumes all remaining bytes for init)
  tlvStream <- if BS.null rest4
    then Right (TlvStream [])
    else either (Left . DecodeTlvError) Right (decodeTlvStream rest4)
  initTlvList <- either (Left . DecodeTlvError) Right
                   (parseInitTlvs tlvStream)
  -- Init consumes all bytes (TLVs are part of init, not extensions)
  Right (Init gf feat initTlvList, BS.empty)

-- | Decode an Error message from payload bytes.
decodeError :: BS.ByteString -> Either DecodeError (Error, BS.ByteString)
decodeError !bs = do
  unless (BS.length bs >= 32) $ Left DecodeInsufficientBytes
  let !cid = BS.take 32 bs
      !rest1 = BS.drop 32 bs
  (dLen, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 rest1)
  unless (BS.length rest2 >= fromIntegral dLen) $
    Left DecodeInsufficientBytes
  let !dat = BS.take (fromIntegral dLen) rest2
      !rest3 = BS.drop (fromIntegral dLen) rest2
  Right (Error cid dat, rest3)

-- | Decode a Warning message from payload bytes.
decodeWarning :: BS.ByteString -> Either DecodeError (Warning, BS.ByteString)
decodeWarning !bs = do
  unless (BS.length bs >= 32) $ Left DecodeInsufficientBytes
  let !cid = BS.take 32 bs
      !rest1 = BS.drop 32 bs
  (dLen, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 rest1)
  unless (BS.length rest2 >= fromIntegral dLen) $
    Left DecodeInsufficientBytes
  let !dat = BS.take (fromIntegral dLen) rest2
      !rest3 = BS.drop (fromIntegral dLen) rest2
  Right (Warning cid dat, rest3)

-- | Decode a Ping message from payload bytes.
decodePing :: BS.ByteString -> Either DecodeError (Ping, BS.ByteString)
decodePing !bs = do
  (numPong, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                        (decodeU16 bs)
  (bLen, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 rest1)
  unless (BS.length rest2 >= fromIntegral bLen) $
    Left DecodeInsufficientBytes
  let !ignored = BS.take (fromIntegral bLen) rest2
      !rest3 = BS.drop (fromIntegral bLen) rest2
  Right (Ping numPong ignored, rest3)

-- | Decode a Pong message from payload bytes.
decodePong :: BS.ByteString -> Either DecodeError (Pong, BS.ByteString)
decodePong !bs = do
  (bLen, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 bs)
  unless (BS.length rest1 >= fromIntegral bLen) $
    Left DecodeInsufficientBytes
  let !ignored = BS.take (fromIntegral bLen) rest1
      !rest2 = BS.drop (fromIntegral bLen) rest1
  Right (Pong ignored, rest2)

-- | Decode a PeerStorage message from payload bytes.
decodePeerStorage
  :: BS.ByteString -> Either DecodeError (PeerStorage, BS.ByteString)
decodePeerStorage !bs = do
  (bLen, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 bs)
  unless (BS.length rest1 >= fromIntegral bLen) $
    Left DecodeInsufficientBytes
  let !blob = BS.take (fromIntegral bLen) rest1
      !rest2 = BS.drop (fromIntegral bLen) rest1
  Right (PeerStorage blob, rest2)

-- | Decode a PeerStorageRetrieval message from payload bytes.
decodePeerStorageRetrieval
  :: BS.ByteString
  -> Either DecodeError (PeerStorageRetrieval, BS.ByteString)
decodePeerStorageRetrieval !bs = do
  (bLen, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 bs)
  unless (BS.length rest1 >= fromIntegral bLen) $
    Left DecodeInsufficientBytes
  let !blob = BS.take (fromIntegral bLen) rest1
      !rest2 = BS.drop (fromIntegral bLen) rest1
  Right (PeerStorageRetrieval blob, rest2)

-- | Decode a message from its type and payload.
--
-- Returns the decoded message and any remaining bytes (for extensions).
-- For unknown types, returns an appropriate error.
decodeMessage
  :: MsgType -> BS.ByteString -> Either DecodeError (Message, BS.ByteString)
decodeMessage MsgInit bs = do
  (m, rest) <- decodeInit bs
  Right (MsgInitVal m, rest)
decodeMessage MsgError bs = do
  (m, rest) <- decodeError bs
  Right (MsgErrorVal m, rest)
decodeMessage MsgWarning bs = do
  (m, rest) <- decodeWarning bs
  Right (MsgWarningVal m, rest)
decodeMessage MsgPing bs = do
  (m, rest) <- decodePing bs
  Right (MsgPingVal m, rest)
decodeMessage MsgPong bs = do
  (m, rest) <- decodePong bs
  Right (MsgPongVal m, rest)
decodeMessage MsgPeerStorage bs = do
  (m, rest) <- decodePeerStorage bs
  Right (MsgPeerStorageVal m, rest)
decodeMessage MsgPeerStorageRet bs = do
  (m, rest) <- decodePeerStorageRetrieval bs
  Right (MsgPeerStorageRetrievalVal m, rest)
decodeMessage (MsgUnknown w) _
  | even w    = Left (DecodeUnknownEvenType w)
  | otherwise = Left (DecodeUnknownOddType w)

-- | Decode a complete envelope (type + payload + optional extension).
--
-- Per BOLT #1:
-- - Unknown odd message types are ignored (returns Nothing for message)
-- - Unknown even message types cause connection close (returns error)
-- - Invalid extension TLV causes connection close (returns error)
--
-- Returns the decoded message (if known) and any extension TLVs.
decodeEnvelope
  :: BS.ByteString
  -> Either DecodeError (Maybe Message, Maybe TlvStream)
decodeEnvelope !bs = do
  (typeWord, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                         (decodeU16 bs)
  let !msgType = parseMsgType typeWord
  case msgType of
    MsgUnknown w
      | even w    -> Left (DecodeUnknownEvenType w)
      | otherwise -> Right (Nothing, Nothing)  -- Ignore unknown odd types
    _ -> do
      (msg, rest2) <- decodeMessage msgType rest1
      -- Parse any remaining bytes as extension TLV
      -- Per BOLT #1: unknown even types must fail, unknown odd are ignored
      ext <- if BS.null rest2
        then Right Nothing
        else case decodeTlvStreamWith (const False) rest2 of
          Left e  -> Left (DecodeInvalidExtension e)
          Right s -> Right (Just s)
      Right (Just msg, ext)
