{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , TlvStream(..)
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

  -- * Primitive encoding
  , encodeU16
  , encodeU32
  , encodeU64
  , encodeBigSize

  -- * Primitive decoding
  , decodeU16
  , decodeU32
  , decodeU64
  , decodeBigSize
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when, unless)
import Data.Bits (unsafeShiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)

-- Primitive encoding ----------------------------------------------------------

-- | Encode a 16-bit unsigned integer (big-endian).
--
-- >>> encodeU16 0x0102
-- "\SOH\STX"
encodeU16 :: Word16 -> BS.ByteString
encodeU16 = BSL.toStrict . BSB.toLazyByteString . BSB.word16BE
{-# INLINE encodeU16 #-}

-- | Encode a 32-bit unsigned integer (big-endian).
--
-- >>> encodeU32 0x01020304
-- "\SOH\STX\ETX\EOT"
encodeU32 :: Word32 -> BS.ByteString
encodeU32 = BSL.toStrict . BSB.toLazyByteString . BSB.word32BE
{-# INLINE encodeU32 #-}

-- | Encode a 64-bit unsigned integer (big-endian).
--
-- >>> encodeU64 0x0102030405060708
-- "\SOH\STX\ETX\EOT\ENQ\ACK\a\b"
encodeU64 :: Word64 -> BS.ByteString
encodeU64 = BSL.toStrict . BSB.toLazyByteString . BSB.word64BE
{-# INLINE encodeU64 #-}

-- | Encode a BigSize value (variable-length unsigned integer).
--
-- >>> encodeBigSize 0
-- "\NUL"
-- >>> encodeBigSize 252
-- "\252"
-- >>> encodeBigSize 253
-- "\253\NUL\253"
-- >>> encodeBigSize 65536
-- "\254\NUL\SOH\NUL\NUL"
encodeBigSize :: Word64 -> BS.ByteString
encodeBigSize !x
  | x < 0xfd = BS.singleton (fromIntegral x)
  | x < 0x10000 = BS.cons 0xfd (encodeU16 (fromIntegral x))
  | x < 0x100000000 = BS.cons 0xfe (encodeU32 (fromIntegral x))
  | otherwise = BS.cons 0xff (encodeU64 x)
{-# INLINE encodeBigSize #-}

-- | Encode a length as u16, checking bounds.
--
-- Returns Nothing if the length exceeds 65535.
encodeLength :: BS.ByteString -> Maybe BS.ByteString
encodeLength !bs
  | BS.length bs > 65535 = Nothing
  | otherwise = Just (encodeU16 (fromIntegral (BS.length bs)))
{-# INLINE encodeLength #-}

-- Primitive decoding ----------------------------------------------------------

-- | Decode a 16-bit unsigned integer (big-endian).
decodeU16 :: BS.ByteString -> Maybe (Word16, BS.ByteString)
decodeU16 !bs
  | BS.length bs < 2 = Nothing
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0)
          !b1 = fromIntegral (BS.index bs 1)
          !val = (b0 `unsafeShiftL` 8) .|. b1
      in  Just (val, BS.drop 2 bs)
{-# INLINE decodeU16 #-}

-- | Decode a 32-bit unsigned integer (big-endian).
decodeU32 :: BS.ByteString -> Maybe (Word32, BS.ByteString)
decodeU32 !bs
  | BS.length bs < 4 = Nothing
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0)
          !b1 = fromIntegral (BS.index bs 1)
          !b2 = fromIntegral (BS.index bs 2)
          !b3 = fromIntegral (BS.index bs 3)
          !val = (b0 `unsafeShiftL` 24) .|. (b1 `unsafeShiftL` 16)
              .|. (b2 `unsafeShiftL` 8) .|. b3
      in  Just (val, BS.drop 4 bs)
{-# INLINE decodeU32 #-}

-- | Decode a 64-bit unsigned integer (big-endian).
decodeU64 :: BS.ByteString -> Maybe (Word64, BS.ByteString)
decodeU64 !bs
  | BS.length bs < 8 = Nothing
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0)
          !b1 = fromIntegral (BS.index bs 1)
          !b2 = fromIntegral (BS.index bs 2)
          !b3 = fromIntegral (BS.index bs 3)
          !b4 = fromIntegral (BS.index bs 4)
          !b5 = fromIntegral (BS.index bs 5)
          !b6 = fromIntegral (BS.index bs 6)
          !b7 = fromIntegral (BS.index bs 7)
          !val = (b0 `unsafeShiftL` 56) .|. (b1 `unsafeShiftL` 48)
              .|. (b2 `unsafeShiftL` 40) .|. (b3 `unsafeShiftL` 32)
              .|. (b4 `unsafeShiftL` 24) .|. (b5 `unsafeShiftL` 16)
              .|. (b6 `unsafeShiftL` 8) .|. b7
      in  Just (val, BS.drop 8 bs)
{-# INLINE decodeU64 #-}

-- | Decode a BigSize value with minimality check.
decodeBigSize :: BS.ByteString -> Maybe (Word64, BS.ByteString)
decodeBigSize !bs
  | BS.null bs = Nothing
  | otherwise = case BS.index bs 0 of
      0xff -> do
        (val, rest) <- decodeU64 (BS.drop 1 bs)
        -- Must be >= 0x100000000 for minimal encoding
        if val >= 0x100000000
          then Just (val, rest)
          else Nothing
      0xfe -> do
        (val, rest) <- decodeU32 (BS.drop 1 bs)
        -- Must be >= 0x10000 for minimal encoding
        if val >= 0x10000
          then Just (fromIntegral val, rest)
          else Nothing
      0xfd -> do
        (val, rest) <- decodeU16 (BS.drop 1 bs)
        -- Must be >= 0xfd for minimal encoding
        if val >= 0xfd
          then Just (fromIntegral val, rest)
          else Nothing
      b -> Just (fromIntegral b, BS.drop 1 bs)

-- TLV types -------------------------------------------------------------------

-- | A single TLV record.
data TlvRecord = TlvRecord
  { tlvType   :: {-# UNPACK #-} !Word64
  , tlvValue  :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData TlvRecord

-- | A TLV stream (series of TLV records).
newtype TlvStream = TlvStream { unTlvStream :: [TlvRecord] }
  deriving stock (Eq, Show, Generic)

instance NFData TlvStream

-- | Encode a TLV record.
encodeTlvRecord :: TlvRecord -> BS.ByteString
encodeTlvRecord (TlvRecord typ val) = mconcat
  [ encodeBigSize typ
  , encodeBigSize (fromIntegral (BS.length val))
  , val
  ]

-- | Encode a TLV stream.
encodeTlvStream :: TlvStream -> BS.ByteString
encodeTlvStream (TlvStream recs) = mconcat (map encodeTlvRecord recs)

-- | TLV decoding errors.
data TlvError
  = TlvNonMinimalEncoding
  | TlvNotStrictlyIncreasing
  | TlvLengthExceedsBounds
  | TlvUnknownEvenType !Word64
  | TlvInvalidKnownType !Word64
  deriving stock (Eq, Show, Generic)

instance NFData TlvError

-- | Decode a TLV stream without any known-type validation.
--
-- This decoder only enforces structural validity:
-- - Types must be strictly increasing
-- - Lengths must not exceed bounds
--
-- All records are returned regardless of type. Note: this does NOT
-- enforce the BOLT #1 unknown-even-type rule. Use 'decodeTlvStreamWith'
-- with an appropriate predicate for spec-compliant parsing.
decodeTlvStreamRaw :: BS.ByteString -> Either TlvError TlvStream
decodeTlvStreamRaw = go Nothing []
  where
    go :: Maybe Word64 -> [TlvRecord] -> BS.ByteString
       -> Either TlvError TlvStream
    go !_ !acc !bs
      | BS.null bs = Right (TlvStream (reverse acc))
    go !mPrevType !acc !bs = do
      (typ, rest1) <- maybe (Left TlvNonMinimalEncoding) Right
                        (decodeBigSize bs)
      -- Strictly increasing check
      case mPrevType of
        Just prevType -> when (typ <= prevType) $
          Left TlvNotStrictlyIncreasing
        Nothing -> pure ()
      (len, rest2) <- maybe (Left TlvNonMinimalEncoding) Right
                        (decodeBigSize rest1)
      -- Length bounds check
      when (fromIntegral len > BS.length rest2) $
        Left TlvLengthExceedsBounds
      let !val = BS.take (fromIntegral len) rest2
          !rest3 = BS.drop (fromIntegral len) rest2
          !rec = TlvRecord typ val
      go (Just typ) (rec : acc) rest3

-- | Decode a TLV stream with configurable known-type predicate.
--
-- Per BOLT #1:
-- - Types must be strictly increasing
-- - Unknown even types cause failure
-- - Unknown odd types are skipped
--
-- The predicate determines which types are "known" for the context.
decodeTlvStreamWith
  :: (Word64 -> Bool)  -- ^ Predicate: is this type known?
  -> BS.ByteString
  -> Either TlvError TlvStream
decodeTlvStreamWith isKnown = go Nothing []
  where
    go :: Maybe Word64 -> [TlvRecord] -> BS.ByteString
       -> Either TlvError TlvStream
    go !_ !acc !bs
      | BS.null bs = Right (TlvStream (reverse acc))
    go !mPrevType !acc !bs = do
      (typ, rest1) <- maybe (Left TlvNonMinimalEncoding) Right
                        (decodeBigSize bs)
      -- Strictly increasing check
      case mPrevType of
        Just prevType -> when (typ <= prevType) $
          Left TlvNotStrictlyIncreasing
        Nothing -> pure ()
      (len, rest2) <- maybe (Left TlvNonMinimalEncoding) Right
                        (decodeBigSize rest1)
      -- Length bounds check
      when (fromIntegral len > BS.length rest2) $
        Left TlvLengthExceedsBounds
      let !val = BS.take (fromIntegral len) rest2
          !rest3 = BS.drop (fromIntegral len) rest2
          !rec = TlvRecord typ val
      -- Unknown type handling: even = fail, odd = skip
      if isKnown typ
        then go (Just typ) (rec : acc) rest3
        else if even typ
          then Left (TlvUnknownEvenType typ)
          else go (Just typ) acc rest3  -- skip unknown odd

-- | Decode a TLV stream with BOLT #1 init_tlvs validation.
--
-- This uses the default known types for init messages (1 and 3).
-- For other contexts, use 'decodeTlvStreamWith' with an appropriate
-- predicate.
decodeTlvStream :: BS.ByteString -> Either TlvError TlvStream
decodeTlvStream = decodeTlvStreamWith isInitTlvType
  where
    isInitTlvType :: Word64 -> Bool
    isInitTlvType 1 = True  -- networks
    isInitTlvType 3 = True  -- remote_addr
    isInitTlvType _ = False

-- Init TLV types --------------------------------------------------------------

-- | TLV records for init message.
data InitTlv
  = InitNetworks ![BS.ByteString]  -- ^ Type 1: chain hashes (32 bytes each)
  | InitRemoteAddr !BS.ByteString  -- ^ Type 3: remote address
  deriving stock (Eq, Show, Generic)

instance NFData InitTlv

-- | Parse init TLVs from a TLV stream.
parseInitTlvs :: TlvStream -> Either TlvError [InitTlv]
parseInitTlvs (TlvStream recs) = traverse parseOne recs
  where
    parseOne (TlvRecord 1 val)
      | BS.length val `mod` 32 == 0 =
          Right (InitNetworks (chunksOf 32 val))
      | otherwise = Left (TlvInvalidKnownType 1)
    parseOne (TlvRecord 3 val) = Right (InitRemoteAddr val)
    parseOne (TlvRecord t _) = Left (TlvUnknownEvenType t)

-- | Split bytestring into chunks of given size.
chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf !n !bs
  | BS.null bs = []
  | otherwise =
      let (!chunk, !rest) = BS.splitAt n bs
      in  chunk : chunksOf n rest

-- | Encode init TLVs to a TLV stream.
encodeInitTlvs :: [InitTlv] -> TlvStream
encodeInitTlvs = TlvStream . map toRecord
  where
    toRecord (InitNetworks chains) =
      TlvRecord 1 (mconcat chains)
    toRecord (InitRemoteAddr addr) =
      TlvRecord 3 addr

-- Message types ---------------------------------------------------------------

-- | BOLT #1 message type codes.
data MsgType
  = MsgInit              -- ^ 16
  | MsgError             -- ^ 17
  | MsgPing              -- ^ 18
  | MsgPong              -- ^ 19
  | MsgWarning           -- ^ 1
  | MsgPeerStorage       -- ^ 7
  | MsgPeerStorageRet    -- ^ 9
  | MsgUnknown !Word16   -- ^ Unknown type
  deriving stock (Eq, Show, Generic)

instance NFData MsgType

-- | Get the numeric type code for a message type.
msgTypeWord :: MsgType -> Word16
msgTypeWord MsgInit            = 16
msgTypeWord MsgError           = 17
msgTypeWord MsgPing            = 18
msgTypeWord MsgPong            = 19
msgTypeWord MsgWarning         = 1
msgTypeWord MsgPeerStorage     = 7
msgTypeWord MsgPeerStorageRet  = 9
msgTypeWord (MsgUnknown w)     = w

-- | Parse a message type from a word.
parseMsgType :: Word16 -> MsgType
parseMsgType 16 = MsgInit
parseMsgType 17 = MsgError
parseMsgType 18 = MsgPing
parseMsgType 19 = MsgPong
parseMsgType 1  = MsgWarning
parseMsgType 7  = MsgPeerStorage
parseMsgType 9  = MsgPeerStorageRet
parseMsgType w  = MsgUnknown w

-- Message ADTs ----------------------------------------------------------------

-- | The init message (type 16).
data Init = Init
  { initGlobalFeatures :: !BS.ByteString
  , initFeatures       :: !BS.ByteString
  , initTlvs           :: ![InitTlv]
  } deriving stock (Eq, Show, Generic)

instance NFData Init

-- | The error message (type 17).
data Error = Error
  { errorChannelId :: !BS.ByteString  -- ^ 32 bytes
  , errorData      :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Error

-- | The warning message (type 1).
data Warning = Warning
  { warningChannelId :: !BS.ByteString  -- ^ 32 bytes
  , warningData      :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Warning

-- | The ping message (type 18).
data Ping = Ping
  { pingNumPongBytes :: {-# UNPACK #-} !Word16
  , pingIgnored      :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Ping

-- | The pong message (type 19).
data Pong = Pong
  { pongIgnored :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Pong

-- | The peer_storage message (type 7).
data PeerStorage = PeerStorage
  { peerStorageBlob :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData PeerStorage

-- | The peer_storage_retrieval message (type 9).
data PeerStorageRetrieval = PeerStorageRetrieval
  { peerStorageRetrievalBlob :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData PeerStorageRetrieval

-- | All BOLT #1 messages.
data Message
  = MsgInitVal !Init
  | MsgErrorVal !Error
  | MsgWarningVal !Warning
  | MsgPingVal !Ping
  | MsgPongVal !Pong
  | MsgPeerStorageVal !PeerStorage
  | MsgPeerStorageRetrievalVal !PeerStorageRetrieval
  deriving stock (Eq, Show, Generic)

instance NFData Message

-- Message envelope ------------------------------------------------------------

-- | A complete message envelope with type, payload, and optional extension.
data Envelope = Envelope
  { envType      :: !MsgType
  , envPayload   :: !BS.ByteString
  , envExtension :: !(Maybe TlvStream)
  } deriving stock (Eq, Show, Generic)

instance NFData Envelope

-- Message encoding ------------------------------------------------------------

-- | Encoding errors.
data EncodeError
  = EncodeLengthOverflow  -- ^ Payload exceeds u16 max (65535 bytes)
  deriving stock (Eq, Show, Generic)

instance NFData EncodeError

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

-- | Get the message type for a message.
messageType :: Message -> MsgType
messageType = \case
  MsgInitVal _                 -> MsgInit
  MsgErrorVal _                -> MsgError
  MsgWarningVal _              -> MsgWarning
  MsgPingVal _                 -> MsgPing
  MsgPongVal _                 -> MsgPong
  MsgPeerStorageVal _          -> MsgPeerStorage
  MsgPeerStorageRetrievalVal _ -> MsgPeerStorageRet

-- | Encode a message as a complete envelope (type + payload + extension).
encodeEnvelope :: Message -> Maybe TlvStream -> Either EncodeError BS.ByteString
encodeEnvelope msg mext = do
  payload <- encodeMessage msg
  Right $ mconcat $
    [ encodeU16 (msgTypeWord (messageType msg))
    , payload
    ] ++ maybe [] (\ext -> [encodeTlvStream ext]) mext

-- Message decoding ------------------------------------------------------------

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
