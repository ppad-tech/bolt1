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

  -- ** Init TLVs
  , InitTlv(..)

  -- * Message envelope
  , Envelope(..)

  -- * Encoding
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

-- | Decode a TLV stream with BOLT #1 validation.
--
-- - Types must be strictly increasing
-- - Unknown even types cause failure
-- - Unknown odd types are skipped
decodeTlvStream :: BS.ByteString -> Either TlvError TlvStream
decodeTlvStream = go Nothing []
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
      if isKnownTlvType typ
        then go (Just typ) (rec : acc) rest3
        else if even typ
          then Left (TlvUnknownEvenType typ)
          else go (Just typ) acc rest3  -- skip unknown odd

-- | Check if a TLV type is known (for init_tlvs).
-- Types 1 (networks) and 3 (remote_addr) are known.
isKnownTlvType :: Word64 -> Bool
isKnownTlvType 1 = True  -- networks
isKnownTlvType 3 = True  -- remote_addr
isKnownTlvType _ = False

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

-- | Encode an Init message payload.
encodeInit :: Init -> BS.ByteString
encodeInit (Init gf feat tlvs) = mconcat
  [ encodeU16 (fromIntegral (BS.length gf))
  , gf
  , encodeU16 (fromIntegral (BS.length feat))
  , feat
  , encodeTlvStream (encodeInitTlvs tlvs)
  ]

-- | Encode an Error message payload.
encodeError :: Error -> BS.ByteString
encodeError (Error cid dat) = mconcat
  [ cid  -- 32 bytes
  , encodeU16 (fromIntegral (BS.length dat))
  , dat
  ]

-- | Encode a Warning message payload.
encodeWarning :: Warning -> BS.ByteString
encodeWarning (Warning cid dat) = mconcat
  [ cid  -- 32 bytes
  , encodeU16 (fromIntegral (BS.length dat))
  , dat
  ]

-- | Encode a Ping message payload.
encodePing :: Ping -> BS.ByteString
encodePing (Ping numPong ignored) = mconcat
  [ encodeU16 numPong
  , encodeU16 (fromIntegral (BS.length ignored))
  , ignored
  ]

-- | Encode a Pong message payload.
encodePong :: Pong -> BS.ByteString
encodePong (Pong ignored) = mconcat
  [ encodeU16 (fromIntegral (BS.length ignored))
  , ignored
  ]

-- | Encode a PeerStorage message payload.
encodePeerStorage :: PeerStorage -> BS.ByteString
encodePeerStorage (PeerStorage blob) = mconcat
  [ encodeU16 (fromIntegral (BS.length blob))
  , blob
  ]

-- | Encode a PeerStorageRetrieval message payload.
encodePeerStorageRetrieval :: PeerStorageRetrieval -> BS.ByteString
encodePeerStorageRetrieval (PeerStorageRetrieval blob) = mconcat
  [ encodeU16 (fromIntegral (BS.length blob))
  , blob
  ]

-- | Encode a message to its payload bytes.
encodeMessage :: Message -> BS.ByteString
encodeMessage = \case
  MsgInitVal m               -> encodeInit m
  MsgErrorVal m              -> encodeError m
  MsgWarningVal m            -> encodeWarning m
  MsgPingVal m               -> encodePing m
  MsgPongVal m               -> encodePong m
  MsgPeerStorageVal m        -> encodePeerStorage m
  MsgPeerStorageRetrievalVal m -> encodePeerStorageRetrieval m

-- | Get the message type for a message.
messageType :: Message -> MsgType
messageType = \case
  MsgInitVal _               -> MsgInit
  MsgErrorVal _              -> MsgError
  MsgWarningVal _            -> MsgWarning
  MsgPingVal _               -> MsgPing
  MsgPongVal _               -> MsgPong
  MsgPeerStorageVal _        -> MsgPeerStorage
  MsgPeerStorageRetrievalVal _ -> MsgPeerStorageRet

-- | Encode a message as a complete envelope (type + payload).
encodeEnvelope :: Message -> Maybe TlvStream -> BS.ByteString
encodeEnvelope msg mext = mconcat $
  [ encodeU16 (msgTypeWord (messageType msg))
  , encodeMessage msg
  ] ++ maybe [] (\ext -> [encodeTlvStream ext]) mext

-- Message decoding ------------------------------------------------------------

-- | Decoding errors.
data DecodeError
  = DecodeInsufficientBytes
  | DecodeInvalidLength
  | DecodeUnknownEvenType !Word16
  | DecodeTlvError !TlvError
  | DecodeInvalidChannelId
  deriving stock (Eq, Show, Generic)

instance NFData DecodeError

-- | Decode an Init message from payload bytes.
decodeInit :: BS.ByteString -> Either DecodeError Init
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
  -- Parse optional TLV stream
  tlvStream <- if BS.null rest4
    then Right (TlvStream [])
    else either (Left . DecodeTlvError) Right (decodeTlvStream rest4)
  initTlvList <- either (Left . DecodeTlvError) Right
                   (parseInitTlvs tlvStream)
  Right (Init gf feat initTlvList)

-- | Decode an Error message from payload bytes.
decodeError :: BS.ByteString -> Either DecodeError Error
decodeError !bs = do
  unless (BS.length bs >= 32) $ Left DecodeInsufficientBytes
  let !cid = BS.take 32 bs
      !rest1 = BS.drop 32 bs
  (dLen, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 rest1)
  unless (BS.length rest2 >= fromIntegral dLen) $
    Left DecodeInsufficientBytes
  let !dat = BS.take (fromIntegral dLen) rest2
  Right (Error cid dat)

-- | Decode a Warning message from payload bytes.
decodeWarning :: BS.ByteString -> Either DecodeError Warning
decodeWarning !bs = do
  unless (BS.length bs >= 32) $ Left DecodeInsufficientBytes
  let !cid = BS.take 32 bs
      !rest1 = BS.drop 32 bs
  (dLen, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 rest1)
  unless (BS.length rest2 >= fromIntegral dLen) $
    Left DecodeInsufficientBytes
  let !dat = BS.take (fromIntegral dLen) rest2
  Right (Warning cid dat)

-- | Decode a Ping message from payload bytes.
decodePing :: BS.ByteString -> Either DecodeError Ping
decodePing !bs = do
  (numPong, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                        (decodeU16 bs)
  (bLen, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 rest1)
  unless (BS.length rest2 >= fromIntegral bLen) $
    Left DecodeInsufficientBytes
  let !ignored = BS.take (fromIntegral bLen) rest2
  Right (Ping numPong ignored)

-- | Decode a Pong message from payload bytes.
decodePong :: BS.ByteString -> Either DecodeError Pong
decodePong !bs = do
  (bLen, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 bs)
  unless (BS.length rest1 >= fromIntegral bLen) $
    Left DecodeInsufficientBytes
  let !ignored = BS.take (fromIntegral bLen) rest1
  Right (Pong ignored)

-- | Decode a PeerStorage message from payload bytes.
decodePeerStorage :: BS.ByteString -> Either DecodeError PeerStorage
decodePeerStorage !bs = do
  (bLen, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 bs)
  unless (BS.length rest1 >= fromIntegral bLen) $
    Left DecodeInsufficientBytes
  let !blob = BS.take (fromIntegral bLen) rest1
  Right (PeerStorage blob)

-- | Decode a PeerStorageRetrieval message from payload bytes.
decodePeerStorageRetrieval :: BS.ByteString
                           -> Either DecodeError PeerStorageRetrieval
decodePeerStorageRetrieval !bs = do
  (bLen, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                     (decodeU16 bs)
  unless (BS.length rest1 >= fromIntegral bLen) $
    Left DecodeInsufficientBytes
  let !blob = BS.take (fromIntegral bLen) rest1
  Right (PeerStorageRetrieval blob)

-- | Decode a message from its type and payload.
decodeMessage :: MsgType -> BS.ByteString -> Either DecodeError Message
decodeMessage MsgInit bs = MsgInitVal <$> decodeInit bs
decodeMessage MsgError bs = MsgErrorVal <$> decodeError bs
decodeMessage MsgWarning bs = MsgWarningVal <$> decodeWarning bs
decodeMessage MsgPing bs = MsgPingVal <$> decodePing bs
decodeMessage MsgPong bs = MsgPongVal <$> decodePong bs
decodeMessage MsgPeerStorage bs = MsgPeerStorageVal <$> decodePeerStorage bs
decodeMessage MsgPeerStorageRet bs =
  MsgPeerStorageRetrievalVal <$> decodePeerStorageRetrieval bs
decodeMessage (MsgUnknown w) _
  | even w    = Left (DecodeUnknownEvenType w)
  | otherwise = Left DecodeInsufficientBytes

-- | Decode a complete envelope (type + payload + optional extension).
--
-- Per BOLT #1:
-- - Unknown odd message types are ignored (returns Nothing)
-- - Unknown even message types cause connection close (returns error)
-- - Invalid extension TLV causes connection close
decodeEnvelope :: BS.ByteString -> Either DecodeError (Maybe Message)
decodeEnvelope !bs = do
  (typeWord, rest1) <- maybe (Left DecodeInsufficientBytes) Right
                         (decodeU16 bs)
  let !msgType = parseMsgType typeWord
  case msgType of
    MsgUnknown w
      | even w    -> Left (DecodeUnknownEvenType w)
      | otherwise -> Right Nothing  -- Ignore unknown odd types
    _ -> do
      msg <- decodeMessage msgType rest1
      Right (Just msg)
