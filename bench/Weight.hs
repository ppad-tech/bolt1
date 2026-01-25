{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Lightning.Protocol.BOLT1
import Lightning.Protocol.BOLT1.Codec
import Lightning.Protocol.BOLT1.TLV (encodeTlvRecord)
import Weigh

-- Fixtures --------------------------------------------------------------------

-- Prevent constant folding with NOINLINE

{-# NOINLINE w16Val #-}
w16Val :: Word16
w16Val = 0x1234

{-# NOINLINE w32Val #-}
w32Val :: Word32
w32Val = 0x12345678

{-# NOINLINE w64Val #-}
w64Val :: Word64
w64Val = 0x0102030405060708

{-# NOINLINE s8Val #-}
s8Val :: Int8
s8Val = -42

{-# NOINLINE s16Val #-}
s16Val :: Int16
s16Val = -1000

{-# NOINLINE s32Val #-}
s32Val :: Int32
s32Val = -100000

{-# NOINLINE s64Val #-}
s64Val :: Int64
s64Val = -10000000000

{-# NOINLINE tu16Small #-}
tu16Small :: Word16
tu16Small = 0x7f

{-# NOINLINE tu16Full #-}
tu16Full :: Word16
tu16Full = 0xffff

{-# NOINLINE tu32Small #-}
tu32Small :: Word32
tu32Small = 0x42

{-# NOINLINE tu32Full #-}
tu32Full :: Word32
tu32Full = 0xffffffff

{-# NOINLINE tu64Small #-}
tu64Small :: Word64
tu64Small = 0x10

{-# NOINLINE tu64Full #-}
tu64Full :: Word64
tu64Full = 0xffffffffffffffff

{-# NOINLINE bigSizeSmall #-}
bigSizeSmall :: Word64
bigSizeSmall = 0xfc

{-# NOINLINE bigSizeMedium #-}
bigSizeMedium :: Word64
bigSizeMedium = 0x1000

{-# NOINLINE bigSizeLarge #-}
bigSizeLarge :: Word64
bigSizeLarge = 0x100000000

-- Pre-encoded bytes for decoder benchmarks

{-# NOINLINE u16Bytes #-}
u16Bytes :: BS.ByteString
u16Bytes = encodeU16 w16Val

{-# NOINLINE u32Bytes #-}
u32Bytes :: BS.ByteString
u32Bytes = encodeU32 w32Val

{-# NOINLINE u64Bytes #-}
u64Bytes :: BS.ByteString
u64Bytes = encodeU64 w64Val

{-# NOINLINE s8Bytes #-}
s8Bytes :: BS.ByteString
s8Bytes = encodeS8 s8Val

{-# NOINLINE s16Bytes #-}
s16Bytes :: BS.ByteString
s16Bytes = encodeS16 s16Val

{-# NOINLINE s32Bytes #-}
s32Bytes :: BS.ByteString
s32Bytes = encodeS32 s32Val

{-# NOINLINE s64Bytes #-}
s64Bytes :: BS.ByteString
s64Bytes = encodeS64 s64Val

{-# NOINLINE bigSizeBytes #-}
bigSizeBytes :: BS.ByteString
bigSizeBytes = encodeBigSize bigSizeLarge

-- TLV fixtures

{-# NOINLINE tlvRecord1 #-}
tlvRecord1 :: TlvRecord
tlvRecord1 = TlvRecord 1 "test-value"

{-# NOINLINE tlvStream1 #-}
tlvStream1 :: TlvStream
tlvStream1 = unsafeTlvStream [tlvRecord1]

{-# NOINLINE tlvStream5 #-}
tlvStream5 :: TlvStream
tlvStream5 = unsafeTlvStream
  [ TlvRecord 1 "value1"
  , TlvRecord 3 "value3"
  , TlvRecord 5 "value5"
  , TlvRecord 7 "value7"
  , TlvRecord 9 "value9"
  ]

{-# NOINLINE tlvStream20 #-}
tlvStream20 :: TlvStream
tlvStream20 = unsafeTlvStream
  [ TlvRecord (2 * i + 1) (BS.replicate 10 (fromIntegral i))
  | i <- [0..19]
  ]

{-# NOINLINE tlvStreamBytes1 #-}
tlvStreamBytes1 :: BS.ByteString
tlvStreamBytes1 = encodeTlvStream tlvStream1

{-# NOINLINE tlvStreamBytes5 #-}
tlvStreamBytes5 :: BS.ByteString
tlvStreamBytes5 = encodeTlvStream tlvStream5

{-# NOINLINE tlvStreamBytes20 #-}
tlvStreamBytes20 :: BS.ByteString
tlvStreamBytes20 = encodeTlvStream tlvStream20

-- Message fixtures

{-# NOINLINE minimalInit #-}
minimalInit :: Init
minimalInit = Init BS.empty BS.empty []

{-# NOINLINE initWithFeatures #-}
initWithFeatures :: Init
initWithFeatures = Init "\x00\x08" "\x00\x0a\x8a" []

{-# NOINLINE initWithTlvs #-}
initWithTlvs :: Init
initWithTlvs = Init BS.empty "\x00\x01" [InitRemoteAddr "127.0.0.1"]

{-# NOINLINE errorMsg #-}
errorMsg :: Error
errorMsg = Error allChannels "something bad happened"

{-# NOINLINE warningMsg #-}
warningMsg :: Warning
warningMsg = Warning allChannels "something concerning"

{-# NOINLINE pingMinimal #-}
pingMinimal :: Ping
pingMinimal = Ping 4 BS.empty

{-# NOINLINE pingWithPadding #-}
pingWithPadding :: Ping
pingWithPadding = Ping 4 (BS.replicate 64 0x00)

{-# NOINLINE pongMsg #-}
pongMsg :: Pong
pongMsg = Pong (BS.replicate 4 0x00)

{-# NOINLINE peerStorageMsg #-}
peerStorageMsg :: PeerStorage
peerStorageMsg = PeerStorage (BS.replicate 100 0xab)

{-# NOINLINE peerStorageRetrievalMsg #-}
peerStorageRetrievalMsg :: PeerStorageRetrieval
peerStorageRetrievalMsg = PeerStorageRetrieval (BS.replicate 50 0xcd)

-- Pre-encoded message bytes for decoder benchmarks

{-# NOINLINE initMinimalBytes #-}
initMinimalBytes :: BS.ByteString
initMinimalBytes = either (const BS.empty) id (encodeInit minimalInit)

{-# NOINLINE initWithTlvsBytes #-}
initWithTlvsBytes :: BS.ByteString
initWithTlvsBytes = either (const BS.empty) id (encodeInit initWithTlvs)

{-# NOINLINE errorBytes #-}
errorBytes :: BS.ByteString
errorBytes = either (const BS.empty) id (encodeError errorMsg)

{-# NOINLINE warningBytes #-}
warningBytes :: BS.ByteString
warningBytes = either (const BS.empty) id (encodeWarning warningMsg)

{-# NOINLINE pingMinimalBytes #-}
pingMinimalBytes :: BS.ByteString
pingMinimalBytes = either (const BS.empty) id (encodePing pingMinimal)

{-# NOINLINE pingWithPaddingBytes #-}
pingWithPaddingBytes :: BS.ByteString
pingWithPaddingBytes = either (const BS.empty) id (encodePing pingWithPadding)

{-# NOINLINE pongBytes #-}
pongBytes :: BS.ByteString
pongBytes = either (const BS.empty) id (encodePong pongMsg)

{-# NOINLINE peerStorageBytes #-}
peerStorageBytes :: BS.ByteString
peerStorageBytes = either (const BS.empty) id (encodePeerStorage peerStorageMsg)

{-# NOINLINE peerStorageRetrievalBytes #-}
peerStorageRetrievalBytes :: BS.ByteString
peerStorageRetrievalBytes =
  either (const BS.empty) id (encodePeerStorageRetrieval peerStorageRetrievalMsg)

-- Envelope fixtures

{-# NOINLINE initMessage #-}
initMessage :: Message
initMessage = MsgInitVal minimalInit

{-# NOINLINE pingMessage #-}
pingMessage :: Message
pingMessage = MsgPingVal pingMinimal

{-# NOINLINE envelopeBytes #-}
envelopeBytes :: BS.ByteString
envelopeBytes = either (const BS.empty) id (encodeEnvelope initMessage Nothing)

-- Main ------------------------------------------------------------------------

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]

  -- Primitive encoders --------------------------------------------------------

  wgroup "Primitive Encoders" $ do
    func "encodeU16" encodeU16 w16Val
    func "encodeU32" encodeU32 w32Val
    func "encodeU64" encodeU64 w64Val
    func "encodeS8" encodeS8 s8Val
    func "encodeS16" encodeS16 s16Val
    func "encodeS32" encodeS32 s32Val
    func "encodeS64" encodeS64 s64Val

  wgroup "Truncated Unsigned Encoders" $ do
    func "encodeTu16/small" encodeTu16 tu16Small
    func "encodeTu16/full" encodeTu16 tu16Full
    func "encodeTu32/small" encodeTu32 tu32Small
    func "encodeTu32/full" encodeTu32 tu32Full
    func "encodeTu64/small" encodeTu64 tu64Small
    func "encodeTu64/full" encodeTu64 tu64Full

  wgroup "Minimal Signed Encoder" $ do
    func "encodeMinSigned/1-byte" encodeMinSigned (0 :: Int64)
    func "encodeMinSigned/2-byte" encodeMinSigned (1000 :: Int64)
    func "encodeMinSigned/4-byte" encodeMinSigned (100000 :: Int64)
    func "encodeMinSigned/8-byte" encodeMinSigned s64Val

  wgroup "BigSize Encoder" $ do
    func "encodeBigSize/1-byte" encodeBigSize bigSizeSmall
    func "encodeBigSize/3-byte" encodeBigSize bigSizeMedium
    func "encodeBigSize/9-byte" encodeBigSize bigSizeLarge

  -- Primitive decoders --------------------------------------------------------

  wgroup "Primitive Decoders" $ do
    func "decodeU16" decodeU16 u16Bytes
    func "decodeU32" decodeU32 u32Bytes
    func "decodeU64" decodeU64 u64Bytes
    func "decodeS8" decodeS8 s8Bytes
    func "decodeS16" decodeS16 s16Bytes
    func "decodeS32" decodeS32 s32Bytes
    func "decodeS64" decodeS64 s64Bytes
    func "decodeBigSize" decodeBigSize bigSizeBytes

  -- TLV operations ------------------------------------------------------------

  wgroup "TLV Encoding" $ do
    func "encodeTlvRecord" encodeTlvRecord tlvRecord1
    func "encodeTlvStream/1-record" encodeTlvStream tlvStream1
    func "encodeTlvStream/5-records" encodeTlvStream tlvStream5
    func "encodeTlvStream/20-records" encodeTlvStream tlvStream20

  wgroup "TLV Decoding" $ do
    func "decodeTlvStreamRaw/1-record" decodeTlvStreamRaw tlvStreamBytes1
    func "decodeTlvStreamRaw/5-records" decodeTlvStreamRaw tlvStreamBytes5
    func "decodeTlvStreamRaw/20-records" decodeTlvStreamRaw tlvStreamBytes20
    func "decodeTlvStream/1-record" decodeTlvStream tlvStreamBytes1
    func "decodeTlvStream/5-records" decodeTlvStream tlvStreamBytes5

  -- Message encoders ----------------------------------------------------------

  wgroup "Message Encoders" $ do
    func "encodeInit/minimal" encodeInit minimalInit
    func "encodeInit/with-features" encodeInit initWithFeatures
    func "encodeInit/with-tlvs" encodeInit initWithTlvs
    func "encodeError" encodeError errorMsg
    func "encodeWarning" encodeWarning warningMsg
    func "encodePing/minimal" encodePing pingMinimal
    func "encodePing/with-padding" encodePing pingWithPadding
    func "encodePong" encodePong pongMsg
    func "encodePeerStorage" encodePeerStorage peerStorageMsg
    func "encodePeerStorageRetrieval" encodePeerStorageRetrieval
      peerStorageRetrievalMsg

  -- Message decoders ----------------------------------------------------------

  wgroup "Message Decoders" $ do
    func "decodeInit/minimal" decodeInit initMinimalBytes
    func "decodeInit/with-tlvs" decodeInit initWithTlvsBytes
    func "decodeError" decodeError errorBytes
    func "decodeWarning" decodeWarning warningBytes
    func "decodePing/minimal" decodePing pingMinimalBytes
    func "decodePing/with-padding" decodePing pingWithPaddingBytes
    func "decodePong" decodePong pongBytes
    func "decodePeerStorage" decodePeerStorage peerStorageBytes
    func "decodePeerStorageRetrieval" decodePeerStorageRetrieval
      peerStorageRetrievalBytes

  -- Envelope operations -------------------------------------------------------

  wgroup "Envelope Operations" $ do
    func "encodeEnvelope/init" (flip encodeEnvelope Nothing) initMessage
    func "encodeEnvelope/ping" (flip encodeEnvelope Nothing) pingMessage
    func "decodeEnvelope" decodeEnvelope envelopeBytes
