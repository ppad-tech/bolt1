{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Lightning.Protocol.BOLT1
import Lightning.Protocol.BOLT1.Codec
import Lightning.Protocol.BOLT1.TLV (encodeInitTlvs, encodeTlvRecord, parseInitTlvs)

-- Fixtures --------------------------------------------------------------------

-- Prevent constant folding by marking fixtures as NOINLINE.

{-# NOINLINE u16Val #-}
u16Val :: Word16
u16Val = 0x1234

{-# NOINLINE u32Val #-}
u32Val :: Word32
u32Val = 0x12345678

{-# NOINLINE u64Val #-}
u64Val :: Word64
u64Val = 0x123456789ABCDEF0

{-# NOINLINE s8Val #-}
s8Val :: Int8
s8Val = -42

{-# NOINLINE s16Val #-}
s16Val :: Int16
s16Val = -1234

{-# NOINLINE s32Val #-}
s32Val :: Int32
s32Val = -12345678

{-# NOINLINE s64Val #-}
s64Val :: Int64
s64Val = -123456789012345

-- Truncated values

{-# NOINLINE tu16Zero #-}
tu16Zero :: Word16
tu16Zero = 0

{-# NOINLINE tu16Small #-}
tu16Small :: Word16
tu16Small = 0x42

{-# NOINLINE tu16Max #-}
tu16Max :: Word16
tu16Max = 0xFFFF

{-# NOINLINE tu32Zero #-}
tu32Zero :: Word32
tu32Zero = 0

{-# NOINLINE tu32Small #-}
tu32Small :: Word32
tu32Small = 0x42

{-# NOINLINE tu32Max #-}
tu32Max :: Word32
tu32Max = 0xFFFFFFFF

{-# NOINLINE tu64Zero #-}
tu64Zero :: Word64
tu64Zero = 0

{-# NOINLINE tu64Small #-}
tu64Small :: Word64
tu64Small = 0x42

{-# NOINLINE tu64Max #-}
tu64Max :: Word64
tu64Max = 0xFFFFFFFFFFFFFFFF

-- MinSigned values

{-# NOINLINE ms0 #-}
ms0 :: Int64
ms0 = 0

{-# NOINLINE ms127 #-}
ms127 :: Int64
ms127 = 127

{-# NOINLINE ms128 #-}
ms128 :: Int64
ms128 = 128

{-# NOINLINE msNeg128 #-}
msNeg128 :: Int64
msNeg128 = -128

{-# NOINLINE msNeg129 #-}
msNeg129 :: Int64
msNeg129 = -129

-- BigSize values

{-# NOINLINE bs0 #-}
bs0 :: Word64
bs0 = 0

{-# NOINLINE bs252 #-}
bs252 :: Word64
bs252 = 252

{-# NOINLINE bs253 #-}
bs253 :: Word64
bs253 = 253

{-# NOINLINE bs65535 #-}
bs65535 :: Word64
bs65535 = 65535

{-# NOINLINE bs65536 #-}
bs65536 :: Word64
bs65536 = 65536

{-# NOINLINE bsLarge #-}
bsLarge :: Word64
bsLarge = 0x100000000

-- Encoded bytes for decode benchmarks

{-# NOINLINE encodedU16 #-}
encodedU16 :: BS.ByteString
encodedU16 = encodeU16 u16Val

{-# NOINLINE encodedU32 #-}
encodedU32 :: BS.ByteString
encodedU32 = encodeU32 u32Val

{-# NOINLINE encodedU64 #-}
encodedU64 :: BS.ByteString
encodedU64 = encodeU64 u64Val

{-# NOINLINE encodedS8 #-}
encodedS8 :: BS.ByteString
encodedS8 = encodeS8 s8Val

{-# NOINLINE encodedS16 #-}
encodedS16 :: BS.ByteString
encodedS16 = encodeS16 s16Val

{-# NOINLINE encodedS32 #-}
encodedS32 :: BS.ByteString
encodedS32 = encodeS32 s32Val

{-# NOINLINE encodedS64 #-}
encodedS64 :: BS.ByteString
encodedS64 = encodeS64 s64Val

{-# NOINLINE encodedTu16Small #-}
encodedTu16Small :: BS.ByteString
encodedTu16Small = encodeTu16 tu16Small

{-# NOINLINE encodedTu32Small #-}
encodedTu32Small :: BS.ByteString
encodedTu32Small = encodeTu32 tu32Small

{-# NOINLINE encodedTu64Small #-}
encodedTu64Small :: BS.ByteString
encodedTu64Small = encodeTu64 tu64Small

{-# NOINLINE encodedMs127 #-}
encodedMs127 :: BS.ByteString
encodedMs127 = encodeMinSigned ms127

{-# NOINLINE encodedMsNeg129 #-}
encodedMsNeg129 :: BS.ByteString
encodedMsNeg129 = encodeMinSigned msNeg129

{-# NOINLINE encodedBs0 #-}
encodedBs0 :: BS.ByteString
encodedBs0 = encodeBigSize bs0

{-# NOINLINE encodedBs253 #-}
encodedBs253 :: BS.ByteString
encodedBs253 = encodeBigSize bs253

{-# NOINLINE encodedBs65536 #-}
encodedBs65536 :: BS.ByteString
encodedBs65536 = encodeBigSize bs65536

{-# NOINLINE encodedBsLarge #-}
encodedBsLarge :: BS.ByteString
encodedBsLarge = encodeBigSize bsLarge

-- TLV fixtures

{-# NOINLINE tlvRec1 #-}
tlvRec1 :: TlvRecord
tlvRec1 = TlvRecord 1 "test"

{-# NOINLINE tlvRec3 #-}
tlvRec3 :: TlvRecord
tlvRec3 = TlvRecord 3 "addr"

{-# NOINLINE tlvRec5 #-}
tlvRec5 :: TlvRecord
tlvRec5 = TlvRecord 5 "value"

{-# NOINLINE tlvStream1 #-}
tlvStream1 :: TlvStream
tlvStream1 = unsafeTlvStream [tlvRec1]

{-# NOINLINE tlvStream5 #-}
tlvStream5 :: TlvStream
tlvStream5 = unsafeTlvStream
  [ TlvRecord 1 "one"
  , TlvRecord 3 "three"
  , TlvRecord 5 "five"
  , TlvRecord 7 "seven"
  , TlvRecord 9 "nine"
  ]

{-# NOINLINE tlvStream20 #-}
tlvStream20 :: TlvStream
tlvStream20 = unsafeTlvStream
  [ TlvRecord (2*i + 1) (BS.replicate 10 (fromIntegral i))
  | i <- [0..19]
  ]

{-# NOINLINE encodedTlvStream1 #-}
encodedTlvStream1 :: BS.ByteString
encodedTlvStream1 = encodeTlvStream tlvStream1

{-# NOINLINE encodedTlvStream5 #-}
encodedTlvStream5 :: BS.ByteString
encodedTlvStream5 = encodeTlvStream tlvStream5

{-# NOINLINE encodedTlvStream20 #-}
encodedTlvStream20 :: BS.ByteString
encodedTlvStream20 = encodeTlvStream tlvStream20

-- Init TLV fixtures

{-# NOINLINE chainHash1 #-}
chainHash1 :: ChainHash
chainHash1 = case chainHash (BS.replicate 32 0x01) of
  Just ch -> ch
  Nothing -> error "impossible"

{-# NOINLINE initTlvNetworks #-}
initTlvNetworks :: [InitTlv]
initTlvNetworks = [InitNetworks [chainHash1]]

{-# NOINLINE initTlvRemoteAddr #-}
initTlvRemoteAddr :: [InitTlv]
initTlvRemoteAddr = [InitRemoteAddr "127.0.0.1"]

{-# NOINLINE encodedInitTlvs #-}
encodedInitTlvs :: BS.ByteString
encodedInitTlvs = encodeTlvStream (encodeInitTlvs initTlvNetworks)

-- Message fixtures

{-# NOINLINE initMinimal #-}
initMinimal :: Init
initMinimal = Init BS.empty BS.empty []

{-# NOINLINE initWithTlvs #-}
initWithTlvs :: Init
initWithTlvs = Init (BS.pack [0x00, 0x01]) (BS.pack [0x02, 0x03]) initTlvNetworks

{-# NOINLINE errorMinimal #-}
errorMinimal :: Error
errorMinimal = Error allChannels BS.empty

{-# NOINLINE errorWithData #-}
errorWithData :: Error
errorWithData = Error allChannels "Connection reset by peer"

{-# NOINLINE warningMsg #-}
warningMsg :: Warning
warningMsg = Warning allChannels "Low disk space"

{-# NOINLINE pingMinimal #-}
pingMinimal :: Ping
pingMinimal = Ping 64 BS.empty

{-# NOINLINE pingWithPadding #-}
pingWithPadding :: Ping
pingWithPadding = Ping 64 (BS.replicate 64 0x00)

{-# NOINLINE pongMsg #-}
pongMsg :: Pong
pongMsg = Pong (BS.replicate 64 0x00)

{-# NOINLINE peerStorageMsg #-}
peerStorageMsg :: PeerStorage
peerStorageMsg = PeerStorage (BS.replicate 100 0xAB)

{-# NOINLINE peerStorageRetMsg #-}
peerStorageRetMsg :: PeerStorageRetrieval
peerStorageRetMsg = PeerStorageRetrieval (BS.replicate 100 0xCD)

-- Encoded messages for decode benchmarks

{-# NOINLINE encodedInitMinimal #-}
encodedInitMinimal :: BS.ByteString
encodedInitMinimal = case encodeInit initMinimal of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedInitWithTlvs #-}
encodedInitWithTlvs :: BS.ByteString
encodedInitWithTlvs = case encodeInit initWithTlvs of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedErrorMinimal #-}
encodedErrorMinimal :: BS.ByteString
encodedErrorMinimal = case encodeError errorMinimal of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedErrorWithData #-}
encodedErrorWithData :: BS.ByteString
encodedErrorWithData = case encodeError errorWithData of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedWarning #-}
encodedWarning :: BS.ByteString
encodedWarning = case encodeWarning warningMsg of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedPingMinimal #-}
encodedPingMinimal :: BS.ByteString
encodedPingMinimal = case encodePing pingMinimal of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedPingWithPadding #-}
encodedPingWithPadding :: BS.ByteString
encodedPingWithPadding = case encodePing pingWithPadding of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedPong #-}
encodedPong :: BS.ByteString
encodedPong = case encodePong pongMsg of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedPeerStorage #-}
encodedPeerStorage :: BS.ByteString
encodedPeerStorage = case encodePeerStorage peerStorageMsg of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedPeerStorageRet #-}
encodedPeerStorageRet :: BS.ByteString
encodedPeerStorageRet = case encodePeerStorageRetrieval peerStorageRetMsg of
  Right bs -> bs
  Left _ -> error "impossible"

-- Envelope fixtures

{-# NOINLINE msgInit #-}
msgInit :: Message
msgInit = MsgInitVal initMinimal

{-# NOINLINE msgPing #-}
msgPing :: Message
msgPing = MsgPingVal pingMinimal

{-# NOINLINE encodedEnvelopeNoExt #-}
encodedEnvelopeNoExt :: BS.ByteString
encodedEnvelopeNoExt = case encodeEnvelope msgPing Nothing of
  Right bs -> bs
  Left _ -> error "impossible"

{-# NOINLINE encodedEnvelopeWithExt #-}
encodedEnvelopeWithExt :: BS.ByteString
encodedEnvelopeWithExt = case encodeEnvelope msgPing (Just tlvStream5) of
  Right bs -> bs
  Left _ -> error "impossible"

-- Main ------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "prim/encode"
      [ bench "encodeU16" $ whnf encodeU16 u16Val
      , bench "encodeU32" $ whnf encodeU32 u32Val
      , bench "encodeU64" $ whnf encodeU64 u64Val
      , bench "encodeS8" $ whnf encodeS8 s8Val
      , bench "encodeS16" $ whnf encodeS16 s16Val
      , bench "encodeS32" $ whnf encodeS32 s32Val
      , bench "encodeS64" $ whnf encodeS64 s64Val
      , bench "encodeTu16/0" $ whnf encodeTu16 tu16Zero
      , bench "encodeTu16/small" $ whnf encodeTu16 tu16Small
      , bench "encodeTu16/max" $ whnf encodeTu16 tu16Max
      , bench "encodeTu32/0" $ whnf encodeTu32 tu32Zero
      , bench "encodeTu32/small" $ whnf encodeTu32 tu32Small
      , bench "encodeTu32/max" $ whnf encodeTu32 tu32Max
      , bench "encodeTu64/0" $ whnf encodeTu64 tu64Zero
      , bench "encodeTu64/small" $ whnf encodeTu64 tu64Small
      , bench "encodeTu64/max" $ whnf encodeTu64 tu64Max
      , bench "encodeMinSigned/0" $ whnf encodeMinSigned ms0
      , bench "encodeMinSigned/127" $ whnf encodeMinSigned ms127
      , bench "encodeMinSigned/128" $ whnf encodeMinSigned ms128
      , bench "encodeMinSigned/-128" $ whnf encodeMinSigned msNeg128
      , bench "encodeMinSigned/-129" $ whnf encodeMinSigned msNeg129
      , bench "encodeBigSize/0" $ whnf encodeBigSize bs0
      , bench "encodeBigSize/252" $ whnf encodeBigSize bs252
      , bench "encodeBigSize/253" $ whnf encodeBigSize bs253
      , bench "encodeBigSize/65535" $ whnf encodeBigSize bs65535
      , bench "encodeBigSize/65536" $ whnf encodeBigSize bs65536
      , bench "encodeBigSize/large" $ whnf encodeBigSize bsLarge
      ]

  , bgroup "prim/decode"
      [ bench "decodeU16" $ nf decodeU16 encodedU16
      , bench "decodeU32" $ nf decodeU32 encodedU32
      , bench "decodeU64" $ nf decodeU64 encodedU64
      , bench "decodeS8" $ nf decodeS8 encodedS8
      , bench "decodeS16" $ nf decodeS16 encodedS16
      , bench "decodeS32" $ nf decodeS32 encodedS32
      , bench "decodeS64" $ nf decodeS64 encodedS64
      , bench "decodeTu16" $ nf (decodeTu16 1) encodedTu16Small
      , bench "decodeTu32" $ nf (decodeTu32 1) encodedTu32Small
      , bench "decodeTu64" $ nf (decodeTu64 1) encodedTu64Small
      , bench "decodeMinSigned/1" $ nf (decodeMinSigned 1) encodedMs127
      , bench "decodeMinSigned/2" $ nf (decodeMinSigned 2) encodedMsNeg129
      , bench "decodeBigSize/0" $ nf decodeBigSize encodedBs0
      , bench "decodeBigSize/253" $ nf decodeBigSize encodedBs253
      , bench "decodeBigSize/65536" $ nf decodeBigSize encodedBs65536
      , bench "decodeBigSize/large" $ nf decodeBigSize encodedBsLarge
      ]

  , bgroup "tlv/encode"
      [ bench "encodeTlvRecord" $ whnf encodeTlvRecord tlvRec1
      , bench "encodeTlvStream/1" $ whnf encodeTlvStream tlvStream1
      , bench "encodeTlvStream/5" $ whnf encodeTlvStream tlvStream5
      , bench "encodeTlvStream/20" $ whnf encodeTlvStream tlvStream20
      , bench "encodeInitTlvs" $ nf encodeInitTlvs initTlvNetworks
      ]

  , bgroup "tlv/decode"
      [ bench "decodeTlvStreamRaw/1" $ nf decodeTlvStreamRaw encodedTlvStream1
      , bench "decodeTlvStreamRaw/5" $ nf decodeTlvStreamRaw encodedTlvStream5
      , bench "decodeTlvStreamRaw/20" $ nf decodeTlvStreamRaw encodedTlvStream20
      , bench "decodeTlvStream" $ nf decodeTlvStream encodedInitTlvs
      , bench "decodeTlvStreamWith" $
          nf (decodeTlvStreamWith (const True)) encodedTlvStream5
      , bench "parseInitTlvs" $
          nf parseInitTlvs (encodeInitTlvs initTlvNetworks)
      ]

  , bgroup "message/encode"
      [ bench "encodeInit/minimal" $ nf encodeInit initMinimal
      , bench "encodeInit/with-tlvs" $ nf encodeInit initWithTlvs
      , bench "encodeError/minimal" $ nf encodeError errorMinimal
      , bench "encodeError/with-data" $ nf encodeError errorWithData
      , bench "encodeWarning" $ nf encodeWarning warningMsg
      , bench "encodePing/minimal" $ nf encodePing pingMinimal
      , bench "encodePing/with-padding" $ nf encodePing pingWithPadding
      , bench "encodePong" $ nf encodePong pongMsg
      , bench "encodePeerStorage" $ nf encodePeerStorage peerStorageMsg
      , bench "encodePeerStorageRetrieval" $
          nf encodePeerStorageRetrieval peerStorageRetMsg
      ]

  , bgroup "message/decode"
      [ bench "decodeInit/minimal" $ nf decodeInit encodedInitMinimal
      , bench "decodeInit/with-tlvs" $ nf decodeInit encodedInitWithTlvs
      , bench "decodeError/minimal" $ nf decodeError encodedErrorMinimal
      , bench "decodeError/with-data" $ nf decodeError encodedErrorWithData
      , bench "decodeWarning" $ nf decodeWarning encodedWarning
      , bench "decodePing/minimal" $ nf decodePing encodedPingMinimal
      , bench "decodePing/with-padding" $ nf decodePing encodedPingWithPadding
      , bench "decodePong" $ nf decodePong encodedPong
      , bench "decodePeerStorage" $ nf decodePeerStorage encodedPeerStorage
      , bench "decodePeerStorageRetrieval" $
          nf decodePeerStorageRetrieval encodedPeerStorageRet
      ]

  , bgroup "envelope"
      [ bench "encodeEnvelope/no-ext" $ nf (encodeEnvelope msgPing) Nothing
      , bench "encodeEnvelope/with-ext" $
          nf (encodeEnvelope msgPing) (Just tlvStream5)
      , bench "decodeEnvelope/no-ext" $ nf decodeEnvelope encodedEnvelopeNoExt
      , bench "decodeEnvelope/with-ext" $
          nf decodeEnvelope encodedEnvelopeWithExt
      , bench "decodeEnvelopeWith" $
          nf (decodeEnvelopeWith (const True)) encodedEnvelopeWithExt
      ]

  , bgroup "roundtrip"
      [ bench "init/minimal" $ nf (decodeInit . forceRight . encodeInit)
          initMinimal
      , bench "init/with-tlvs" $ nf (decodeInit . forceRight . encodeInit)
          initWithTlvs
      , bench "error" $ nf (decodeError . forceRight . encodeError) errorWithData
      , bench "warning" $ nf (decodeWarning . forceRight . encodeWarning)
          warningMsg
      , bench "ping" $ nf (decodePing . forceRight . encodePing) pingWithPadding
      , bench "pong" $ nf (decodePong . forceRight . encodePong) pongMsg
      , bench "peer-storage" $
          nf (decodePeerStorage . forceRight . encodePeerStorage) peerStorageMsg
      , bench "peer-storage-retrieval" $
          nf (decodePeerStorageRetrieval . forceRight . encodePeerStorageRetrieval)
            peerStorageRetMsg
      , bench "envelope" $ nf
          (decodeEnvelope . forceRight . encodeEnvelope msgPing) (Just tlvStream5)
      ]
  ]

-- Helper for roundtrip benchmarks
forceRight :: Either a b -> b
forceRight (Right b) = b
forceRight (Left _) = error "forceRight: Left"
{-# INLINE forceRight #-}
