{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Fixtures
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Test fixtures for BOLT #1 benchmarks.

module Fixtures where

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Lightning.Protocol.BOLT1

-- Sample ByteStrings -----------------------------------------------------

-- | 64-byte sample data.
bytes64 :: BS.ByteString
bytes64 = BS.replicate 64 0xAB
{-# NOINLINE bytes64 #-}

-- | 1KB sample data.
bytes1k :: BS.ByteString
bytes1k = BS.replicate 1024 0xCD
{-# NOINLINE bytes1k #-}

-- | 16KB sample data.
bytes16k :: BS.ByteString
bytes16k = BS.replicate 16384 0xEF
{-# NOINLINE bytes16k #-}

-- Sample chain hashes (32 bytes each) ------------------------------------

-- | Bitcoin mainnet genesis block hash (reversed, as used in LN).
mainnetChainHash :: ChainHash
mainnetChainHash = fromJust $ chainHash $ BS.pack
  [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
  , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
  , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
  , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
  ]
{-# NOINLINE mainnetChainHash #-}

-- | Bitcoin testnet genesis block hash (reversed, as used in LN).
testnetChainHash :: ChainHash
testnetChainHash = fromJust $ chainHash $ BS.pack
  [ 0x43, 0x49, 0x7f, 0xd7, 0xf8, 0x26, 0x95, 0x71
  , 0x08, 0xf4, 0xa3, 0x0f, 0xd9, 0xce, 0xc3, 0xae
  , 0xba, 0x79, 0x97, 0x20, 0x84, 0xe9, 0x0e, 0xad
  , 0x01, 0xea, 0x33, 0x09, 0x00, 0x00, 0x00, 0x00
  ]
{-# NOINLINE testnetChainHash #-}

-- Sample channel IDs (32 bytes each) -------------------------------------

-- | Sample channel ID (non-zero).
sampleChannelId :: ChannelId
sampleChannelId = fromJust $ channelId $ BS.pack
  [ 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08
  , 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10
  , 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18
  , 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20
  ]
{-# NOINLINE sampleChannelId #-}

-- Sample Init messages ---------------------------------------------------

-- | Minimal Init message (empty features, no TLVs).
minimalInit :: Init
minimalInit = Init
  { initGlobalFeatures = BS.empty
  , initFeatures       = BS.empty
  , initTlvs           = []
  }
{-# NOINLINE minimalInit #-}

-- | Init with feature bits set.
initWithFeatures :: Init
initWithFeatures = Init
  { initGlobalFeatures = BS.pack [0x00, 0x01]  -- 2 bytes
  , initFeatures       = BS.pack [0x02, 0xa2]  -- data_loss_protect, etc.
  , initTlvs           = []
  }
{-# NOINLINE initWithFeatures #-}

-- | Init with TLV extensions.
initWithTlvs :: Init
initWithTlvs = Init
  { initGlobalFeatures = BS.empty
  , initFeatures       = BS.pack [0x02, 0xa2]
  , initTlvs           = [InitNetworks [mainnetChainHash]]
  }
{-# NOINLINE initWithTlvs #-}

-- | Init with multiple chain hashes.
initWithMultipleChains :: Init
initWithMultipleChains = Init
  { initGlobalFeatures = BS.empty
  , initFeatures       = BS.pack [0x02, 0xa2]
  , initTlvs           = [InitNetworks [mainnetChainHash, testnetChainHash]]
  }
{-# NOINLINE initWithMultipleChains #-}

-- | Full Init with features and remote_addr TLV.
fullInit :: Init
fullInit = Init
  { initGlobalFeatures = BS.pack [0x00, 0x01]
  , initFeatures       = BS.pack [0x02, 0xa2, 0x01]
  , initTlvs           =
      [ InitNetworks [mainnetChainHash]
      , InitRemoteAddr (BS.pack [0x01, 0x7f, 0x00, 0x00, 0x01, 0x27, 0x10])
      ]
  }
{-# NOINLINE fullInit #-}

-- Sample Error messages --------------------------------------------------

-- | Minimal Error message (connection-level, empty data).
minimalError :: Error
minimalError = Error
  { errorChannelId = allChannels
  , errorData      = BS.empty
  }
{-# NOINLINE minimalError #-}

-- | Error with channel ID and message.
errorWithData :: Error
errorWithData = Error
  { errorChannelId = sampleChannelId
  , errorData      = "funding transaction failed"
  }
{-# NOINLINE errorWithData #-}

-- | Error with longer data.
errorWithLongData :: Error
errorWithLongData = Error
  { errorChannelId = sampleChannelId
  , errorData      = bytes1k
  }
{-# NOINLINE errorWithLongData #-}

-- Sample Warning messages ------------------------------------------------

-- | Minimal Warning message.
minimalWarning :: Warning
minimalWarning = Warning
  { warningChannelId = allChannels
  , warningData      = BS.empty
  }
{-# NOINLINE minimalWarning #-}

-- | Warning with message.
warningWithData :: Warning
warningWithData = Warning
  { warningChannelId = sampleChannelId
  , warningData      = "channel fee too low"
  }
{-# NOINLINE warningWithData #-}

-- Sample Ping messages ---------------------------------------------------

-- | Minimal Ping (no padding, no response requested).
minimalPing :: Ping
minimalPing = Ping
  { pingNumPongBytes = 0
  , pingIgnored      = BS.empty
  }
{-# NOINLINE minimalPing #-}

-- | Ping with response requested but no padding.
pingWithResponse :: Ping
pingWithResponse = Ping
  { pingNumPongBytes = 64
  , pingIgnored      = BS.empty
  }
{-# NOINLINE pingWithResponse #-}

-- | Ping with padding (64 bytes).
pingWithPadding :: Ping
pingWithPadding = Ping
  { pingNumPongBytes = 64
  , pingIgnored      = bytes64
  }
{-# NOINLINE pingWithPadding #-}

-- | Ping with large padding (1KB).
pingWithLargePadding :: Ping
pingWithLargePadding = Ping
  { pingNumPongBytes = 128
  , pingIgnored      = bytes1k
  }
{-# NOINLINE pingWithLargePadding #-}

-- Sample Pong messages ---------------------------------------------------

-- | Minimal Pong (no ignored bytes).
minimalPong :: Pong
minimalPong = Pong
  { pongIgnored = BS.empty
  }
{-# NOINLINE minimalPong #-}

-- | Pong with padding (64 bytes).
pongWithPadding :: Pong
pongWithPadding = Pong
  { pongIgnored = bytes64
  }
{-# NOINLINE pongWithPadding #-}

-- | Pong with large padding (1KB).
pongWithLargePadding :: Pong
pongWithLargePadding = Pong
  { pongIgnored = bytes1k
  }
{-# NOINLINE pongWithLargePadding #-}

-- Sample PeerStorage messages --------------------------------------------

-- | Minimal PeerStorage (empty blob).
minimalPeerStorage :: PeerStorage
minimalPeerStorage = PeerStorage
  { peerStorageBlob = BS.empty
  }
{-# NOINLINE minimalPeerStorage #-}

-- | PeerStorage with 1KB blob.
peerStorageSmall :: PeerStorage
peerStorageSmall = PeerStorage
  { peerStorageBlob = bytes1k
  }
{-# NOINLINE peerStorageSmall #-}

-- | PeerStorage with 16KB blob.
peerStorageLarge :: PeerStorage
peerStorageLarge = PeerStorage
  { peerStorageBlob = bytes16k
  }
{-# NOINLINE peerStorageLarge #-}

-- Sample PeerStorageRetrieval messages -----------------------------------

-- | Minimal PeerStorageRetrieval (empty blob).
minimalPeerStorageRetrieval :: PeerStorageRetrieval
minimalPeerStorageRetrieval = PeerStorageRetrieval
  { peerStorageRetrievalBlob = BS.empty
  }
{-# NOINLINE minimalPeerStorageRetrieval #-}

-- | PeerStorageRetrieval with 1KB blob.
peerStorageRetrievalSmall :: PeerStorageRetrieval
peerStorageRetrievalSmall = PeerStorageRetrieval
  { peerStorageRetrievalBlob = bytes1k
  }
{-# NOINLINE peerStorageRetrievalSmall #-}

-- | PeerStorageRetrieval with 16KB blob.
peerStorageRetrievalLarge :: PeerStorageRetrieval
peerStorageRetrievalLarge = PeerStorageRetrieval
  { peerStorageRetrievalBlob = bytes16k
  }
{-# NOINLINE peerStorageRetrievalLarge #-}

-- Sample TLV streams -----------------------------------------------------

-- | Empty TLV stream.
emptyTlvStream :: TlvStream
emptyTlvStream = unsafeTlvStream []
{-# NOINLINE emptyTlvStream #-}

-- | TLV stream with 1 record.
smallTlvStream :: TlvStream
smallTlvStream = unsafeTlvStream
  [ TlvRecord 1 (BS.replicate 32 0x01)
  ]
{-# NOINLINE smallTlvStream #-}

-- | TLV stream with 5 records.
mediumTlvStream :: TlvStream
mediumTlvStream = unsafeTlvStream
  [ TlvRecord 1 (BS.replicate 8 0x01)
  , TlvRecord 3 (BS.replicate 16 0x03)
  , TlvRecord 5 (BS.replicate 32 0x05)
  , TlvRecord 7 (BS.replicate 64 0x07)
  , TlvRecord 9 (BS.replicate 128 0x09)
  ]
{-# NOINLINE mediumTlvStream #-}

-- | TLV stream with 20 records.
largeTlvStream :: TlvStream
largeTlvStream = unsafeTlvStream
  [ TlvRecord 1  (BS.replicate 8 0x01)
  , TlvRecord 3  (BS.replicate 16 0x02)
  , TlvRecord 5  (BS.replicate 8 0x03)
  , TlvRecord 7  (BS.replicate 16 0x04)
  , TlvRecord 9  (BS.replicate 8 0x05)
  , TlvRecord 11 (BS.replicate 16 0x06)
  , TlvRecord 13 (BS.replicate 8 0x07)
  , TlvRecord 15 (BS.replicate 16 0x08)
  , TlvRecord 17 (BS.replicate 8 0x09)
  , TlvRecord 19 (BS.replicate 16 0x0a)
  , TlvRecord 21 (BS.replicate 8 0x0b)
  , TlvRecord 23 (BS.replicate 16 0x0c)
  , TlvRecord 25 (BS.replicate 8 0x0d)
  , TlvRecord 27 (BS.replicate 16 0x0e)
  , TlvRecord 29 (BS.replicate 8 0x0f)
  , TlvRecord 31 (BS.replicate 16 0x10)
  , TlvRecord 33 (BS.replicate 8 0x11)
  , TlvRecord 35 (BS.replicate 16 0x12)
  , TlvRecord 37 (BS.replicate 8 0x13)
  , TlvRecord 39 (BS.replicate 16 0x14)
  ]
{-# NOINLINE largeTlvStream #-}

-- Encoded message bytes (for decode benchmarks) --------------------------

-- Helper to encode or fail.
encodeOrFail :: Either EncodeError BS.ByteString -> BS.ByteString
encodeOrFail (Right bs) = bs
encodeOrFail (Left _)   = error "encodeOrFail: encoding failed"

-- | Encoded minimal Init.
encodedMinimalInit :: BS.ByteString
encodedMinimalInit = encodeOrFail $ encodeEnvelope (MsgInitVal minimalInit) Nothing
{-# NOINLINE encodedMinimalInit #-}

-- | Encoded Init with TLVs.
encodedInitWithTlvs :: BS.ByteString
encodedInitWithTlvs = encodeOrFail $ encodeEnvelope (MsgInitVal initWithTlvs) Nothing
{-# NOINLINE encodedInitWithTlvs #-}

-- | Encoded full Init.
encodedFullInit :: BS.ByteString
encodedFullInit = encodeOrFail $ encodeEnvelope (MsgInitVal fullInit) Nothing
{-# NOINLINE encodedFullInit #-}

-- | Encoded minimal Error.
encodedMinimalError :: BS.ByteString
encodedMinimalError = encodeOrFail $ encodeEnvelope (MsgErrorVal minimalError) Nothing
{-# NOINLINE encodedMinimalError #-}

-- | Encoded Error with data.
encodedErrorWithData :: BS.ByteString
encodedErrorWithData = encodeOrFail $ encodeEnvelope (MsgErrorVal errorWithData) Nothing
{-# NOINLINE encodedErrorWithData #-}

-- | Encoded minimal Warning.
encodedMinimalWarning :: BS.ByteString
encodedMinimalWarning = encodeOrFail $
  encodeEnvelope (MsgWarningVal minimalWarning) Nothing
{-# NOINLINE encodedMinimalWarning #-}

-- | Encoded Warning with data.
encodedWarningWithData :: BS.ByteString
encodedWarningWithData = encodeOrFail $
  encodeEnvelope (MsgWarningVal warningWithData) Nothing
{-# NOINLINE encodedWarningWithData #-}

-- | Encoded minimal Ping.
encodedMinimalPing :: BS.ByteString
encodedMinimalPing = encodeOrFail $ encodeEnvelope (MsgPingVal minimalPing) Nothing
{-# NOINLINE encodedMinimalPing #-}

-- | Encoded Ping with padding.
encodedPingWithPadding :: BS.ByteString
encodedPingWithPadding = encodeOrFail $
  encodeEnvelope (MsgPingVal pingWithPadding) Nothing
{-# NOINLINE encodedPingWithPadding #-}

-- | Encoded Ping with large padding.
encodedPingWithLargePadding :: BS.ByteString
encodedPingWithLargePadding = encodeOrFail $
  encodeEnvelope (MsgPingVal pingWithLargePadding) Nothing
{-# NOINLINE encodedPingWithLargePadding #-}

-- | Encoded minimal Pong.
encodedMinimalPong :: BS.ByteString
encodedMinimalPong = encodeOrFail $ encodeEnvelope (MsgPongVal minimalPong) Nothing
{-# NOINLINE encodedMinimalPong #-}

-- | Encoded Pong with padding.
encodedPongWithPadding :: BS.ByteString
encodedPongWithPadding = encodeOrFail $
  encodeEnvelope (MsgPongVal pongWithPadding) Nothing
{-# NOINLINE encodedPongWithPadding #-}

-- | Encoded minimal PeerStorage.
encodedMinimalPeerStorage :: BS.ByteString
encodedMinimalPeerStorage = encodeOrFail $
  encodeEnvelope (MsgPeerStorageVal minimalPeerStorage) Nothing
{-# NOINLINE encodedMinimalPeerStorage #-}

-- | Encoded PeerStorage with 1KB blob.
encodedPeerStorageSmall :: BS.ByteString
encodedPeerStorageSmall = encodeOrFail $
  encodeEnvelope (MsgPeerStorageVal peerStorageSmall) Nothing
{-# NOINLINE encodedPeerStorageSmall #-}

-- | Encoded minimal PeerStorageRetrieval.
encodedMinimalPeerStorageRetrieval :: BS.ByteString
encodedMinimalPeerStorageRetrieval = encodeOrFail $
  encodeEnvelope (MsgPeerStorageRetrievalVal minimalPeerStorageRetrieval) Nothing
{-# NOINLINE encodedMinimalPeerStorageRetrieval #-}

-- | Encoded PeerStorageRetrieval with 1KB blob.
encodedPeerStorageRetrievalSmall :: BS.ByteString
encodedPeerStorageRetrievalSmall = encodeOrFail $
  encodeEnvelope (MsgPeerStorageRetrievalVal peerStorageRetrievalSmall) Nothing
{-# NOINLINE encodedPeerStorageRetrievalSmall #-}

-- Encoded TLV streams (for decode benchmarks) ----------------------------

-- | Encoded empty TLV stream.
encodedEmptyTlvStream :: BS.ByteString
encodedEmptyTlvStream = encodeTlvStream emptyTlvStream
{-# NOINLINE encodedEmptyTlvStream #-}

-- | Encoded small TLV stream (1 record).
encodedSmallTlvStream :: BS.ByteString
encodedSmallTlvStream = encodeTlvStream smallTlvStream
{-# NOINLINE encodedSmallTlvStream #-}

-- | Encoded medium TLV stream (5 records).
encodedMediumTlvStream :: BS.ByteString
encodedMediumTlvStream = encodeTlvStream mediumTlvStream
{-# NOINLINE encodedMediumTlvStream #-}

-- | Encoded large TLV stream (20 records).
encodedLargeTlvStream :: BS.ByteString
encodedLargeTlvStream = encodeTlvStream largeTlvStream
{-# NOINLINE encodedLargeTlvStream #-}
