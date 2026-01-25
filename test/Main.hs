{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Lightning.Protocol.BOLT1
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt1" [
    bigsize_tests
  , primitive_tests
  , signed_tests
  , truncated_tests
  , minsigned_tests
  , tlv_tests
  , message_tests
  , envelope_tests
  , extension_tests
  , bounds_tests
  , property_tests
  ]

-- BigSize test vectors from BOLT #1 Appendix A -------------------------------

bigsize_tests :: TestTree
bigsize_tests = testGroup "BigSize (Appendix A)" [
    testCase "zero" $
      encodeBigSize 0 @?= unhex "00"
  , testCase "one byte high (252)" $
      encodeBigSize 252 @?= unhex "fc"
  , testCase "two byte low (253)" $
      encodeBigSize 253 @?= unhex "fd00fd"
  , testCase "two byte high (65535)" $
      encodeBigSize 65535 @?= unhex "fdffff"
  , testCase "four byte low (65536)" $
      encodeBigSize 65536 @?= unhex "fe00010000"
  , testCase "four byte high (4294967295)" $
      encodeBigSize 4294967295 @?= unhex "feffffffff"
  , testCase "eight byte low (4294967296)" $
      encodeBigSize 4294967296 @?= unhex "ff0000000100000000"
  , testCase "eight byte high (max u64)" $
      encodeBigSize 18446744073709551615 @?= unhex "ffffffffffffffffff"
  , testCase "decode zero" $
      decodeBigSize (unhex "00") @?= Just (0, "")
  , testCase "decode 252" $
      decodeBigSize (unhex "fc") @?= Just (252, "")
  , testCase "decode 253" $
      decodeBigSize (unhex "fd00fd") @?= Just (253, "")
  , testCase "decode 65535" $
      decodeBigSize (unhex "fdffff") @?= Just (65535, "")
  , testCase "decode 65536" $
      decodeBigSize (unhex "fe00010000") @?= Just (65536, "")
  , testCase "decode 4294967295" $
      decodeBigSize (unhex "feffffffff") @?= Just (4294967295, "")
  , testCase "decode 4294967296" $
      decodeBigSize (unhex "ff0000000100000000") @?= Just (4294967296, "")
  , testCase "decode max u64" $
      decodeBigSize (unhex "ffffffffffffffffff") @?=
        Just (18446744073709551615, "")
  , testCase "non-minimal 2-byte fails" $
      decodeBigSize (unhex "fd00fc") @?= Nothing
  , testCase "non-minimal 4-byte fails" $
      decodeBigSize (unhex "fe0000ffff") @?= Nothing
  , testCase "non-minimal 8-byte fails" $
      decodeBigSize (unhex "ff00000000ffffffff") @?= Nothing
  ]

-- Primitive encode/decode tests -----------------------------------------------

primitive_tests :: TestTree
primitive_tests = testGroup "Primitives" [
    testCase "encodeU16 0x0102" $
      encodeU16 0x0102 @?= BS.pack [0x01, 0x02]
  , testCase "decodeU16 0x0102" $
      decodeU16 (BS.pack [0x01, 0x02]) @?= Just (0x0102, "")
  , testCase "encodeU32 0x01020304" $
      encodeU32 0x01020304 @?= BS.pack [0x01, 0x02, 0x03, 0x04]
  , testCase "decodeU32 0x01020304" $
      decodeU32 (BS.pack [0x01, 0x02, 0x03, 0x04]) @?= Just (0x01020304, "")
  , testCase "encodeU64" $
      encodeU64 0x0102030405060708 @?=
        BS.pack [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]
  , testCase "decodeU64" $
      decodeU64 (BS.pack [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]) @?=
        Just (0x0102030405060708, "")
  , testCase "decodeU16 insufficient" $
      decodeU16 (BS.pack [0x01]) @?= Nothing
  , testCase "decodeU32 insufficient" $
      decodeU32 (BS.pack [0x01, 0x02]) @?= Nothing
  , testCase "decodeU64 insufficient" $
      decodeU64 (BS.pack [0x01, 0x02, 0x03, 0x04]) @?= Nothing
  ]

-- Signed integer tests ---------------------------------------------------------

signed_tests :: TestTree
signed_tests = testGroup "Signed integers" [
    testCase "encodeS8 42" $
      encodeS8 42 @?= BS.pack [0x2a]
  , testCase "encodeS8 -42" $
      encodeS8 (-42) @?= BS.pack [0xd6]
  , testCase "encodeS8 127" $
      encodeS8 127 @?= BS.pack [0x7f]
  , testCase "encodeS8 -128" $
      encodeS8 (-128) @?= BS.pack [0x80]
  , testCase "decodeS8 42" $
      decodeS8 (BS.pack [0x2a]) @?= Just (42, "")
  , testCase "decodeS8 -42" $
      decodeS8 (BS.pack [0xd6]) @?= Just (-42, "")
  , testCase "encodeS16 -1" $
      encodeS16 (-1) @?= BS.pack [0xff, 0xff]
  , testCase "encodeS16 32767" $
      encodeS16 32767 @?= BS.pack [0x7f, 0xff]
  , testCase "encodeS16 -32768" $
      encodeS16 (-32768) @?= BS.pack [0x80, 0x00]
  , testCase "decodeS16 -1" $
      decodeS16 (BS.pack [0xff, 0xff]) @?= Just (-1, "")
  , testCase "encodeS32 -1" $
      encodeS32 (-1) @?= BS.pack [0xff, 0xff, 0xff, 0xff]
  , testCase "encodeS32 2147483647" $
      encodeS32 2147483647 @?= BS.pack [0x7f, 0xff, 0xff, 0xff]
  , testCase "encodeS32 -2147483648" $
      encodeS32 (-2147483648) @?= BS.pack [0x80, 0x00, 0x00, 0x00]
  , testCase "decodeS32 -1" $
      decodeS32 (BS.pack [0xff, 0xff, 0xff, 0xff]) @?= Just (-1, "")
  , testCase "encodeS64 -1" $
      encodeS64 (-1) @?=
        BS.pack [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
  , testCase "decodeS64 -1" $
      decodeS64 (BS.pack [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]) @?=
        Just (-1, "")
  ]

-- Truncated unsigned integer tests ---------------------------------------------

truncated_tests :: TestTree
truncated_tests = testGroup "Truncated unsigned integers" [
    testCase "encodeTu16 0" $
      encodeTu16 0 @?= ""
  , testCase "encodeTu16 1" $
      encodeTu16 1 @?= BS.pack [0x01]
  , testCase "encodeTu16 255" $
      encodeTu16 255 @?= BS.pack [0xff]
  , testCase "encodeTu16 256" $
      encodeTu16 256 @?= BS.pack [0x01, 0x00]
  , testCase "encodeTu16 65535" $
      encodeTu16 65535 @?= BS.pack [0xff, 0xff]
  , testCase "decodeTu16 0 bytes" $
      decodeTu16 0 "" @?= Just (0, "")
  , testCase "decodeTu16 1 byte" $
      decodeTu16 1 (BS.pack [0x01]) @?= Just (1, "")
  , testCase "decodeTu16 2 bytes" $
      decodeTu16 2 (BS.pack [0x01, 0x00]) @?= Just (256, "")
  , testCase "decodeTu16 non-minimal fails" $
      decodeTu16 2 (BS.pack [0x00, 0x01]) @?= Nothing
  , testCase "encodeTu32 0" $
      encodeTu32 0 @?= ""
  , testCase "encodeTu32 1" $
      encodeTu32 1 @?= BS.pack [0x01]
  , testCase "encodeTu32 0x010000" $
      encodeTu32 0x010000 @?= BS.pack [0x01, 0x00, 0x00]
  , testCase "encodeTu32 0x01000000" $
      encodeTu32 0x01000000 @?= BS.pack [0x01, 0x00, 0x00, 0x00]
  , testCase "decodeTu32 0 bytes" $
      decodeTu32 0 "" @?= Just (0, "")
  , testCase "decodeTu32 3 bytes" $
      decodeTu32 3 (BS.pack [0x01, 0x00, 0x00]) @?= Just (0x010000, "")
  , testCase "decodeTu32 non-minimal fails" $
      decodeTu32 3 (BS.pack [0x00, 0x01, 0x00]) @?= Nothing
  , testCase "encodeTu64 0" $
      encodeTu64 0 @?= ""
  , testCase "encodeTu64 0x0100000000" $
      encodeTu64 0x0100000000 @?= BS.pack [0x01, 0x00, 0x00, 0x00, 0x00]
  , testCase "decodeTu64 5 bytes" $
      decodeTu64 5 (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00]) @?=
        Just (0x0100000000, "")
  , testCase "decodeTu64 non-minimal fails" $
      decodeTu64 5 (BS.pack [0x00, 0x01, 0x00, 0x00, 0x00]) @?= Nothing
  ]

-- Minimal signed integer tests (Appendix D) ------------------------------------

minsigned_tests :: TestTree
minsigned_tests = testGroup "Minimal signed (Appendix D)" [
    -- Test vectors from BOLT #1 Appendix D
    testCase "encode 0" $
      encodeMinSigned 0 @?= unhex "00"
  , testCase "encode 42" $
      encodeMinSigned 42 @?= unhex "2a"
  , testCase "encode -42" $
      encodeMinSigned (-42) @?= unhex "d6"
  , testCase "encode 127" $
      encodeMinSigned 127 @?= unhex "7f"
  , testCase "encode -128" $
      encodeMinSigned (-128) @?= unhex "80"
  , testCase "encode 128" $
      encodeMinSigned 128 @?= unhex "0080"
  , testCase "encode -129" $
      encodeMinSigned (-129) @?= unhex "ff7f"
  , testCase "encode 15000" $
      encodeMinSigned 15000 @?= unhex "3a98"
  , testCase "encode -15000" $
      encodeMinSigned (-15000) @?= unhex "c568"
  , testCase "encode 32767" $
      encodeMinSigned 32767 @?= unhex "7fff"
  , testCase "encode -32768" $
      encodeMinSigned (-32768) @?= unhex "8000"
  , testCase "encode 32768" $
      encodeMinSigned 32768 @?= unhex "00008000"
  , testCase "encode -32769" $
      encodeMinSigned (-32769) @?= unhex "ffff7fff"
  , testCase "encode 21000000" $
      encodeMinSigned 21000000 @?= unhex "01406f40"
  , testCase "encode -21000000" $
      encodeMinSigned (-21000000) @?= unhex "febf90c0"
  , testCase "encode 2147483647" $
      encodeMinSigned 2147483647 @?= unhex "7fffffff"
  , testCase "encode -2147483648" $
      encodeMinSigned (-2147483648) @?= unhex "80000000"
  , testCase "encode 2147483648" $
      encodeMinSigned 2147483648 @?= unhex "0000000080000000"
  , testCase "encode -2147483649" $
      encodeMinSigned (-2147483649) @?= unhex "ffffffff7fffffff"
  , testCase "encode 500000000000" $
      encodeMinSigned 500000000000 @?= unhex "000000746a528800"
  , testCase "encode -500000000000" $
      encodeMinSigned (-500000000000) @?= unhex "ffffff8b95ad7800"
  , testCase "encode max int64" $
      encodeMinSigned 9223372036854775807 @?= unhex "7fffffffffffffff"
  , testCase "encode min int64" $
      encodeMinSigned (-9223372036854775808) @?= unhex "8000000000000000"
  -- Decode tests
  , testCase "decode 1-byte 42" $
      decodeMinSigned 1 (unhex "2a") @?= Just (42, "")
  , testCase "decode 1-byte -42" $
      decodeMinSigned 1 (unhex "d6") @?= Just (-42, "")
  , testCase "decode 2-byte 128" $
      decodeMinSigned 2 (unhex "0080") @?= Just (128, "")
  , testCase "decode 2-byte -129" $
      decodeMinSigned 2 (unhex "ff7f") @?= Just (-129, "")
  , testCase "decode 4-byte 32768" $
      decodeMinSigned 4 (unhex "00008000") @?= Just (32768, "")
  , testCase "decode 8-byte 2147483648" $
      decodeMinSigned 8 (unhex "0000000080000000") @?= Just (2147483648, "")
  -- Minimality rejection
  , testCase "decode 2-byte for 1-byte value fails" $
      decodeMinSigned 2 (unhex "0042") @?= Nothing  -- 42 fits in 1 byte
  , testCase "decode 4-byte for 2-byte value fails" $
      decodeMinSigned 4 (unhex "00000080") @?= Nothing  -- 128 fits in 2 bytes
  , testCase "decode 8-byte for 4-byte value fails" $
      decodeMinSigned 8 (unhex "0000000000008000") @?= Nothing  -- 32768 fits in 4
  ]

-- TLV tests -------------------------------------------------------------------

tlv_tests :: TestTree
tlv_tests = testGroup "TLV" [
    testCase "empty stream" $
      decodeTlvStream "" @?= Right (TlvStream [])
  , testCase "single record type 1" $ do
      let bs = mconcat [
              encodeBigSize 1      -- type
            , encodeBigSize 32     -- length
            , BS.replicate 32 0x00 -- value (chain hash)
            ]
      case decodeTlvStream bs of
        Right (TlvStream [r]) -> do
          tlvType r @?= 1
          BS.length (tlvValue r) @?= 32
        other -> assertFailure $ "unexpected: " ++ show other
  , testCase "strictly increasing types" $ do
      let bs = mconcat [
              encodeBigSize 1, encodeBigSize 0
            , encodeBigSize 3, encodeBigSize 4, "test"
            ]
      case decodeTlvStream bs of
        Right (TlvStream recs) -> length recs @?= 2
        Left e -> assertFailure $ "unexpected error: " ++ show e
  , testCase "non-increasing types fails" $ do
      let bs = mconcat [
              encodeBigSize 3, encodeBigSize 0
            , encodeBigSize 1, encodeBigSize 0
            ]
      case decodeTlvStream bs of
        Left TlvNotStrictlyIncreasing -> pure ()
        other -> assertFailure $ "expected TlvNotStrictlyIncreasing: " ++
                                 show other
  , testCase "duplicate types fails" $ do
      let bs = mconcat [
              encodeBigSize 1, encodeBigSize 0
            , encodeBigSize 1, encodeBigSize 0
            ]
      case decodeTlvStream bs of
        Left TlvNotStrictlyIncreasing -> pure ()
        other -> assertFailure $ "expected TlvNotStrictlyIncreasing: " ++
                                 show other
  , testCase "unknown even type fails" $ do
      let bs = mconcat [encodeBigSize 2, encodeBigSize 0]
      case decodeTlvStream bs of
        Left (TlvUnknownEvenType 2) -> pure ()
        other -> assertFailure $ "expected TlvUnknownEvenType: " ++ show other
  , testCase "unknown odd type skipped" $ do
      let bs = mconcat [
              encodeBigSize 5, encodeBigSize 2, "hi"
            , encodeBigSize 7, encodeBigSize 0
            ]
      case decodeTlvStream bs of
        Right (TlvStream []) -> pure ()  -- both skipped (unknown odd)
        other -> assertFailure $ "expected empty stream: " ++ show other
  , testCase "length exceeds bounds fails" $ do
      let bs = mconcat [encodeBigSize 1, encodeBigSize 100, "short"]
      case decodeTlvStream bs of
        Left TlvLengthExceedsBounds -> pure ()
        other -> assertFailure $ "expected TlvLengthExceedsBounds: " ++
                                 show other
  , testCase "decodeTlvStreamWith custom predicate" $ do
      -- Use a predicate that only knows type 5
      let isKnown t = t == 5
          bs = mconcat [
              encodeBigSize 5, encodeBigSize 2, "hi"
            ]
      case decodeTlvStreamWith isKnown bs of
        Right (TlvStream [r]) -> tlvType r @?= 5
        other -> assertFailure $ "unexpected: " ++ show other
  , testCase "decodeTlvStreamRaw returns all records" $ do
      let bs = mconcat [
              encodeBigSize 2, encodeBigSize 1, "a"  -- even type
            , encodeBigSize 5, encodeBigSize 1, "b"  -- odd type
            ]
      case decodeTlvStreamRaw bs of
        Right (TlvStream recs) -> length recs @?= 2
        Left e -> assertFailure $ "unexpected error: " ++ show e
  ]

-- Message encode/decode tests -------------------------------------------------

message_tests :: TestTree
message_tests = testGroup "Messages" [
    testGroup "Init" [
      testCase "encode/decode minimal init" $ do
        let msg = Init "" "" []
        case encodeMessage (MsgInitVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgInit encoded of
            Right (MsgInitVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    , testCase "encode/decode init with features" $ do
        let msg = Init (BS.pack [0x01]) (BS.pack [0x02, 0x0a]) []
        case encodeMessage (MsgInitVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgInit encoded of
            Right (MsgInitVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    , testCase "encode/decode init with networks TLV" $ do
        let chainHash = BS.replicate 32 0xab
            msg = Init "" "" [InitNetworks [chainHash]]
        case encodeMessage (MsgInitVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgInit encoded of
            Right (MsgInitVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "Error" [
      testCase "encode/decode error" $ do
        let cid = unsafeChannelId (BS.replicate 32 0xff)
            msg = Error cid "something went wrong"
        case encodeMessage (MsgErrorVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgError encoded of
            Right (MsgErrorVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    , testCase "error insufficient channel_id" $ do
        case decodeMessage MsgError (BS.replicate 31 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    ]
  , testGroup "Warning" [
      testCase "encode/decode warning" $ do
        let cid = unsafeChannelId (BS.replicate 32 0x00)
            msg = Warning cid "be careful"
        case encodeMessage (MsgWarningVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgWarning encoded of
            Right (MsgWarningVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "Ping" [
      testCase "encode/decode ping" $ do
        let msg = Ping 100 (BS.replicate 10 0x00)
        case encodeMessage (MsgPingVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgPing encoded of
            Right (MsgPingVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    , testCase "ping with zero ignored" $ do
        let msg = Ping 50 ""
        case encodeMessage (MsgPingVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgPing encoded of
            Right (MsgPingVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "Pong" [
      testCase "encode/decode pong" $ do
        let msg = Pong (BS.replicate 100 0x00)
        case encodeMessage (MsgPongVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgPong encoded of
            Right (MsgPongVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "PeerStorage" [
      testCase "encode/decode peer_storage" $ do
        let msg = PeerStorage "encrypted blob data"
        case encodeMessage (MsgPeerStorageVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgPeerStorage encoded of
            Right (MsgPeerStorageVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "PeerStorageRetrieval" [
      testCase "encode/decode peer_storage_retrieval" $ do
        let msg = PeerStorageRetrieval "retrieved blob"
        case encodeMessage (MsgPeerStorageRetrievalVal msg) of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeMessage MsgPeerStorageRet encoded of
            Right (MsgPeerStorageRetrievalVal decoded, _) -> decoded @?= msg
            other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "Unknown types" [
      testCase "decodeMessage unknown even type" $ do
        case decodeMessage (MsgUnknown 100) "payload" of
          Left (DecodeUnknownEvenType 100) -> pure ()
          other -> assertFailure $ "expected unknown even: " ++ show other
    , testCase "decodeMessage unknown odd type" $ do
        case decodeMessage (MsgUnknown 101) "payload" of
          Left (DecodeUnknownOddType 101) -> pure ()
          other -> assertFailure $ "expected unknown odd: " ++ show other
    ]
  ]

-- Envelope tests --------------------------------------------------------------

envelope_tests :: TestTree
envelope_tests = testGroup "Envelope" [
    testCase "encode/decode init envelope" $ do
      let msg = MsgInitVal (Init "" "" [])
      case encodeEnvelope msg Nothing of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> case decodeEnvelope encoded of
          Right (Just decoded, _) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
  , testCase "encode/decode ping envelope" $ do
      let msg = MsgPingVal (Ping 10 "")
      case encodeEnvelope msg Nothing of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> case decodeEnvelope encoded of
          Right (Just decoded, _) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
  , testCase "unknown even type fails" $ do
      let bs = encodeU16 100 <> "payload"  -- 100 is even, unknown
      case decodeEnvelope bs of
        Left (DecodeUnknownEvenType 100) -> pure ()
        other -> assertFailure $ "expected unknown even: " ++ show other
  , testCase "unknown odd type ignored" $ do
      let bs = encodeU16 101 <> "payload"  -- 101 is odd, unknown
      case decodeEnvelope bs of
        Right (Nothing, Nothing) -> pure ()  -- ignored
        other -> assertFailure $ "expected (Nothing, Nothing): " ++ show other
  , testCase "insufficient bytes for type" $ do
      case decodeEnvelope (BS.pack [0x00]) of
        Left DecodeInsufficientBytes -> pure ()
        other -> assertFailure $ "expected insufficient: " ++ show other
  , testCase "message type codes" $ do
      msgTypeWord MsgInit @?= 16
      msgTypeWord MsgError @?= 17
      msgTypeWord MsgPing @?= 18
      msgTypeWord MsgPong @?= 19
      msgTypeWord MsgWarning @?= 1
      msgTypeWord MsgPeerStorage @?= 7
      msgTypeWord MsgPeerStorageRet @?= 9
  ]

-- Extension TLV tests ---------------------------------------------------------

extension_tests :: TestTree
extension_tests = testGroup "Extension TLV" [
    testCase "encode envelope with extension (odd type)" $ do
      let msg = MsgPingVal (Ping 10 "")
          ext = TlvStream [TlvRecord 101 "extension data"]  -- odd type
      case encodeEnvelope msg (Just ext) of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> do
          -- Should contain message + extension
          assertBool "encoded should be longer" (BS.length encoded > 6)
  , testCase "decode envelope with odd extension - skipped per BOLT#1" $ do
      -- Per BOLT #1: unknown odd types are ignored (skipped)
      let msg = MsgPingVal (Ping 10 "")
          ext = TlvStream [TlvRecord 101 "ext"]  -- odd type
      case encodeEnvelope msg (Just ext) of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> case decodeEnvelope encoded of
          Right (Just decoded, Just (TlvStream [])) -> do
            -- Extension is empty because unknown odd types are skipped
            decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
  , testCase "decode envelope with unknown even extension fails" $ do
      -- Per BOLT #1: unknown even types must cause failure
      let pingPayload = mconcat [encodeU16 10, encodeU16 0]  -- numPong=10, len=0
          extTlv = mconcat [encodeBigSize 100, encodeBigSize 3, "abc"]  -- even!
          envelope = encodeU16 18 <> pingPayload <> extTlv  -- type 18 = ping
      case decodeEnvelope envelope of
        Left (DecodeInvalidExtension (TlvUnknownEvenType 100)) -> pure ()
        other -> assertFailure $ "expected unknown even error: " ++ show other
  , testCase "decode envelope with invalid extension fails" $ do
      -- Ping + invalid TLV (non-strictly-increasing)
      let pingPayload = mconcat [encodeU16 10, encodeU16 0]
          badTlv = mconcat [
              encodeBigSize 101, encodeBigSize 1, "a"  -- odd types for this test
            , encodeBigSize 51, encodeBigSize 1, "b"   -- 51 < 101, invalid
            ]
          envelope = encodeU16 18 <> pingPayload <> badTlv
      case decodeEnvelope envelope of
        Left (DecodeInvalidExtension TlvNotStrictlyIncreasing) -> pure ()
        other -> assertFailure $ "expected invalid extension: " ++ show other
  , testCase "unknown even in extension fails even with odd types present" $ do
      -- Mixed odd and even - should fail on the even type
      let pingPayload = mconcat [encodeU16 10, encodeU16 0]
          extTlv = mconcat [
              encodeBigSize 101, encodeBigSize 1, "a"  -- odd, would be skipped
            , encodeBigSize 200, encodeBigSize 1, "b"  -- even, must fail
            ]
          envelope = encodeU16 18 <> pingPayload <> extTlv
      case decodeEnvelope envelope of
        Left (DecodeInvalidExtension (TlvUnknownEvenType 200)) -> pure ()
        other -> assertFailure $ "expected unknown even error: " ++ show other
  ]

-- Bounds checking tests -------------------------------------------------------

bounds_tests :: TestTree
bounds_tests = testGroup "Bounds checking" [
    testCase "encode ping with oversized ignored fails" $ do
      let msg = Ping 10 (BS.replicate 70000 0x00)  -- > 65535
      case encodeMessage (MsgPingVal msg) of
        Left EncodeLengthOverflow -> pure ()
        other -> assertFailure $ "expected overflow: " ++ show other
  , testCase "encode pong with oversized ignored fails" $ do
      let msg = Pong (BS.replicate 70000 0x00)
      case encodeMessage (MsgPongVal msg) of
        Left EncodeLengthOverflow -> pure ()
        other -> assertFailure $ "expected overflow: " ++ show other
  , testCase "encode error with oversized data fails" $ do
      let cid = unsafeChannelId (BS.replicate 32 0x00)
          msg = Error cid (BS.replicate 70000 0x00)
      case encodeMessage (MsgErrorVal msg) of
        Left EncodeLengthOverflow -> pure ()
        other -> assertFailure $ "expected overflow: " ++ show other
  , testCase "encode init with oversized features fails" $ do
      let msg = Init "" (BS.replicate 70000 0x00) []
      case encodeMessage (MsgInitVal msg) of
        Left EncodeLengthOverflow -> pure ()
        other -> assertFailure $ "expected overflow: " ++ show other
  , testCase "encode peer_storage with oversized blob fails" $ do
      let msg = PeerStorage (BS.replicate 70000 0x00)
      case encodeMessage (MsgPeerStorageVal msg) of
        Left EncodeLengthOverflow -> pure ()
        other -> assertFailure $ "expected overflow: " ++ show other
  , testCase "encode envelope exceeding 65535 bytes fails" $ do
      -- Create a message that fits in encodeMessage but combined with
      -- extension exceeds 65535 bytes total
      let msg = MsgPongVal (Pong (BS.replicate 60000 0x00))
          ext = TlvStream [TlvRecord 101 (BS.replicate 10000 0x00)]
      case encodeEnvelope msg (Just ext) of
        Left EncodeMessageTooLarge -> pure ()
        other -> assertFailure $ "expected message too large: " ++ show other
  ]

-- Property tests --------------------------------------------------------------

property_tests :: TestTree
property_tests = testGroup "Properties" [
    testProperty "BigSize roundtrip" $ \(NonNegative n) ->
      case decodeBigSize (encodeBigSize n) of
        Just (m, rest) -> m == n && BS.null rest
        Nothing -> False
  , testProperty "U16 roundtrip" $ \w ->
      decodeU16 (encodeU16 w) == Just (w, "")
  , testProperty "U32 roundtrip" $ \w ->
      decodeU32 (encodeU32 w) == Just (w, "")
  , testProperty "U64 roundtrip" $ \w ->
      decodeU64 (encodeU64 w) == Just (w, "")
  , testProperty "Ping roundtrip" $ \(NonNegative num) bs ->
      let ignored = BS.pack (take 1000 bs)  -- limit size
          msg = Ping (fromIntegral (num `mod` 65536 :: Integer)) ignored
      in case encodeMessage (MsgPingVal msg) of
           Left _ -> False
           Right encoded -> case decodeMessage MsgPing encoded of
             Right (MsgPingVal decoded, rest) ->
               decoded == msg && BS.null rest
             _ -> False
  , testProperty "Pong roundtrip" $ \bs ->
      let ignored = BS.pack (take 1000 bs)
          msg = Pong ignored
      in case encodeMessage (MsgPongVal msg) of
           Left _ -> False
           Right encoded -> case decodeMessage MsgPong encoded of
             Right (MsgPongVal decoded, rest) ->
               decoded == msg && BS.null rest
             _ -> False
  , testProperty "PeerStorage roundtrip" $ \bs ->
      let blob = BS.pack (take 1000 bs)
          msg = PeerStorage blob
      in case encodeMessage (MsgPeerStorageVal msg) of
           Left _ -> False
           Right encoded -> case decodeMessage MsgPeerStorage encoded of
             Right (MsgPeerStorageVal decoded, rest) ->
               decoded == msg && BS.null rest
             _ -> False
  , testProperty "Error roundtrip" $ \bs ->
      let cid = unsafeChannelId (BS.replicate 32 0x00)
          dat = BS.pack (take 1000 bs)
          msg = Error cid dat
      in case encodeMessage (MsgErrorVal msg) of
           Left _ -> False
           Right encoded -> case decodeMessage MsgError encoded of
             Right (MsgErrorVal decoded, rest) ->
               decoded == msg && BS.null rest
             _ -> False
  , testProperty "Envelope with odd extension (skipped per BOLT#1)" $ \bs ->
      -- Unknown odd types in extensions are skipped per BOLT #1
      let msg = MsgPingVal (Ping 42 "")
          extData = BS.pack (take 100 bs)
          ext = TlvStream [TlvRecord 101 extData]  -- odd type, will be skipped
      in case encodeEnvelope msg (Just ext) of
           Left _ -> False
           Right encoded -> case decodeEnvelope encoded of
             -- Extension should be empty (odd types skipped)
             Right (Just decoded, Just (TlvStream [])) -> decoded == msg
             _ -> False
  ]

-- Helpers ---------------------------------------------------------------------

-- | Construct a 'ChannelId' from a known-valid 32-byte 'BS.ByteString'.
--
-- Uses 'error' for invalid input since all channel IDs in tests are
-- known-valid compile-time constants.
unsafeChannelId :: BS.ByteString -> ChannelId
unsafeChannelId bs = case channelId bs of
  Just cid -> cid
  Nothing  -> error $ "unsafeChannelId: invalid length: " ++ show (BS.length bs)

-- | Decode hex string (test-only helper).
--
-- Uses 'error' for invalid hex since all hex literals in tests are
-- known-valid compile-time constants. This is acceptable in test code
-- where the failure would indicate a bug in the test itself.
unhex :: BS.ByteString -> BS.ByteString
unhex bs = case B16.decode bs of
  Just r  -> r
  Nothing -> error $ "unhex: invalid hex literal: " ++ show bs
