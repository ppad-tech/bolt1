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
        let cid = BS.replicate 32 0xff
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
        let cid = BS.replicate 32 0x00
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
    testCase "encode envelope with extension" $ do
      let msg = MsgPingVal (Ping 10 "")
          ext = TlvStream [TlvRecord 100 "extension data"]
      case encodeEnvelope msg (Just ext) of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> do
          -- Should contain message + extension
          assertBool "encoded should be longer" (BS.length encoded > 6)
  , testCase "decode envelope with extension roundtrip" $ do
      let msg = MsgPingVal (Ping 10 "")
          ext = TlvStream [TlvRecord 101 "ext"]
      case encodeEnvelope msg (Just ext) of
        Left e -> assertFailure $ "encode failed: " ++ show e
        Right encoded -> case decodeEnvelope encoded of
          Right (Just decoded, Just decodedExt) -> do
            decoded @?= msg
            length (unTlvStream decodedExt) @?= 1
          other -> assertFailure $ "unexpected: " ++ show other
  , testCase "decode envelope extension is parsed" $ do
      -- Manually construct ping + extension TLV
      let pingPayload = mconcat [encodeU16 10, encodeU16 0]  -- numPong=10, len=0
          extTlv = mconcat [encodeBigSize 200, encodeBigSize 3, "abc"]
          envelope = encodeU16 18 <> pingPayload <> extTlv  -- type 18 = ping
      case decodeEnvelope envelope of
        Right (Just (MsgPingVal ping), Just (TlvStream [r])) -> do
          pingNumPongBytes ping @?= 10
          tlvType r @?= 200
          tlvValue r @?= "abc"
        other -> assertFailure $ "unexpected: " ++ show other
  , testCase "decode envelope with invalid extension fails" $ do
      -- Ping + invalid TLV (non-strictly-increasing)
      let pingPayload = mconcat [encodeU16 10, encodeU16 0]
          badTlv = mconcat [
              encodeBigSize 100, encodeBigSize 1, "a"
            , encodeBigSize 50, encodeBigSize 1, "b"  -- 50 < 100, invalid
            ]
          envelope = encodeU16 18 <> pingPayload <> badTlv
      case decodeEnvelope envelope of
        Left (DecodeInvalidExtension TlvNotStrictlyIncreasing) -> pure ()
        other -> assertFailure $ "expected invalid extension: " ++ show other
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
      let msg = Error (BS.replicate 32 0x00) (BS.replicate 70000 0x00)
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
      let cid = BS.replicate 32 0x00
          dat = BS.pack (take 1000 bs)
          msg = Error cid dat
      in case encodeMessage (MsgErrorVal msg) of
           Left _ -> False
           Right encoded -> case decodeMessage MsgError encoded of
             Right (MsgErrorVal decoded, rest) ->
               decoded == msg && BS.null rest
             _ -> False
  , testProperty "Envelope with extension roundtrip" $ \bs ->
      let msg = MsgPingVal (Ping 42 "")
          extData = BS.pack (take 100 bs)
          ext = TlvStream [TlvRecord 101 extData]
      in case encodeEnvelope msg (Just ext) of
           Left _ -> False
           Right encoded -> case decodeEnvelope encoded of
             Right (Just decoded, Just (TlvStream [r])) ->
               decoded == msg && tlvType r == 101 && tlvValue r == extData
             _ -> False
  ]

-- Helpers ---------------------------------------------------------------------

-- | Decode hex string. Fails the test on invalid hex.
unhex :: BS.ByteString -> BS.ByteString
unhex bs = case B16.decode bs of
  Just r  -> r
  Nothing -> assertFailure' $ "invalid hex: " ++ show bs

-- | assertFailure that returns any type (for use in pure contexts)
assertFailure' :: String -> a
assertFailure' msg = error msg
