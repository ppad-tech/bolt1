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
  ]

-- Message encode/decode tests -------------------------------------------------

message_tests :: TestTree
message_tests = testGroup "Messages" [
    testGroup "Init" [
      testCase "encode/decode minimal init" $ do
        let msg = Init "" "" []
            encoded = encodeMessage (MsgInitVal msg)
        case decodeMessage MsgInit encoded of
          Right (MsgInitVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    , testCase "encode/decode init with features" $ do
        let msg = Init (BS.pack [0x01]) (BS.pack [0x02, 0x0a]) []
            encoded = encodeMessage (MsgInitVal msg)
        case decodeMessage MsgInit encoded of
          Right (MsgInitVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    , testCase "encode/decode init with networks TLV" $ do
        let chainHash = BS.replicate 32 0xab
            msg = Init "" "" [InitNetworks [chainHash]]
            encoded = encodeMessage (MsgInitVal msg)
        case decodeMessage MsgInit encoded of
          Right (MsgInitVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "Error" [
      testCase "encode/decode error" $ do
        let cid = BS.replicate 32 0xff
            msg = Error cid "something went wrong"
            encoded = encodeMessage (MsgErrorVal msg)
        case decodeMessage MsgError encoded of
          Right (MsgErrorVal decoded) -> decoded @?= msg
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
            encoded = encodeMessage (MsgWarningVal msg)
        case decodeMessage MsgWarning encoded of
          Right (MsgWarningVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "Ping" [
      testCase "encode/decode ping" $ do
        let msg = Ping 100 (BS.replicate 10 0x00)
            encoded = encodeMessage (MsgPingVal msg)
        case decodeMessage MsgPing encoded of
          Right (MsgPingVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    , testCase "ping with zero ignored" $ do
        let msg = Ping 50 ""
            encoded = encodeMessage (MsgPingVal msg)
        case decodeMessage MsgPing encoded of
          Right (MsgPingVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "Pong" [
      testCase "encode/decode pong" $ do
        let msg = Pong (BS.replicate 100 0x00)
            encoded = encodeMessage (MsgPongVal msg)
        case decodeMessage MsgPong encoded of
          Right (MsgPongVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "PeerStorage" [
      testCase "encode/decode peer_storage" $ do
        let msg = PeerStorage "encrypted blob data"
            encoded = encodeMessage (MsgPeerStorageVal msg)
        case decodeMessage MsgPeerStorage encoded of
          Right (MsgPeerStorageVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    ]
  , testGroup "PeerStorageRetrieval" [
      testCase "encode/decode peer_storage_retrieval" $ do
        let msg = PeerStorageRetrieval "retrieved blob"
            encoded = encodeMessage (MsgPeerStorageRetrievalVal msg)
        case decodeMessage MsgPeerStorageRet encoded of
          Right (MsgPeerStorageRetrievalVal decoded) -> decoded @?= msg
          other -> assertFailure $ "unexpected: " ++ show other
    ]
  ]

-- Envelope tests --------------------------------------------------------------

envelope_tests :: TestTree
envelope_tests = testGroup "Envelope" [
    testCase "encode/decode init envelope" $ do
      let msg = MsgInitVal (Init "" "" [])
          encoded = encodeEnvelope msg Nothing
      case decodeEnvelope encoded of
        Right (Just decoded) -> decoded @?= msg
        other -> assertFailure $ "unexpected: " ++ show other
  , testCase "encode/decode ping envelope" $ do
      let msg = MsgPingVal (Ping 10 "")
          encoded = encodeEnvelope msg Nothing
      case decodeEnvelope encoded of
        Right (Just decoded) -> decoded @?= msg
        other -> assertFailure $ "unexpected: " ++ show other
  , testCase "unknown even type fails" $ do
      let bs = encodeU16 100 <> "payload"  -- 100 is even, unknown
      case decodeEnvelope bs of
        Left (DecodeUnknownEvenType 100) -> pure ()
        other -> assertFailure $ "expected unknown even: " ++ show other
  , testCase "unknown odd type ignored" $ do
      let bs = encodeU16 101 <> "payload"  -- 101 is odd, unknown
      case decodeEnvelope bs of
        Right Nothing -> pure ()  -- ignored
        other -> assertFailure $ "expected Nothing: " ++ show other
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
      let msg = Ping (fromIntegral (num `mod` 65536 :: Integer))
                     (BS.pack bs)
          encoded = encodeMessage (MsgPingVal msg)
      in case decodeMessage MsgPing encoded of
           Right (MsgPingVal decoded) -> decoded == msg
           _ -> False
  , testProperty "Pong roundtrip" $ \bs ->
      let msg = Pong (BS.pack bs)
          encoded = encodeMessage (MsgPongVal msg)
      in case decodeMessage MsgPong encoded of
           Right (MsgPongVal decoded) -> decoded == msg
           _ -> False
  , testProperty "PeerStorage roundtrip" $ \bs ->
      let msg = PeerStorage (BS.pack bs)
          encoded = encodeMessage (MsgPeerStorageVal msg)
      in case decodeMessage MsgPeerStorage encoded of
           Right (MsgPeerStorageVal decoded) -> decoded == msg
           _ -> False
  , testProperty "Error roundtrip" $ \bs ->
      let cid = BS.replicate 32 0x00
          msg = Error cid (BS.pack bs)
          encoded = encodeMessage (MsgErrorVal msg)
      in case decodeMessage MsgError encoded of
           Right (MsgErrorVal decoded) -> decoded == msg
           _ -> False
  ]

-- Helpers ---------------------------------------------------------------------

unhex :: BS.ByteString -> BS.ByteString
unhex bs = case B16.decode bs of
  Just r  -> r
  Nothing -> error $ "invalid hex: " ++ show bs
