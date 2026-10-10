{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int64)
import qualified Data.List as L
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Word (Word16, Word32, Word64)
import Lightning.Protocol.BOLT1
import qualified Lightning.Protocol.BOLT9 as BOLT9
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt1" [
    appendix_a
  , appendix_b
  , appendix_c
  , appendix_d
  , primitives
  , tlv
  , messages
  , properties
  ]

-- helpers --------------------------------------------------------------------

-- run an assertion on decoded hex (spaces and a 0x prefix are allowed),
-- failing the test if the literal isn't valid hex
with_hex :: String -> (BS.ByteString -> Assertion) -> Assertion
with_hex s k = case B16.decode (B8.pack (strip s)) of
  Nothing -> assertFailure ("invalid hex literal: " ++ s)
  Just bs -> k bs
  where
    strip = filter (/= ' ') . drop0x
    drop0x ('0':'x':r) = r
    drop0x r = r

is_left :: Either a b -> Bool
is_left = either (const True) (const False)

empty_fv :: BOLT9.FeatureVector
empty_fv = BOLT9.parse ""

-- Appendix A: BigSize --------------------------------------------------------

appendix_a :: TestTree
appendix_a = testGroup "Appendix A (BigSize)" [
    testGroup "decoding" (map dec ok_vectors)
  , testGroup "encoding" (map enc ok_vectors)
  , testGroup "decoding failures" (map bad bad_vectors)
  ]
  where
    dec (v, h) = testCase ("decode " ++ h) $ with_hex h $ \bs ->
      decode_bigsize bs @?= Just (v, "")
    enc (v, h) = testCase ("encode " ++ show v) $ with_hex h $ \bs ->
      encode_bigsize v @?= bs
    bad h = testCase ("reject " ++ show h) $ with_hex h $ \bs ->
      decode_bigsize bs @?= Nothing

    ok_vectors :: [(Word64, String)]
    ok_vectors = [
        (0, "00"), (252, "fc"), (253, "fd00fd"), (65535, "fdffff")
      , (65536, "fe00010000"), (4294967295, "feffffffff")
      , (4294967296, "ff0000000100000000")
      , (18446744073709551615, "ffffffffffffffffff")
      ]

    -- non-canonical encodings, short reads and no reads
    bad_vectors = [
        "fd00fc", "fe0000ffff", "ff00000000ffffffff"
      , "fd00", "feffff", "ffffffffff"
      , "", "fd", "fe", "ff"
      ]

-- Appendix B: TLV ------------------------------------------------------------

-- the n1 and n2 test namespaces, decoded with bolt1's primitives

data N1 = N1Tlv1 !Word64
        | N1Tlv2 !ShortChannelId
        | N1Tlv3 !Point !Word64 !Word64
        | N1Tlv4 !Word16
  deriving (Eq, Show)

decode_n1 :: BS.ByteString -> Either String [N1]
decode_n1 bs = do
  s <- either (Left . show) Right (decode_tlv_stream (`elem` n1_types) bs)
  sequence [ one r | r <- un_tlv_stream s, tlv_type r `elem` n1_types ]
  where
    n1_types = [1, 2, 3, 254]
    one (TlvRecord 1 v) = maybe (Left "tlv1") (Right . N1Tlv1)
      (decode_tu64 v)
    one (TlvRecord 2 v) = case decode_short_channel_id v of
      Just (c, "") -> Right (N1Tlv2 c)
      _ -> Left "tlv2"
    one (TlvRecord 3 v) = maybe (Left "tlv3") Right $ do
      (p, r0) <- decode_point v
      (a, r1) <- decode_u64 r0
      (b, r2) <- decode_u64 r1
      if BS.null r2 then pure (N1Tlv3 p a b) else Nothing
    one (TlvRecord 254 v) = case decode_u16 v of
      Just (c, "") -> Right (N1Tlv4 c)
      _ -> Left "tlv4"
    one _ = Left "unreachable"

decode_n2 :: BS.ByteString -> Either String [(Word64, Word64)]
decode_n2 bs = do
  s <- either (Left . show) Right (decode_tlv_stream (`elem` [0, 11]) bs)
  sequence [ one r | r <- un_tlv_stream s, tlv_type r `elem` [0, 11] ]
  where
    one (TlvRecord 0 v) = maybe (Left "tlv1") (Right . (,) 0) (decode_tu64 v)
    one (TlvRecord 11 v) =
      maybe (Left "tlv2") (Right . (,) 11 . fromIntegral) (decode_tu32 v)
    one _ = Left "unreachable"

appendix_b :: TestTree
appendix_b = testGroup "Appendix B (TLV)" [
    testGroup "failures in any namespace" [
      testCase h $ with_hex h $ \bs -> do
        assertBool "n1" (is_left (decode_n1 bs))
        assertBool "n2" (is_left (decode_n2 bs))
    | h <- any_fail ]
  , testGroup "n1 failures" [
      testCase h $ with_hex h $ \bs -> assertBool "n1" (is_left (decode_n1 bs))
    | h <- n1_fail ]
  , testGroup "n2 failures" [
      testCase h $ with_hex h $ \bs -> assertBool "n2" (is_left (decode_n2 bs))
    | h <- n2_fail ]
  , testGroup "ignored successes" [
      testCase h $ with_hex h $ \bs -> do
        decode_n1 bs @?= Right []
        decode_n2 bs @?= Right []
    | h <- any_ok ]
  , testGroup "n1 successes" [
      testCase h $ with_hex h $ \bs -> decode_n1 bs @?= Right [v]
    | (h, v) <- n1_ok ]
  , testCase "n1 tlv3" $ with_hex n1_tlv3 $ \bs ->
      with_hex node_id $ \pk -> case point pk of
        Nothing -> assertFailure "bad node_id"
        Just p  -> decode_n1 bs @?= Right [N1Tlv3 p 1 2]
  , testCase "unknown even types fail when no types are known" $
      mapM_ (\h -> with_hex h $ \bs ->
        assertBool h (is_left (decode_tlv_stream (const False) bs)))
        ["12 00", "fd0102 00", "fe01000002 00", "ff0100000000000002 00"]
  , testCase "appending a higher-numbered valid stream succeeds" $
      with_hex "01 01 01" $ \a -> with_hex "fd00fe 02 0226" $ \b ->
        decode_n1 (a <> b) @?= Right [N1Tlv1 1, N1Tlv4 550]
  , testCase "appending an invalid stream fails" $
      with_hex "01 01 01" $ \a -> mapM_ (\h -> with_hex h $ \b ->
        assertBool h (is_left (decode_n1 (a <> b)))) any_fail
  ]
  where
    any_fail = [
        "0xfd", "0xfd01", "0xfd0001 00", "0xfd0101", "0x0f fd", "0x0f fd26"
      , "0x0f fd2602", "0x0f fd0001 00", "0x0f fd0201 " ++ replicate 1024 '0'
      , "0x12 00", "0xfd0102 00", "0xfe01000002 00"
      , "0xff0100000000000002 00"
      ]
    n1_fail = [
        "0x01 09 ffffffffffffffffff", "0x01 01 00", "0x01 02 0001"
      , "0x01 03 000100", "0x01 04 00010000", "0x01 05 0001000000"
      , "0x01 06 000100000000", "0x01 07 00010000000000"
      , "0x01 08 0001000000000000", "0x02 07 01010101010101"
      , "0x02 09 010101010101010101"
      , "0x03 21 " ++ node_id
      , "0x03 29 " ++ node_id ++ "0000000000000001"
      , "0x03 30 " ++ node_id ++ "000000000000000100000000000001"
      , "0x03 31 04" ++ drop 2 node_id
          ++ "00000000000000010000000000000002"
      , "0x03 32 " ++ node_id ++ "0000000000000001000000000000000001"
      , "0xfd00fe 00", "0xfd00fe 01 01", "0xfd00fe 03 010101", "0x00 00"
      , "0x02 08 0000000000000226 01 01 2a"
      , "0x02 08 0000000000000231 02 08 0000000000000451"
      , "0x1f 00 0f 01 2a", "0x1f 00 1f 01 2a"
      ]
    n2_fail = ["0xffffffffffffffffff 00 00 00"]
    any_ok = [
        "0x", "0x21 00", "0xfd0201 00", "0xfd00fd 00", "0xfd00ff 00"
      , "0xfe02000001 00", "0xff0200000000000001 00"
      ]
    n1_ok = [
        ("0x01 00", N1Tlv1 0), ("0x01 01 01", N1Tlv1 1)
      , ("0x01 02 0100", N1Tlv1 256), ("0x01 03 010000", N1Tlv1 65536)
      , ("0x01 04 01000000", N1Tlv1 16777216)
      , ("0x01 05 0100000000", N1Tlv1 4294967296)
      , ("0x01 06 010000000000", N1Tlv1 1099511627776)
      , ("0x01 07 01000000000000", N1Tlv1 281474976710656)
      , ("0x01 08 0100000000000000", N1Tlv1 72057594037927936)
      , ("0x02 08 0000000000000226", N1Tlv2 (ShortChannelId 0x226))
      , ("0xfd00fe 02 0226", N1Tlv4 550)
      ]
    node_id =
      "023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"
    n1_tlv3 = "0x03 31 " ++ node_id ++ "00000000000000010000000000000002"

-- Appendix C: message extension ----------------------------------------------

appendix_c :: TestTree
appendix_c = testGroup "Appendix C (message extension)" [
    testCase "no extension" $ with_hex "001000000000" $ \bs ->
      case decode_message bs of
        Right (MsgInit i) -> do
          init_networks i @?= Nothing
          init_tlvs i @?= empty_tlv_stream
        other -> assertFailure (show other)
  , testCase "two unknown odd records are kept" $
      with_hex "001000000000c9012acb0104" $ \bs ->
        case decode_message bs of
          Right m@(MsgInit i) -> do
            map tlv_type (un_tlv_stream (init_tlvs i)) @?= [0xc9, 0xcb]
            encode_message m @?= Right bs
          other -> assertFailure (show other)
  , testCase "truncated extension" $ with_hex "00100000000001" $ \bs ->
      decode_message bs @?= Left (DecodeTlvError TlvTruncated)
  , testCase "unknown even record" $ with_hex "001000000000ca012a" $ \bs ->
      decode_message bs @?= Left (DecodeTlvError (TlvUnknownEvenType 0xca))
  , testCase "duplicate record" $ with_hex "001000000000c90101c90102" $ \bs ->
      decode_message bs @?= Left (DecodeTlvError TlvNotStrictlyIncreasing)
  ]

-- Appendix D: signed integers ------------------------------------------------

appendix_d :: TestTree
appendix_d = testGroup "Appendix D (signed integers)" [
    testCase (show v) $ with_hex h $ \bs -> case BS.length bs of
      1 -> do
        encode_s8 (fromIntegral v) @?= bs
        decode_s8 bs @?= Just (fromIntegral v, "")
      2 -> do
        encode_s16 (fromIntegral v) @?= bs
        decode_s16 bs @?= Just (fromIntegral v, "")
      4 -> do
        encode_s32 (fromIntegral v) @?= bs
        decode_s32 bs @?= Just (fromIntegral v, "")
      8 -> do
        encode_s64 v @?= bs
        decode_s64 bs @?= Just (v, "")
      _ -> assertFailure "bad vector width"
  | (v, h) <- vectors ]
  where
    vectors :: [(Int64, String)]
    vectors = [
        (0, "00"), (42, "2a"), (-42, "d6"), (127, "7f"), (-128, "80")
      , (128, "0080"), (-129, "ff7f"), (15000, "3a98"), (-15000, "c568")
      , (32767, "7fff"), (-32768, "8000"), (32768, "00008000")
      , (-32769, "ffff7fff"), (21000000, "01406f40")
      , (-21000000, "febf90c0"), (2147483647, "7fffffff")
      , (-2147483648, "80000000"), (2147483648, "0000000080000000")
      , (-2147483649, "ffffffff7fffffff"), (500000000000, "000000746a528800")
      , (-500000000000, "ffffff8b95ad7800")
      , (9223372036854775807, "7fffffffffffffff")
      , (-9223372036854775808, "8000000000000000")
      ]

-- primitives -----------------------------------------------------------------

primitives :: TestTree
primitives = testGroup "Primitives" [
    testCase "u16/u32/u64 known answers" $ do
      encode_u16 0x0102 @?= "\x01\x02"
      encode_u32 0x01020304 @?= "\x01\x02\x03\x04"
      encode_u64 0x0102030405060708 @?= "\x01\x02\x03\x04\x05\x06\x07\x08"
      decode_u32 "\x01\x02\x03\x04rest" @?= Just (0x01020304, "rest")
  , testCase "short reads fail" $ do
      decode_u16 "\x01" @?= Nothing
      decode_u32 "\x01\x02\x03" @?= Nothing
      decode_u64 "\x01\x02\x03\x04\x05\x06\x07" @?= Nothing
      decode_s8 "" @?= Nothing
  , testCase "truncated integers" $ do
      encode_tu64 0 @?= ""
      encode_tu64 1 @?= "\x01"
      encode_tu32 0x010000 @?= "\x01\x00\x00"
      decode_tu16 "" @?= Just 0
      decode_tu16 "\x00\x01" @?= Nothing
      decode_tu16 "\x01\x00\x00" @?= Nothing
      decode_tu32 "\x01\x00\x00\x00\x00" @?= Nothing
      decode_tu64 "\x01\x00\x00\x00\x00\x00\x00\x00\x00" @?= Nothing
  , testCase "length-prefixed bytes" $ do
      encode_u16_prefixed "abc" @?= Just "\x00\x03\&abc"
      encode_u16_prefixed (BS.replicate 65536 0) @?= Nothing
      decode_u16_prefixed "\x00\x03\&abcde" @?= Just ("abc", "de")
      decode_u16_prefixed "\x00\x03\&ab" @?= Nothing
  , testCase "fixed-size types check their lengths" $ do
      assertBool "chain_hash" (isNothing (chain_hash (BS.replicate 31 0)))
      assertBool "channel_id" (isNothing (channel_id (BS.replicate 33 0)))
      assertBool "signature" (isNothing (signature (BS.replicate 65 0)))
      assertBool "payment_hash" (isNothing (payment_hash ""))
      assertBool "preimage" (isNothing (payment_preimage ""))
      assertBool "secret" (isNothing (per_commitment_secret ""))
  , testCase "point prefix" $ do
      assertBool "02" (isJust (point (BS.cons 0x02 (BS.replicate 32 1))))
      assertBool "03" (isJust (point (BS.cons 0x03 (BS.replicate 32 1))))
      assertBool "04" (isNothing (point (BS.cons 0x04 (BS.replicate 32 1))))
      assertBool "len" (isNothing (point (BS.cons 0x02 (BS.replicate 31 1))))
  , testCase "short_channel_id" $ case short_channel_id 539268 845 1 of
      Nothing -> assertFailure "valid scid rejected"
      Just c -> do
        scid_block_height c @?= 539268
        scid_tx_index c @?= 845
        scid_output_index c @?= 1
        encode_short_channel_id c @?= "\x08\x3a\x84\x00\x03\x4d\x00\x01"
  , testCase "short_channel_id rejects 25-bit components" $ do
      short_channel_id 0x1000000 0 0 @?= Nothing
      short_channel_id 0 0x1000000 0 @?= Nothing
  , testCase "amount bounds" $ do
      fmap un_satoshi (satoshi 0x000775f05a074000) @?= Just 0x000775f05a074000
      satoshi 0x000775f05a074001 @?= Nothing
      fmap un_milli_satoshi (milli_satoshi 0x1d24b2dfac520000)
        @?= Just 0x1d24b2dfac520000
      milli_satoshi 0x1d24b2dfac520001 @?= Nothing
      sat_to_msat max_satoshi @?= max_milli_satoshi
      decode_satoshi (encode_u64 0x000775f05a074001) @?= Nothing
      decode_milli_satoshi (encode_u64 maxBound) @?= Nothing
  , testCase "checked arithmetic" $ do
      add_sat max_satoshi max_satoshi @?= Nothing
      (do a <- satoshi 1; b <- satoshi 2; sub_sat a b) @?= Nothing
      (do a <- satoshi 2; b <- satoshi 1; sub_sat a b) @?= satoshi 1
      (do a <- milli_satoshi 1; b <- milli_satoshi 2; sub_msat a b)
        @?= Nothing
      add_msat max_milli_satoshi max_milli_satoshi @?= Nothing
  , testCase "secrets don't show" $
      case payment_preimage (BS.replicate 32 7) of
        Nothing -> assertFailure "valid preimage rejected"
        Just p -> do
          show p @?= "PaymentPreimage <redacted>"
          show (Just p) @?= "Just (PaymentPreimage <redacted>)"
  , testCase "secret equality" $ do
      let a = payment_preimage (BS.replicate 32 7)
          b = payment_preimage (BS.replicate 31 7 <> "\x08")
      assertBool "equal" (a == a)
      assertBool "unequal" (a /= b)
  ]

-- TLV ------------------------------------------------------------------------

tlv :: TestTree
tlv = testGroup "TLV" [
    testCase "tlv_stream sorts" $
      fmap (map tlv_type . un_tlv_stream)
        (tlv_stream [TlvRecord 5 "", TlvRecord 1 "", TlvRecord 3 ""])
        @?= Just [1, 3, 5]
  , testCase "tlv_stream rejects duplicates" $
      tlv_stream [TlvRecord 1 "a", TlvRecord 1 "b"] @?= Nothing
  , testCase "lookup_tlv" $ do
      (lookup_tlv 3 =<< tlv_stream [TlvRecord 1 "a", TlvRecord 3 "b"])
        @?= Just "b"
      (lookup_tlv 2 =<< tlv_stream [TlvRecord 1 "a", TlvRecord 3 "b"])
        @?= Nothing
  , testCase "huge lengths are rejected, not wrapped" $
      mapM_ (\h -> with_hex h $ \bs ->
               decode_tlv_stream (const True) bs @?= Left TlvTruncated)
        [ "01ffffffffffffffffff", "01ff800000000000000067"
        , "01ffffffffffffffffff6701aa" ]
  , testCase "truncation vs non-minimal" $ do
      decode_tlv_stream (const True) "\xfd" @?= Left TlvTruncated
      decode_tlv_stream (const True) "\xfd\x00\x01\x00"
        @?= Left TlvNonMinimalBigSize
  , testCase "unknown odd records are kept" $
      fmap (map tlv_type . un_tlv_stream)
        (decode_tlv_stream (const False) "\x01\x00\x03\x01\x2a")
        @?= Right [1, 3]
  ]

-- messages -------------------------------------------------------------------

messages :: TestTree
messages = testGroup "Messages" [
    testCase "pong known answer" $
      encode_message (MsgPong (Pong "\x00\x00" empty_tlv_stream))
        @?= Right "\x00\x13\x00\x02\x00\x00"
  , testCase "ping known answer" $
      encode_message (MsgPing (Ping 4 "" empty_tlv_stream))
        @?= Right "\x00\x12\x00\x04\x00\x00"
  , testCase "error known answer" $
      encode_message (MsgError (Error all_channels "x" empty_tlv_stream))
        @?= Right ("\x00\x11" <> BS.replicate 32 0 <> "\x00\x01x")
  , testCase "init with networks and remote_addr" $
      case chain_hash (BS.replicate 32 0x6f) of
        Nothing -> assertFailure "valid chain hash rejected"
        Just ch -> do
          let addr = "\x01\x7f\x00\x00\x01\x26\x07"
              i = Init empty_fv (BOLT9.parse "\x02") (Just [ch])
                    (Just addr) empty_tlv_stream
              wire = "\x00\x10\x00\x00\x00\x01\x02"
                  <> "\x01\x20" <> un_chain_hash ch <> "\x03\x07" <> addr
          encode_message (MsgInit i) @?= Right wire
          decode_message wire @?= Right (MsgInit i)
  , testCase "init networks must be a multiple of 32 bytes" $
      decode_init "\x00\x00\x00\x00\x01\x01\x00"
        @?= Left (DecodeInvalidTlvValue 1)
  , testCase "init rejects tlvs clashing with typed fields" $
      case tlv_stream [TlvRecord 3 "x"] of
        Nothing -> assertFailure "valid stream rejected"
        Just s  -> encode_init (Init empty_fv empty_fv Nothing (Just "y") s)
          @?= Left EncodeInvalidTlvs
  , testCase "unknown message types" $ do
      decode_message "\x00\x20" @?= Left (DecodeUnknownEvenType 32)
      decode_message "\x80\x01" @?= Left (DecodeUnknownOddType 32769)
      decode_message "\x00" @?= Left DecodeInsufficientBytes
  , testCase "truncated payloads" $ do
      decode_init "\x00" @?= Left DecodeInsufficientBytes
      decode_init "\x00\x01" @?= Left DecodeInsufficientBytes
      decode_error (BS.replicate 31 0) @?= Left DecodeInsufficientBytes
      decode_warning (BS.replicate 33 0) @?= Left DecodeInsufficientBytes
      decode_ping "\x00\x04\x00\x02\x00" @?= Left DecodeInsufficientBytes
      decode_pong "\x00\x02\x00" @?= Left DecodeInsufficientBytes
      decode_peer_storage "\x00\x01" @?= Left DecodeInsufficientBytes
      decode_peer_storage_retrieval "\x00" @?= Left DecodeInsufficientBytes
  , testCase "invalid extensions" $ do
      decode_pong "\x00\x00\x02\x00" @?= Left
        (DecodeTlvError (TlvUnknownEvenType 2))
      decode_ping "\x00\x00\x00\x00\x01" @?= Left (DecodeTlvError TlvTruncated)
  , testCase "message size limit" $ do
      encode_envelope 7 (BS.replicate 65533 0) @?=
        Right (encode_u16 7 <> BS.replicate 65533 0)
      encode_envelope 7 (BS.replicate 65534 0) @?= Left EncodeMessageTooLarge
      encode_message (MsgPeerStorage (PeerStorage (BS.replicate 65532 0)
        empty_tlv_stream)) @?= Left EncodeMessageTooLarge
      encode_pong (Pong (BS.replicate 65536 0) empty_tlv_stream)
        @?= Left EncodeLengthOverflow
  , testCase "ping_response" $ do
      ping_response (Ping 3 "ab" empty_tlv_stream)
        @?= Just (Pong "\x00\x00\x00" empty_tlv_stream)
      ping_response (Ping 65531 "" empty_tlv_stream) @?=
        Just (Pong (BS.replicate 65531 0) empty_tlv_stream)
      ping_response (Ping 65532 "" empty_tlv_stream) @?= Nothing
  ]

-- properties -----------------------------------------------------------------

newtype Bytes = Bytes BS.ByteString
  deriving Show

instance Arbitrary Bytes where
  arbitrary = Bytes . BS.pack <$> (choose (0, 64) >>= vector)

-- streams of unknown odd records
newtype OddTlvs = OddTlvs TlvStream
  deriving Show

instance Arbitrary OddTlvs where
  arbitrary = do
    ts <- L.nub . map (\t -> 2 * (t `div` 2) + 1)
            <$> listOf (arbitrary :: Gen Word64)
    vs <- vectorOf (length ts) arbitrary
    pure . OddTlvs . fromMaybe empty_tlv_stream $
      tlv_stream [ TlvRecord t v | (t, Bytes v) <- zip ts vs ]

newtype Bytes32 = Bytes32 BS.ByteString
  deriving Show

instance Arbitrary Bytes32 where
  arbitrary = Bytes32 . BS.pack <$> vector 32

newtype Msg = Msg Message
  deriving Show

instance Arbitrary Msg where
  arbitrary = Msg <$> oneof [
      do Bytes a <- arbitrary
         Bytes b <- arbitrary
         nets <- oneof [pure Nothing, Just <$> listOf chain]
         addr <- oneof [pure Nothing, (\(Bytes x) -> Just x) <$> arbitrary]
         OddTlvs s <- arbitrary
         let s' = filter_tlv_stream (\t -> t /= 1 && t /= 3) s
         pure (MsgInit (Init (BOLT9.parse a) (BOLT9.parse b) nets addr s'))
    , do c <- cid
         Bytes d <- arbitrary
         OddTlvs s <- arbitrary
         pure (MsgError (Error c d s))
    , do c <- cid
         Bytes d <- arbitrary
         OddTlvs s <- arbitrary
         pure (MsgWarning (Warning c d s))
    , do n <- arbitrary
         Bytes d <- arbitrary
         OddTlvs s <- arbitrary
         pure (MsgPing (Ping n d s))
    , do Bytes d <- arbitrary
         OddTlvs s <- arbitrary
         pure (MsgPong (Pong d s))
    , do Bytes d <- arbitrary
         OddTlvs s <- arbitrary
         pure (MsgPeerStorage (PeerStorage d s))
    , do Bytes d <- arbitrary
         OddTlvs s <- arbitrary
         pure (MsgPeerStorageRetrieval (PeerStorageRetrieval d s))
    ]
    where
      chain = do
        Bytes32 b <- arbitrary
        maybe chain pure (chain_hash b)
      cid = do
        Bytes32 b <- arbitrary
        pure (fromMaybe all_channels (channel_id b))

properties :: TestTree
properties = testGroup "Properties" [
    testProperty "bigsize round-trips" $ \w ->
      decode_bigsize (encode_bigsize w) === Just (w, "")
  , testProperty "tu16 round-trips" $ \w ->
      decode_tu16 (encode_tu16 w) === Just w
  , testProperty "tu32 round-trips" $ \w ->
      decode_tu32 (encode_tu32 w) === Just w
  , testProperty "tu64 round-trips" $ \w ->
      decode_tu64 (encode_tu64 w) === Just w
  , testProperty "u16 round-trips" $ \w ->
      decode_u16 (encode_u16 w) === Just (w, "")
  , testProperty "u32 round-trips" $ \w ->
      decode_u32 (encode_u32 w) === Just (w, "")
  , testProperty "u64 round-trips" $ \w ->
      decode_u64 (encode_u64 w) === Just (w, "")
  , testProperty "s64 round-trips" $ \w ->
      decode_s64 (encode_s64 w) === Just (w, "")
  , testProperty "scid components round-trip" $ \h t o ->
      let h' = h `mod` 0x1000000 :: Word32
          t' = t `mod` 0x1000000 :: Word32
      in  fmap (\c -> (scid_block_height c, scid_tx_index c,
                       scid_output_index c)) (short_channel_id h' t' o)
            === Just (h', t', o)
  , testProperty "sat -> msat -> sat is the identity" $
      forAll (choose (0, 0x000775f05a074000)) $ \w ->
        fmap (msat_to_sat . sat_to_msat) (satoshi w) === satoshi w
  , testProperty "add then sub is the identity" $
      forAll (choose (0, 0x000775f05a074000)) $ \a ->
      forAll (choose (0, 0x000775f05a074000 - a)) $ \b ->
        (do x <- satoshi a
            y <- satoshi b
            z <- add_sat x y
            sub_sat z y) === satoshi a
  , testProperty "tlv streams round-trip" $ \(OddTlvs s) ->
      decode_tlv_stream (const False) (encode_tlv_stream s) === Right s
  , testProperty "messages round-trip" $ \(Msg m) ->
      case encode_message m of
        Left e   -> counterexample (show e) False
        Right bs -> decode_message bs === Right m
  , testProperty "re-encoding reproduces the wire bytes" $ \(Msg m) ->
      case encode_message m of
        Left e   -> counterexample (show e) False
        Right bs -> case decode_message bs of
          Left e   -> counterexample (show e) False
          Right m' -> encode_message m' === Right bs
  ]
