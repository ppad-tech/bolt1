{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Fixtures
import Lightning.Protocol.BOLT1

main :: IO ()
main = defaultMain [
    bgroup "primitives" [
      bench "encode_u64" $ nf encode_u64 0x0102030405060708
    , bench "decode_u64" $ nf decode_u64 "\x01\x02\x03\x04\x05\x06\x07\x08"
    , bench "encode_tu64" $ nf encode_tu64 0x010203
    , bench "decode_tu64" $ nf decode_tu64 "\x01\x02\x03"
    , bench "encode_bigsize (9 bytes)" $ nf encode_bigsize 0x100000000
    , bench "decode_bigsize (9 bytes)" $
        nf decode_bigsize "\xff\x00\x00\x00\x01\x00\x00\x00\x00"
    ]
  , bgroup "tlv" [
      bench "encode_tlv_stream (21 records)" $ nf encode_tlv_stream tlvs
    , bench "decode_tlv_stream (21 records)" $
        nf (decode_tlv_stream (const False)) tlvs_bytes
    ]
  , bgroup "messages" [
      bench "encode init" $ nf encode_message init_msg
    , bench "decode init" $ nf decode_message init_bytes
    , bench "encode ping" $ nf encode_message ping_msg
    , bench "decode ping" $ nf decode_message ping_bytes
    , bench "encode error (with tlvs)" $ nf encode_message error_msg
    , bench "decode error (with tlvs)" $ nf decode_message error_bytes
    ]
  ]
