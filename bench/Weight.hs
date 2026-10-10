{-# LANGUAGE OverloadedStrings #-}

module Main where

import Fixtures
import Lightning.Protocol.BOLT1
import Weigh

main :: IO ()
main = mainWith $ do
  wgroup "primitives" $ do
    func "baseline (weigh overhead)" (+ (1 :: Int)) 1
    func "encode_u16" encode_u16 0x0102
    func "encode_u64" encode_u64 0x0102030405060708
    func "decode_u64" decode_u64 "\x01\x02\x03\x04\x05\x06\x07\x08"
    func "encode_tu64" encode_tu64 0x010203
    func "encode_bigsize (9 bytes)" encode_bigsize 0x100000000
  wgroup "tlv" $ do
    func "encode_tlv_stream (21 records)" encode_tlv_stream tlvs
    func "decode_tlv_stream (21 records)"
      (decode_tlv_stream (const False)) tlvs_bytes
  wgroup "messages" $ do
    func "encode init" encode_message init_msg
    func "decode init" decode_message init_bytes
    func "encode ping" encode_message ping_msg
    func "decode ping" decode_message ping_bytes
    func "encode error (with tlvs)" encode_message error_msg
    func "decode error (with tlvs)" decode_message error_bytes
