{-# LANGUAGE OverloadedStrings #-}

module Fixtures where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Lightning.Protocol.BOLT1
import qualified Lightning.Protocol.BOLT9 as BOLT9

{-# NOINLINE tlvs #-}
tlvs :: TlvStream
tlvs = fromMaybe empty_tlv_stream $
  tlv_stream [ TlvRecord t (BS.replicate 32 0x2a) | t <- [1, 3 .. 41] ]

{-# NOINLINE tlvs_bytes #-}
tlvs_bytes :: BS.ByteString
tlvs_bytes = encode_tlv_stream tlvs

{-# NOINLINE init_msg #-}
init_msg :: Message
init_msg = MsgInit Init {
    init_global_features = BOLT9.parse ""
  , init_features        = BOLT9.parse "\x02\xaa\x52\x69\xa1"
  , init_networks        = fmap pure (chain_hash (BS.replicate 32 0x6f))
  , init_remote_addr     = Just "\x01\x7f\x00\x00\x01\x26\x07"
  , init_tlvs            = empty_tlv_stream
  }

{-# NOINLINE ping_msg #-}
ping_msg :: Message
ping_msg = MsgPing (Ping 128 (BS.replicate 64 0) empty_tlv_stream)

{-# NOINLINE error_msg #-}
error_msg :: Message
error_msg = MsgError (Error all_channels (BS.replicate 128 0x61) tlvs)

wire :: Message -> BS.ByteString
wire m = either (const BS.empty) id (encode_message m)

{-# NOINLINE init_bytes #-}
init_bytes :: BS.ByteString
init_bytes = wire init_msg

{-# NOINLINE ping_bytes #-}
ping_bytes :: BS.ByteString
ping_bytes = wire ping_msg

{-# NOINLINE error_bytes #-}
error_bytes :: BS.ByteString
error_bytes = wire error_msg
