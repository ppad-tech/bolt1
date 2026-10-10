{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT1.Message
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- The messages defined by BOLT #1.

module Lightning.Protocol.BOLT1.Message (
    Init(..)
  , Error(..)
  , Warning(..)
  , Ping(..)
  , Pong(..)
  , PeerStorage(..)
  , PeerStorageRetrieval(..)
  , Message(..)
  , message_type
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.Prim
import Lightning.Protocol.BOLT1.TLV
import qualified Lightning.Protocol.BOLT9 as BOLT9

-- | The @init@ message (type 16).
--
--   The two feature vectors are kept as received. Per BOLT #1, a
--   receiver treats their union ('BOLT9.union') as the peer's
--   features.
data Init = Init
  { init_global_features :: !BOLT9.FeatureVector
  , init_features        :: !BOLT9.FeatureVector
  , init_networks        :: !(Maybe [ChainHash])
    -- ^ @networks@ (TLV type 1): chains the node is interested in
  , init_remote_addr     :: !(Maybe BS.ByteString)
    -- ^ @remote_addr@ (TLV type 3): a raw BOLT #7 address descriptor
  , init_tlvs            :: !TlvStream
    -- ^ unknown odd TLV records
  } deriving (Eq, Show, Generic)

instance NFData Init

-- | The @error@ message (type 17).
data Error = Error
  { error_channel_id :: !ChannelId
  , error_data       :: !BS.ByteString
  , error_tlvs       :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData Error

-- | The @warning@ message (type 1).
data Warning = Warning
  { warning_channel_id :: !ChannelId
  , warning_data       :: !BS.ByteString
  , warning_tlvs       :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData Warning

-- | The @ping@ message (type 18).
data Ping = Ping
  { ping_num_pong_bytes :: {-# UNPACK #-} !Word16
  , ping_ignored        :: !BS.ByteString
  , ping_tlvs           :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData Ping

-- | The @pong@ message (type 19).
data Pong = Pong
  { pong_ignored :: !BS.ByteString
  , pong_tlvs    :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData Pong

-- | The @peer_storage@ message (type 7).
data PeerStorage = PeerStorage
  { peer_storage_blob :: !BS.ByteString
  , peer_storage_tlvs :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData PeerStorage

-- | The @peer_storage_retrieval@ message (type 9).
data PeerStorageRetrieval = PeerStorageRetrieval
  { peer_storage_retrieval_blob :: !BS.ByteString
  , peer_storage_retrieval_tlvs :: !TlvStream
    -- ^ extension TLV records (all unknown odd)
  } deriving (Eq, Show, Generic)

instance NFData PeerStorageRetrieval

-- | A BOLT #1 message.
data Message
  = MsgInit !Init
  | MsgError !Error
  | MsgWarning !Warning
  | MsgPing !Ping
  | MsgPong !Pong
  | MsgPeerStorage !PeerStorage
  | MsgPeerStorageRetrieval !PeerStorageRetrieval
  deriving (Eq, Show, Generic)

instance NFData Message

-- | The wire type of a 'Message'.
--
--   >>> message_type (MsgPong (Pong "" empty_tlv_stream))
--   19
message_type :: Message -> Word16
message_type m = case m of
  MsgInit {}                 -> 16
  MsgError {}                -> 17
  MsgWarning {}              -> 1
  MsgPing {}                 -> 18
  MsgPong {}                 -> 19
  MsgPeerStorage {}          -> 7
  MsgPeerStorageRetrieval {} -> 9
