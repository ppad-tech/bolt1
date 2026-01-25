{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module: Lightning.Protocol.BOLT1.Message
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Message types for BOLT #1.

module Lightning.Protocol.BOLT1.Message (
  -- * Message types
    MsgType(..)
  , msgTypeWord
  , parseMsgType

  -- * Setup messages
  , Init(..)
  , Error(..)
  , Warning(..)

  -- * Control messages
  , Ping(..)
  , Pong(..)

  -- * Peer storage messages
  , PeerStorage(..)
  , PeerStorageRetrieval(..)

  -- * Message envelope
  , Message(..)
  , messageType
  , Envelope(..)
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.TLV

-- Message types ---------------------------------------------------------------

-- | BOLT #1 message type codes.
data MsgType
  = MsgInit              -- ^ 16
  | MsgError             -- ^ 17
  | MsgPing              -- ^ 18
  | MsgPong              -- ^ 19
  | MsgWarning           -- ^ 1
  | MsgPeerStorage       -- ^ 7
  | MsgPeerStorageRet    -- ^ 9
  | MsgUnknown !Word16   -- ^ Unknown type
  deriving stock (Eq, Show, Generic)

instance NFData MsgType

-- | Get the numeric type code for a message type.
msgTypeWord :: MsgType -> Word16
msgTypeWord MsgInit            = 16
msgTypeWord MsgError           = 17
msgTypeWord MsgPing            = 18
msgTypeWord MsgPong            = 19
msgTypeWord MsgWarning         = 1
msgTypeWord MsgPeerStorage     = 7
msgTypeWord MsgPeerStorageRet  = 9
msgTypeWord (MsgUnknown w)     = w

-- | Parse a message type from a word.
parseMsgType :: Word16 -> MsgType
parseMsgType 16 = MsgInit
parseMsgType 17 = MsgError
parseMsgType 18 = MsgPing
parseMsgType 19 = MsgPong
parseMsgType 1  = MsgWarning
parseMsgType 7  = MsgPeerStorage
parseMsgType 9  = MsgPeerStorageRet
parseMsgType w  = MsgUnknown w

-- Message ADTs ----------------------------------------------------------------

-- | The init message (type 16).
data Init = Init
  { initGlobalFeatures :: !BS.ByteString
  , initFeatures       :: !BS.ByteString
  , initTlvs           :: ![InitTlv]
  } deriving stock (Eq, Show, Generic)

instance NFData Init

-- | The error message (type 17).
data Error = Error
  { errorChannelId :: !BS.ByteString  -- ^ 32 bytes
  , errorData      :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Error

-- | The warning message (type 1).
data Warning = Warning
  { warningChannelId :: !BS.ByteString  -- ^ 32 bytes
  , warningData      :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Warning

-- | The ping message (type 18).
data Ping = Ping
  { pingNumPongBytes :: {-# UNPACK #-} !Word16
  , pingIgnored      :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Ping

-- | The pong message (type 19).
data Pong = Pong
  { pongIgnored :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Pong

-- | The peer_storage message (type 7).
data PeerStorage = PeerStorage
  { peerStorageBlob :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData PeerStorage

-- | The peer_storage_retrieval message (type 9).
data PeerStorageRetrieval = PeerStorageRetrieval
  { peerStorageRetrievalBlob :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData PeerStorageRetrieval

-- | All BOLT #1 messages.
data Message
  = MsgInitVal !Init
  | MsgErrorVal !Error
  | MsgWarningVal !Warning
  | MsgPingVal !Ping
  | MsgPongVal !Pong
  | MsgPeerStorageVal !PeerStorage
  | MsgPeerStorageRetrievalVal !PeerStorageRetrieval
  deriving stock (Eq, Show, Generic)

instance NFData Message

-- | Get the message type for a message.
messageType :: Message -> MsgType
messageType (MsgInitVal _)                 = MsgInit
messageType (MsgErrorVal _)                = MsgError
messageType (MsgWarningVal _)              = MsgWarning
messageType (MsgPingVal _)                 = MsgPing
messageType (MsgPongVal _)                 = MsgPong
messageType (MsgPeerStorageVal _)          = MsgPeerStorage
messageType (MsgPeerStorageRetrievalVal _) = MsgPeerStorageRet

-- Message envelope ------------------------------------------------------------

-- | A complete message envelope with type, payload, and optional extension.
data Envelope = Envelope
  { envType      :: !MsgType
  , envPayload   :: !BS.ByteString
  , envExtension :: !(Maybe TlvStream)
  } deriving stock (Eq, Show, Generic)

instance NFData Envelope
