{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module: Lightning.Protocol.BOLT1.Internal
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Internal definitions for BOLT #1.
--
-- This module exports unsafe constructors that bypass
-- validation. Use only in tests or trusted internal code.

module Lightning.Protocol.BOLT1.Internal (
  -- * Unsafe constructors
    unsafeMsgUnknown
  , unsafeEnvelope
  , unsafeChainHash
  , unsafeChannelId
  ) where

import qualified Data.ByteString as BS
import Data.Word (Word16)
import Lightning.Protocol.BOLT1.Message
import Lightning.Protocol.BOLT1.Prim
import Lightning.Protocol.BOLT1.TLV

-- | Construct a 'MsgUnknown' without validation.
--
-- This bypasses the check that prevents wrapping known
-- type codes. For test use only.
unsafeMsgUnknown :: Word16 -> MsgType
unsafeMsgUnknown = MsgUnknown

-- | Construct an 'Envelope' without validation.
--
-- This bypasses the check that 'envType' matches the
-- message. For test use only.
unsafeEnvelope
  :: MsgType
  -> BS.ByteString
  -> Maybe TlvStream
  -> Envelope
unsafeEnvelope = Envelope

-- | Construct a 'ChainHash' without length validation.
--
-- For test use only.
unsafeChainHash :: BS.ByteString -> ChainHash
unsafeChainHash = ChainHash

-- | Construct a 'ChannelId' without length validation.
--
-- For test use only.
unsafeChannelId :: BS.ByteString -> ChannelId
unsafeChannelId = ChannelId
