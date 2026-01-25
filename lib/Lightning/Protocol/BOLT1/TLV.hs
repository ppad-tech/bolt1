{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module: Lightning.Protocol.BOLT1.TLV
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- TLV (Type-Length-Value) format for BOLT #1.

module Lightning.Protocol.BOLT1.TLV (
  -- * TLV types
    TlvRecord(..)
  , TlvStream
  , unTlvStream
  , tlvStream
  , unsafeTlvStream
  , TlvError(..)

  -- * TLV encoding
  , encodeTlvRecord
  , encodeTlvStream

  -- * TLV decoding
  , decodeTlvStream
  , decodeTlvStreamWith
  , decodeTlvStreamRaw

  -- * Init TLV types
  , InitTlv(..)
  , parseInitTlvs
  , encodeInitTlvs
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.Prim

-- TLV types -------------------------------------------------------------------

-- | A single TLV record.
data TlvRecord = TlvRecord
  { tlvType   :: {-# UNPACK #-} !Word64
  , tlvValue  :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData TlvRecord

-- | A TLV stream (series of TLV records).
newtype TlvStream = TlvStream { unTlvStream :: [TlvRecord] }
  deriving stock (Eq, Show, Generic)

instance NFData TlvStream

-- | Smart constructor for 'TlvStream' that validates records are
-- strictly increasing by type.
--
-- Returns 'Nothing' if types are not strictly increasing.
tlvStream :: [TlvRecord] -> Maybe TlvStream
tlvStream recs
  | isStrictlyIncreasing (map tlvType recs) = Just (TlvStream recs)
  | otherwise = Nothing
  where
    isStrictlyIncreasing :: [Word64] -> Bool
    isStrictlyIncreasing [] = True
    isStrictlyIncreasing [_] = True
    isStrictlyIncreasing (x:y:rest) = x < y && isStrictlyIncreasing (y:rest)

-- | Unsafe constructor for 'TlvStream' that skips validation.
--
-- Use only when ordering is already guaranteed (e.g., in decode functions).
unsafeTlvStream :: [TlvRecord] -> TlvStream
unsafeTlvStream = TlvStream

-- | TLV decoding errors.
data TlvError
  = TlvNonMinimalEncoding
  | TlvNotStrictlyIncreasing
  | TlvLengthExceedsBounds
  | TlvUnknownEvenType !Word64
  | TlvInvalidKnownType !Word64
  deriving stock (Eq, Show, Generic)

instance NFData TlvError

-- TLV encoding ----------------------------------------------------------------

-- | Encode a TLV record.
encodeTlvRecord :: TlvRecord -> BS.ByteString
encodeTlvRecord (TlvRecord typ val) = mconcat
  [ encodeBigSize typ
  , encodeBigSize (fromIntegral (BS.length val))
  , val
  ]

-- | Encode a TLV stream.
encodeTlvStream :: TlvStream -> BS.ByteString
encodeTlvStream (TlvStream recs) = mconcat (map encodeTlvRecord recs)

-- TLV decoding ----------------------------------------------------------------

-- | Decode a TLV stream without any known-type validation.
--
-- This decoder only enforces structural validity:
-- - Types must be strictly increasing
-- - Lengths must not exceed bounds
--
-- All records are returned regardless of type. Note: this does NOT
-- enforce the BOLT #1 unknown-even-type rule. Use 'decodeTlvStreamWith'
-- with an appropriate predicate for spec-compliant parsing.
decodeTlvStreamRaw :: BS.ByteString -> Either TlvError TlvStream
decodeTlvStreamRaw = go Nothing []
  where
    go :: Maybe Word64 -> [TlvRecord] -> BS.ByteString
       -> Either TlvError TlvStream
    go !_ !acc !bs
      | BS.null bs = Right (unsafeTlvStream (reverse acc))
    go !mPrevType !acc !bs = do
      (typ, rest1) <- maybe (Left TlvNonMinimalEncoding) Right
                        (decodeBigSize bs)
      -- Strictly increasing check
      case mPrevType of
        Just prevType -> when (typ <= prevType) $
          Left TlvNotStrictlyIncreasing
        Nothing -> pure ()
      (len, rest2) <- maybe (Left TlvNonMinimalEncoding) Right
                        (decodeBigSize rest1)
      -- Length bounds check
      when (fromIntegral len > BS.length rest2) $
        Left TlvLengthExceedsBounds
      let !val = BS.take (fromIntegral len) rest2
          !rest3 = BS.drop (fromIntegral len) rest2
          !rec = TlvRecord typ val
      go (Just typ) (rec : acc) rest3

-- | Decode a TLV stream with configurable known-type predicate.
--
-- Per BOLT #1:
-- - Types must be strictly increasing
-- - Unknown even types cause failure
-- - Unknown odd types are skipped
--
-- The predicate determines which types are "known" for the context.
decodeTlvStreamWith
  :: (Word64 -> Bool)  -- ^ Predicate: is this type known?
  -> BS.ByteString
  -> Either TlvError TlvStream
decodeTlvStreamWith isKnown = go Nothing []
  where
    go :: Maybe Word64 -> [TlvRecord] -> BS.ByteString
       -> Either TlvError TlvStream
    go !_ !acc !bs
      | BS.null bs = Right (unsafeTlvStream (reverse acc))
    go !mPrevType !acc !bs = do
      (typ, rest1) <- maybe (Left TlvNonMinimalEncoding) Right
                        (decodeBigSize bs)
      -- Strictly increasing check
      case mPrevType of
        Just prevType -> when (typ <= prevType) $
          Left TlvNotStrictlyIncreasing
        Nothing -> pure ()
      (len, rest2) <- maybe (Left TlvNonMinimalEncoding) Right
                        (decodeBigSize rest1)
      -- Length bounds check
      when (fromIntegral len > BS.length rest2) $
        Left TlvLengthExceedsBounds
      let !val = BS.take (fromIntegral len) rest2
          !rest3 = BS.drop (fromIntegral len) rest2
          !rec = TlvRecord typ val
      -- Unknown type handling: even = fail, odd = skip
      if isKnown typ
        then go (Just typ) (rec : acc) rest3
        else if even typ
          then Left (TlvUnknownEvenType typ)
          else go (Just typ) acc rest3  -- skip unknown odd

-- | Decode a TLV stream with BOLT #1 init_tlvs validation.
--
-- This uses the default known types for init messages (1 and 3).
-- For other contexts, use 'decodeTlvStreamWith' with an appropriate
-- predicate.
decodeTlvStream :: BS.ByteString -> Either TlvError TlvStream
decodeTlvStream = decodeTlvStreamWith isInitTlvType
  where
    isInitTlvType :: Word64 -> Bool
    isInitTlvType 1 = True  -- networks
    isInitTlvType 3 = True  -- remote_addr
    isInitTlvType _ = False

-- Init TLV types --------------------------------------------------------------

-- | TLV records for init message.
data InitTlv
  = InitNetworks ![BS.ByteString]  -- ^ Type 1: chain hashes (32 bytes each)
  | InitRemoteAddr !BS.ByteString  -- ^ Type 3: remote address
  deriving stock (Eq, Show, Generic)

instance NFData InitTlv

-- | Parse init TLVs from a TLV stream.
parseInitTlvs :: TlvStream -> Either TlvError [InitTlv]
parseInitTlvs (TlvStream recs) = traverse parseOne recs
  where
    parseOne (TlvRecord 1 val)
      | BS.length val `mod` 32 == 0 =
          Right (InitNetworks (chunksOf 32 val))
      | otherwise = Left (TlvInvalidKnownType 1)
    parseOne (TlvRecord 3 val) = Right (InitRemoteAddr val)
    parseOne (TlvRecord t _) = Left (TlvUnknownEvenType t)

-- | Split bytestring into chunks of given size.
chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf !n !bs
  | BS.null bs = []
  | otherwise =
      let (!chunk, !rest) = BS.splitAt n bs
      in  chunk : chunksOf n rest

-- | Encode init TLVs to a TLV stream.
encodeInitTlvs :: [InitTlv] -> TlvStream
encodeInitTlvs = unsafeTlvStream . map toRecord
  where
    toRecord (InitNetworks chains) =
      TlvRecord 1 (mconcat chains)
    toRecord (InitRemoteAddr addr) =
      TlvRecord 3 addr
