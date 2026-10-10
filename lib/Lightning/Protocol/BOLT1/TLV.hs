{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT1.TLV
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- The TLV (type-length-value) format of BOLT #1.

module Lightning.Protocol.BOLT1.TLV (
    TlvRecord(..)
  , TlvStream
  , tlv_stream
  , un_tlv_stream
  , empty_tlv_stream
  , lookup_tlv
  , filter_tlv_stream
  , TlvError(..)
  , encode_tlv_stream
  , decode_tlv_stream
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import qualified Data.List as L
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.Prim

-- | A single TLV record.
data TlvRecord = TlvRecord
  { tlv_type  :: {-# UNPACK #-} !Word64
  , tlv_value :: !BS.ByteString
  } deriving (Eq, Show, Generic)

instance NFData TlvRecord

-- | A TLV stream: records in strictly increasing type order.
newtype TlvStream = TlvStream [TlvRecord]
  deriving (Eq, Show, Generic)

instance NFData TlvStream

-- | Construct a 'TlvStream' from records in any order. Fails if two
--   records share a type.
--
--   >>> let Just s = tlv_stream [TlvRecord 3 "b", TlvRecord 1 "a"]
--   >>> map tlv_type (un_tlv_stream s)
--   [1,3]
--   >>> tlv_stream [TlvRecord 1 "a", TlvRecord 1 "b"]
--   Nothing
tlv_stream :: [TlvRecord] -> Maybe TlvStream
tlv_stream rs
  | increasing sorted = Just (TlvStream sorted)
  | otherwise         = Nothing
  where
    sorted = L.sortOn tlv_type rs

increasing :: [TlvRecord] -> Bool
increasing (a:rest@(b:_)) = tlv_type a < tlv_type b && increasing rest
increasing _ = True

-- | The records of a 'TlvStream', in increasing type order.
un_tlv_stream :: TlvStream -> [TlvRecord]
un_tlv_stream (TlvStream rs) = rs
{-# INLINE un_tlv_stream #-}

-- | The empty 'TlvStream'.
empty_tlv_stream :: TlvStream
empty_tlv_stream = TlvStream []

-- | The value of the record with the given type, if present.
--
--   >>> lookup_tlv 3 =<< tlv_stream [TlvRecord 1 "a", TlvRecord 3 "b"]
--   Just "b"
lookup_tlv :: Word64 -> TlvStream -> Maybe BS.ByteString
lookup_tlv t (TlvStream rs) = go rs
  where
    go [] = Nothing
    go (TlvRecord u v : more)
      | u == t    = Just v
      | u > t     = Nothing
      | otherwise = go more

-- | Keep the records whose types satisfy the predicate.
--
--   >>> let Just s = tlv_stream [TlvRecord 1 "", TlvRecord 2 ""]
--   >>> map tlv_type (un_tlv_stream (filter_tlv_stream odd s))
--   [1]
filter_tlv_stream :: (Word64 -> Bool) -> TlvStream -> TlvStream
filter_tlv_stream p (TlvStream rs) = TlvStream (filter (p . tlv_type) rs)

-- | Why a TLV stream failed to decode.
data TlvError
  = TlvTruncated               -- ^ a type, length or value was cut off
  | TlvNonMinimalBigSize       -- ^ a type or length wasn't minimal
  | TlvNotStrictlyIncreasing   -- ^ types out of order or repeated
  | TlvUnknownEvenType !Word64 -- ^ an even type the context doesn't know
  deriving (Eq, Show, Generic)

instance NFData TlvError

-- | Encode a 'TlvStream'.
--
--   >>> fmap encode_tlv_stream (tlv_stream [TlvRecord 1 "a"])
--   Just "\SOH\SOHa"
encode_tlv_stream :: TlvStream -> BS.ByteString
encode_tlv_stream (TlvStream rs) = mconcat (concatMap enc rs)
  where
    enc (TlvRecord t v) =
      [encode_bigsize t, encode_bigsize (fromIntegral (BS.length v)), v]

-- | Decode a TLV stream occupying the whole input, given a predicate
--   identifying the types known in the stream's context.
--
--   Per BOLT #1, decoding fails on truncation, non-minimal BigSize
--   encodings, types that are not strictly increasing, and unknown
--   even types. Every record is kept, including unknown odd ones, so
--   re-encoding reproduces the input.
--
--   >>> let Right s = decode_tlv_stream (== 1) "\SOH\SOHa\ETX\NUL"
--   >>> map tlv_type (un_tlv_stream s)
--   [1,3]
--   >>> decode_tlv_stream (== 1) "\STX\NUL"
--   Left (TlvUnknownEvenType 2)
decode_tlv_stream
  :: (Word64 -> Bool)  -- ^ is this type known?
  -> BS.ByteString
  -> Either TlvError TlvStream
decode_tlv_stream known = go Nothing []
  where
    go !_ !acc !bs | BS.null bs = Right (TlvStream (reverse acc))
    go !prev !acc !bs = do
      (t, r0) <- bigsize bs
      case prev of
        Just p | t <= p -> Left TlvNotStrictlyIncreasing
        _ -> pure ()
      (l, r1) <- bigsize r0
      if l > fromIntegral (BS.length r1)
        then Left TlvTruncated
        else do
          let !n = fromIntegral l
              !v = BU.unsafeTake n r1
              !r2 = BU.unsafeDrop n r1
          if not (known t) && even t
            then Left (TlvUnknownEvenType t)
            else go (Just t) (TlvRecord t v : acc) r2

    bigsize b = case decode_bigsize_detailed b of
      Right r                -> Right r
      Left BigSizeTruncated  -> Left TlvTruncated
      Left BigSizeNonMinimal -> Left TlvNonMinimalBigSize
