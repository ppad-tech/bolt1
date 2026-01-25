{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT1.Prim
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Primitive type encoding and decoding for BOLT #1.

module Lightning.Protocol.BOLT1.Prim (
  -- * Unsigned integer encoding
    encodeU16
  , encodeU32
  , encodeU64

  -- * Signed integer encoding
  , encodeS8
  , encodeS16
  , encodeS32
  , encodeS64

  -- * Truncated unsigned integer encoding
  , encodeTu16
  , encodeTu32
  , encodeTu64

  -- * Minimal signed integer encoding
  , encodeMinSigned

  -- * BigSize encoding
  , encodeBigSize

  -- * Unsigned integer decoding
  , decodeU16
  , decodeU32
  , decodeU64

  -- * Signed integer decoding
  , decodeS8
  , decodeS16
  , decodeS32
  , decodeS64

  -- * Truncated unsigned integer decoding
  , decodeTu16
  , decodeTu32
  , decodeTu64

  -- * Minimal signed integer decoding
  , decodeMinSigned

  -- * BigSize decoding
  , decodeBigSize

  -- * Internal helpers
  , encodeLength
  ) where

import Data.Bits (unsafeShiftL, unsafeShiftR, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32, Word64)

-- Unsigned integer encoding ---------------------------------------------------

-- | Encode a 16-bit unsigned integer (big-endian).
--
-- >>> encodeU16 0x0102
-- "\SOH\STX"
encodeU16 :: Word16 -> BS.ByteString
encodeU16 = BSL.toStrict . BSB.toLazyByteString . BSB.word16BE
{-# INLINE encodeU16 #-}

-- | Encode a 32-bit unsigned integer (big-endian).
--
-- >>> encodeU32 0x01020304
-- "\SOH\STX\ETX\EOT"
encodeU32 :: Word32 -> BS.ByteString
encodeU32 = BSL.toStrict . BSB.toLazyByteString . BSB.word32BE
{-# INLINE encodeU32 #-}

-- | Encode a 64-bit unsigned integer (big-endian).
--
-- >>> encodeU64 0x0102030405060708
-- "\SOH\STX\ETX\EOT\ENQ\ACK\a\b"
encodeU64 :: Word64 -> BS.ByteString
encodeU64 = BSL.toStrict . BSB.toLazyByteString . BSB.word64BE
{-# INLINE encodeU64 #-}

-- Signed integer encoding -----------------------------------------------------

-- | Encode an 8-bit signed integer.
--
-- >>> encodeS8 42
-- "*"
-- >>> encodeS8 (-42)
-- "\214"
encodeS8 :: Int8 -> BS.ByteString
encodeS8 = BS.singleton . fromIntegral
{-# INLINE encodeS8 #-}

-- | Encode a 16-bit signed integer (big-endian two's complement).
--
-- >>> encodeS16 0x0102
-- "\SOH\STX"
-- >>> encodeS16 (-1)
-- "\255\255"
encodeS16 :: Int16 -> BS.ByteString
encodeS16 = BSL.toStrict . BSB.toLazyByteString . BSB.int16BE
{-# INLINE encodeS16 #-}

-- | Encode a 32-bit signed integer (big-endian two's complement).
--
-- >>> encodeS32 0x01020304
-- "\SOH\STX\ETX\EOT"
-- >>> encodeS32 (-1)
-- "\255\255\255\255"
encodeS32 :: Int32 -> BS.ByteString
encodeS32 = BSL.toStrict . BSB.toLazyByteString . BSB.int32BE
{-# INLINE encodeS32 #-}

-- | Encode a 64-bit signed integer (big-endian two's complement).
--
-- >>> encodeS64 0x0102030405060708
-- "\SOH\STX\ETX\EOT\ENQ\ACK\a\b"
-- >>> encodeS64 (-1)
-- "\255\255\255\255\255\255\255\255"
encodeS64 :: Int64 -> BS.ByteString
encodeS64 = BSL.toStrict . BSB.toLazyByteString . BSB.int64BE
{-# INLINE encodeS64 #-}

-- Truncated unsigned integer encoding -----------------------------------------

-- | Encode a truncated 16-bit unsigned integer (0-2 bytes).
--
-- Leading zeros are omitted per BOLT #1. Zero encodes to empty.
--
-- >>> encodeTu16 0
-- ""
-- >>> encodeTu16 1
-- "\SOH"
-- >>> encodeTu16 256
-- "\SOH\NUL"
encodeTu16 :: Word16 -> BS.ByteString
encodeTu16 0 = BS.empty
encodeTu16 !x
  | x < 0x100 = BS.singleton (fromIntegral x)
  | otherwise = encodeU16 x
{-# INLINE encodeTu16 #-}

-- | Encode a truncated 32-bit unsigned integer (0-4 bytes).
--
-- Leading zeros are omitted per BOLT #1. Zero encodes to empty.
--
-- >>> encodeTu32 0
-- ""
-- >>> encodeTu32 1
-- "\SOH"
-- >>> encodeTu32 0x010000
-- "\SOH\NUL\NUL"
encodeTu32 :: Word32 -> BS.ByteString
encodeTu32 0 = BS.empty
encodeTu32 !x
  | x < 0x100       = BS.singleton (fromIntegral x)
  | x < 0x10000     = encodeU16 (fromIntegral x)
  | x < 0x1000000   = BS.pack [ fromIntegral (x `unsafeShiftR` 16)
                              , fromIntegral (x `unsafeShiftR` 8)
                              , fromIntegral x
                              ]
  | otherwise       = encodeU32 x
{-# INLINE encodeTu32 #-}

-- | Encode a truncated 64-bit unsigned integer (0-8 bytes).
--
-- Leading zeros are omitted per BOLT #1. Zero encodes to empty.
--
-- >>> encodeTu64 0
-- ""
-- >>> encodeTu64 1
-- "\SOH"
-- >>> encodeTu64 0x0100000000
-- "\SOH\NUL\NUL\NUL\NUL"
encodeTu64 :: Word64 -> BS.ByteString
encodeTu64 0 = BS.empty
encodeTu64 !x
  | x < 0x100             = BS.singleton (fromIntegral x)
  | x < 0x10000           = encodeU16 (fromIntegral x)
  | x < 0x1000000         = BS.pack [ fromIntegral (x `unsafeShiftR` 16)
                                    , fromIntegral (x `unsafeShiftR` 8)
                                    , fromIntegral x
                                    ]
  | x < 0x100000000       = encodeU32 (fromIntegral x)
  | x < 0x10000000000     = BS.pack [ fromIntegral (x `unsafeShiftR` 32)
                                    , fromIntegral (x `unsafeShiftR` 24)
                                    , fromIntegral (x `unsafeShiftR` 16)
                                    , fromIntegral (x `unsafeShiftR` 8)
                                    , fromIntegral x
                                    ]
  | x < 0x1000000000000   = BS.pack [ fromIntegral (x `unsafeShiftR` 40)
                                    , fromIntegral (x `unsafeShiftR` 32)
                                    , fromIntegral (x `unsafeShiftR` 24)
                                    , fromIntegral (x `unsafeShiftR` 16)
                                    , fromIntegral (x `unsafeShiftR` 8)
                                    , fromIntegral x
                                    ]
  | x < 0x100000000000000 = BS.pack [ fromIntegral (x `unsafeShiftR` 48)
                                    , fromIntegral (x `unsafeShiftR` 40)
                                    , fromIntegral (x `unsafeShiftR` 32)
                                    , fromIntegral (x `unsafeShiftR` 24)
                                    , fromIntegral (x `unsafeShiftR` 16)
                                    , fromIntegral (x `unsafeShiftR` 8)
                                    , fromIntegral x
                                    ]
  | otherwise             = encodeU64 x
{-# INLINE encodeTu64 #-}

-- Minimal signed integer encoding ---------------------------------------------

-- | Encode a signed 64-bit integer using minimal bytes.
--
-- Uses the smallest number of bytes that can represent the value
-- in two's complement. Per BOLT #1 Appendix D test vectors.
--
-- >>> encodeMinSigned 0
-- "\NUL"
-- >>> encodeMinSigned 127
-- "\DEL"
-- >>> encodeMinSigned 128
-- "\NUL\128"
-- >>> encodeMinSigned (-1)
-- "\255"
-- >>> encodeMinSigned (-128)
-- "\128"
-- >>> encodeMinSigned (-129)
-- "\255\DEL"
encodeMinSigned :: Int64 -> BS.ByteString
encodeMinSigned !x
  | x >= -128 && x <= 127 =
      -- Fits in 1 byte
      BS.singleton (fromIntegral x)
  | x >= -32768 && x <= 32767 =
      -- Fits in 2 bytes
      encodeS16 (fromIntegral x)
  | x >= -2147483648 && x <= 2147483647 =
      -- Fits in 4 bytes
      encodeS32 (fromIntegral x)
  | otherwise =
      -- Need 8 bytes
      encodeS64 x
{-# INLINE encodeMinSigned #-}

-- BigSize encoding ------------------------------------------------------------

-- | Encode a BigSize value (variable-length unsigned integer).
--
-- >>> encodeBigSize 0
-- "\NUL"
-- >>> encodeBigSize 252
-- "\252"
-- >>> encodeBigSize 253
-- "\253\NUL\253"
-- >>> encodeBigSize 65536
-- "\254\NUL\SOH\NUL\NUL"
encodeBigSize :: Word64 -> BS.ByteString
encodeBigSize !x
  | x < 0xfd = BS.singleton (fromIntegral x)
  | x < 0x10000 = BS.cons 0xfd (encodeU16 (fromIntegral x))
  | x < 0x100000000 = BS.cons 0xfe (encodeU32 (fromIntegral x))
  | otherwise = BS.cons 0xff (encodeU64 x)
{-# INLINE encodeBigSize #-}

-- Length encoding -------------------------------------------------------------

-- | Encode a length as u16, checking bounds.
--
-- Returns Nothing if the length exceeds 65535.
encodeLength :: BS.ByteString -> Maybe BS.ByteString
encodeLength !bs
  | BS.length bs > 65535 = Nothing
  | otherwise = Just (encodeU16 (fromIntegral (BS.length bs)))
{-# INLINE encodeLength #-}

-- Unsigned integer decoding ---------------------------------------------------

-- | Decode a 16-bit unsigned integer (big-endian).
decodeU16 :: BS.ByteString -> Maybe (Word16, BS.ByteString)
decodeU16 !bs
  | BS.length bs < 2 = Nothing
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0)
          !b1 = fromIntegral (BS.index bs 1)
          !val = (b0 `unsafeShiftL` 8) .|. b1
      in  Just (val, BS.drop 2 bs)
{-# INLINE decodeU16 #-}

-- | Decode a 32-bit unsigned integer (big-endian).
decodeU32 :: BS.ByteString -> Maybe (Word32, BS.ByteString)
decodeU32 !bs
  | BS.length bs < 4 = Nothing
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0)
          !b1 = fromIntegral (BS.index bs 1)
          !b2 = fromIntegral (BS.index bs 2)
          !b3 = fromIntegral (BS.index bs 3)
          !val = (b0 `unsafeShiftL` 24) .|. (b1 `unsafeShiftL` 16)
              .|. (b2 `unsafeShiftL` 8) .|. b3
      in  Just (val, BS.drop 4 bs)
{-# INLINE decodeU32 #-}

-- | Decode a 64-bit unsigned integer (big-endian).
decodeU64 :: BS.ByteString -> Maybe (Word64, BS.ByteString)
decodeU64 !bs
  | BS.length bs < 8 = Nothing
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0)
          !b1 = fromIntegral (BS.index bs 1)
          !b2 = fromIntegral (BS.index bs 2)
          !b3 = fromIntegral (BS.index bs 3)
          !b4 = fromIntegral (BS.index bs 4)
          !b5 = fromIntegral (BS.index bs 5)
          !b6 = fromIntegral (BS.index bs 6)
          !b7 = fromIntegral (BS.index bs 7)
          !val = (b0 `unsafeShiftL` 56) .|. (b1 `unsafeShiftL` 48)
              .|. (b2 `unsafeShiftL` 40) .|. (b3 `unsafeShiftL` 32)
              .|. (b4 `unsafeShiftL` 24) .|. (b5 `unsafeShiftL` 16)
              .|. (b6 `unsafeShiftL` 8) .|. b7
      in  Just (val, BS.drop 8 bs)
{-# INLINE decodeU64 #-}

-- Signed integer decoding -----------------------------------------------------

-- | Decode an 8-bit signed integer.
decodeS8 :: BS.ByteString -> Maybe (Int8, BS.ByteString)
decodeS8 !bs
  | BS.null bs = Nothing
  | otherwise  = Just (fromIntegral (BS.index bs 0), BS.drop 1 bs)
{-# INLINE decodeS8 #-}

-- | Decode a 16-bit signed integer (big-endian two's complement).
decodeS16 :: BS.ByteString -> Maybe (Int16, BS.ByteString)
decodeS16 !bs = do
  (w, rest) <- decodeU16 bs
  Just (fromIntegral w, rest)
{-# INLINE decodeS16 #-}

-- | Decode a 32-bit signed integer (big-endian two's complement).
decodeS32 :: BS.ByteString -> Maybe (Int32, BS.ByteString)
decodeS32 !bs = do
  (w, rest) <- decodeU32 bs
  Just (fromIntegral w, rest)
{-# INLINE decodeS32 #-}

-- | Decode a 64-bit signed integer (big-endian two's complement).
decodeS64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
decodeS64 !bs = do
  (w, rest) <- decodeU64 bs
  Just (fromIntegral w, rest)
{-# INLINE decodeS64 #-}

-- Truncated unsigned integer decoding -----------------------------------------

-- | Decode a truncated 16-bit unsigned integer (0-2 bytes).
--
-- Returns Nothing if the encoding is non-minimal (has leading zeros).
decodeTu16 :: Int -> BS.ByteString -> Maybe (Word16, BS.ByteString)
decodeTu16 !len !bs
  | len < 0 || len > 2 = Nothing
  | BS.length bs < len = Nothing
  | len == 0 = Just (0, bs)
  | otherwise =
      let !bytes = BS.take len bs
          !rest = BS.drop len bs
      in  if BS.index bytes 0 == 0
            then Nothing  -- non-minimal: leading zero
            else Just (decodeBeWord16 bytes, rest)
  where
    decodeBeWord16 :: BS.ByteString -> Word16
    decodeBeWord16 b = case BS.length b of
      1 -> fromIntegral (BS.index b 0)
      2 -> (fromIntegral (BS.index b 0) `unsafeShiftL` 8)
        .|. fromIntegral (BS.index b 1)
      _ -> 0
{-# INLINE decodeTu16 #-}

-- | Decode a truncated 32-bit unsigned integer (0-4 bytes).
--
-- Returns Nothing if the encoding is non-minimal (has leading zeros).
decodeTu32 :: Int -> BS.ByteString -> Maybe (Word32, BS.ByteString)
decodeTu32 !len !bs
  | len < 0 || len > 4 = Nothing
  | BS.length bs < len = Nothing
  | len == 0 = Just (0, bs)
  | otherwise =
      let !bytes = BS.take len bs
          !rest = BS.drop len bs
      in  if BS.index bytes 0 == 0
            then Nothing  -- non-minimal: leading zero
            else Just (decodeBeWord32 len bytes, rest)
  where
    decodeBeWord32 :: Int -> BS.ByteString -> Word32
    decodeBeWord32 n b = go 0 0
      where
        go !acc !i
          | i >= n    = acc
          | otherwise = go ((acc `unsafeShiftL` 8)
                           .|. fromIntegral (BS.index b i)) (i + 1)
{-# INLINE decodeTu32 #-}

-- | Decode a truncated 64-bit unsigned integer (0-8 bytes).
--
-- Returns Nothing if the encoding is non-minimal (has leading zeros).
decodeTu64 :: Int -> BS.ByteString -> Maybe (Word64, BS.ByteString)
decodeTu64 !len !bs
  | len < 0 || len > 8 = Nothing
  | BS.length bs < len = Nothing
  | len == 0 = Just (0, bs)
  | otherwise =
      let !bytes = BS.take len bs
          !rest = BS.drop len bs
      in  if BS.index bytes 0 == 0
            then Nothing  -- non-minimal: leading zero
            else Just (decodeBeWord64 len bytes, rest)
  where
    decodeBeWord64 :: Int -> BS.ByteString -> Word64
    decodeBeWord64 n b = go 0 0
      where
        go !acc !i
          | i >= n    = acc
          | otherwise = go ((acc `unsafeShiftL` 8)
                           .|. fromIntegral (BS.index b i)) (i + 1)
{-# INLINE decodeTu64 #-}

-- Minimal signed integer decoding ---------------------------------------------

-- | Decode a minimal signed integer (1, 2, 4, or 8 bytes).
--
-- Validates that the encoding is minimal: the value could not be
-- represented in fewer bytes. Per BOLT #1 Appendix D test vectors.
decodeMinSigned :: Int -> BS.ByteString -> Maybe (Int64, BS.ByteString)
decodeMinSigned !len !bs
  | BS.length bs < len = Nothing
  | otherwise = case len of
      1 -> do
        (v, rest) <- decodeS8 bs
        Just (fromIntegral v, rest)
      2 -> do
        (v, rest) <- decodeS16 bs
        -- Must not fit in 1 byte
        if v >= -128 && v <= 127
          then Nothing
          else Just (fromIntegral v, rest)
      4 -> do
        (v, rest) <- decodeS32 bs
        -- Must not fit in 2 bytes
        if v >= -32768 && v <= 32767
          then Nothing
          else Just (fromIntegral v, rest)
      8 -> do
        (v, rest) <- decodeS64 bs
        -- Must not fit in 4 bytes
        if v >= -2147483648 && v <= 2147483647
          then Nothing
          else Just (v, rest)
      _ -> Nothing
{-# INLINE decodeMinSigned #-}

-- BigSize decoding ------------------------------------------------------------

-- | Decode a BigSize value with minimality check.
decodeBigSize :: BS.ByteString -> Maybe (Word64, BS.ByteString)
decodeBigSize !bs
  | BS.null bs = Nothing
  | otherwise = case BS.index bs 0 of
      0xff -> do
        (val, rest) <- decodeU64 (BS.drop 1 bs)
        -- Must be >= 0x100000000 for minimal encoding
        if val >= 0x100000000
          then Just (val, rest)
          else Nothing
      0xfe -> do
        (val, rest) <- decodeU32 (BS.drop 1 bs)
        -- Must be >= 0x10000 for minimal encoding
        if val >= 0x10000
          then Just (fromIntegral val, rest)
          else Nothing
      0xfd -> do
        (val, rest) <- decodeU16 (BS.drop 1 bs)
        -- Must be >= 0xfd for minimal encoding
        if val >= 0xfd
          then Just (fromIntegral val, rest)
          else Nothing
      b -> Just (fromIntegral b, BS.drop 1 bs)
