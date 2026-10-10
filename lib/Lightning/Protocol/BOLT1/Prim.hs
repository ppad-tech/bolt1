{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT1.Prim
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Fundamental types and primitive encodings for BOLT #1.

module Lightning.Protocol.BOLT1.Prim (
  -- * Fixed-size byte types
    ChainHash
  , chain_hash
  , un_chain_hash
  , ChannelId
  , channel_id
  , un_channel_id
  , all_channels
  , Signature
  , signature
  , un_signature
  , Point
  , point
  , un_point
  , PaymentHash
  , payment_hash
  , un_payment_hash
  , PaymentPreimage
  , payment_preimage
  , un_payment_preimage
  , PerCommitmentSecret
  , per_commitment_secret
  , un_per_commitment_secret

  -- * Short channel identifiers
  , ShortChannelId(..)
  , short_channel_id
  , scid_block_height
  , scid_tx_index
  , scid_output_index

  -- * Amounts
  , Satoshi
  , satoshi
  , un_satoshi
  , max_satoshi
  , MilliSatoshi
  , milli_satoshi
  , un_milli_satoshi
  , max_milli_satoshi
  , sat_to_msat
  , msat_to_sat
  , add_sat
  , sub_sat
  , add_msat
  , sub_msat

  -- * Unsigned integers
  , encode_u16
  , encode_u32
  , encode_u64
  , decode_u16
  , decode_u32
  , decode_u64

  -- * Signed integers
  , encode_s8
  , encode_s16
  , encode_s32
  , encode_s64
  , decode_s8
  , decode_s16
  , decode_s32
  , decode_s64

  -- * Truncated unsigned integers
  , encode_tu16
  , encode_tu32
  , encode_tu64
  , decode_tu16
  , decode_tu32
  , decode_tu64

  -- * BigSize
  , encode_bigsize
  , decode_bigsize
  , BigSizeError(..)
  , decode_bigsize_detailed

  -- * Length-prefixed bytes
  , encode_u16_prefixed
  , decode_u16_prefixed

  -- * Fixed-size field codecs
  , decode_chain_hash
  , decode_channel_id
  , decode_signature
  , decode_point
  , decode_payment_hash
  , decode_payment_preimage
  , decode_per_commitment_secret
  , encode_short_channel_id
  , decode_short_channel_id
  , encode_satoshi
  , decode_satoshi
  , encode_milli_satoshi
  , decode_milli_satoshi
  ) where

import Control.DeepSeq (NFData(..))
import Data.Bits ((.&.), (.|.), unsafeShiftL, unsafeShiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Generics (Generic)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- constant-time equality on equal-length bytestrings
ct_eq :: BS.ByteString -> BS.ByteString -> Bool
ct_eq a b
  | BS.length a /= BS.length b = False
  | otherwise = go 0 0
  where
    !n = BS.length a
    go :: Word8 -> Int -> Bool
    go !acc !i
      | i == n    = acc == 0
      | otherwise =
          go (acc .|. (BU.unsafeIndex a i `xor` BU.unsafeIndex b i))
             (i + 1)

-- take exactly n bytes from the front of the input
take_n :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
take_n !n !bs
  | BS.length bs < n = Nothing
  | otherwise        = Just (BU.unsafeTake n bs, BU.unsafeDrop n bs)
{-# INLINE take_n #-}

-- fixed-size byte types ------------------------------------------------------

-- | A 32-byte chain hash, identifying the chain a channel or message
--   belongs to.
newtype ChainHash = ChainHash BS.ByteString
  deriving (Eq, Ord, Show, Generic)

instance NFData ChainHash

-- | Construct a 'ChainHash' from exactly 32 bytes.
--
--   >>> fmap (BS.length . un_chain_hash) (chain_hash (BS.replicate 32 0))
--   Just 32
--   >>> chain_hash "too short"
--   Nothing
chain_hash :: BS.ByteString -> Maybe ChainHash
chain_hash bs
  | BS.length bs == 32 = Just (ChainHash bs)
  | otherwise          = Nothing
{-# INLINE chain_hash #-}

-- | The bytes of a 'ChainHash'.
un_chain_hash :: ChainHash -> BS.ByteString
un_chain_hash (ChainHash bs) = bs
{-# INLINE un_chain_hash #-}

-- | A 32-byte channel identifier.
newtype ChannelId = ChannelId BS.ByteString
  deriving (Eq, Ord, Show, Generic)

instance NFData ChannelId

-- | Construct a 'ChannelId' from exactly 32 bytes.
--
--   >>> fmap (BS.length . un_channel_id) (channel_id (BS.replicate 32 1))
--   Just 32
channel_id :: BS.ByteString -> Maybe ChannelId
channel_id bs
  | BS.length bs == 32 = Just (ChannelId bs)
  | otherwise          = Nothing
{-# INLINE channel_id #-}

-- | The bytes of a 'ChannelId'.
un_channel_id :: ChannelId -> BS.ByteString
un_channel_id (ChannelId bs) = bs
{-# INLINE un_channel_id #-}

-- | The all-zero channel identifier, which refers to all channels
--   (e.g. in connection-level @error@ and @warning@ messages).
all_channels :: ChannelId
all_channels = ChannelId (BS.replicate 32 0x00)

-- | A 64-byte compact ECDSA signature.
newtype Signature = Signature BS.ByteString
  deriving (Eq, Ord, Show, Generic)

instance NFData Signature

-- | Construct a 'Signature' from exactly 64 bytes.
--
--   >>> signature (BS.replicate 63 0x00)
--   Nothing
signature :: BS.ByteString -> Maybe Signature
signature bs
  | BS.length bs == 64 = Just (Signature bs)
  | otherwise          = Nothing
{-# INLINE signature #-}

-- | The bytes of a 'Signature'.
un_signature :: Signature -> BS.ByteString
un_signature (Signature bs) = bs
{-# INLINE un_signature #-}

-- | A 33-byte compressed secp256k1 point.
newtype Point = Point BS.ByteString
  deriving (Eq, Ord, Show, Generic)

instance NFData Point

-- | Construct a 'Point' from 33 bytes with a compressed-encoding
--   prefix (0x02 or 0x03). The curve equation is not checked; use
--   ppad-secp256k1 to parse the point if it must be on the curve.
--
--   >>> point (BS.cons 0x04 (BS.replicate 32 0x01))
--   Nothing
point :: BS.ByteString -> Maybe Point
point bs
  | BS.length bs == 33
  , let !h = BU.unsafeIndex bs 0
  , h == 0x02 || h == 0x03 = Just (Point bs)
  | otherwise              = Nothing
{-# INLINE point #-}

-- | The bytes of a 'Point'.
un_point :: Point -> BS.ByteString
un_point (Point bs) = bs
{-# INLINE un_point #-}

-- | A 32-byte SHA256 payment hash.
newtype PaymentHash = PaymentHash BS.ByteString
  deriving (Eq, Ord, Show, Generic)

instance NFData PaymentHash

-- | Construct a 'PaymentHash' from exactly 32 bytes.
payment_hash :: BS.ByteString -> Maybe PaymentHash
payment_hash bs
  | BS.length bs == 32 = Just (PaymentHash bs)
  | otherwise          = Nothing
{-# INLINE payment_hash #-}

-- | The bytes of a 'PaymentHash'.
un_payment_hash :: PaymentHash -> BS.ByteString
un_payment_hash (PaymentHash bs) = bs
{-# INLINE un_payment_hash #-}

-- | A 32-byte payment preimage.
--
--   This is secret material: its 'Show' instance is redacted and its
--   'Eq' instance runs in constant time.
newtype PaymentPreimage = PaymentPreimage BS.ByteString

instance Eq PaymentPreimage where
  PaymentPreimage a == PaymentPreimage b = ct_eq a b

instance Show PaymentPreimage where
  showsPrec d _ = showParen (d > 10) $
    showString "PaymentPreimage <redacted>"

instance NFData PaymentPreimage where
  rnf (PaymentPreimage bs) = rnf bs

-- | Construct a 'PaymentPreimage' from exactly 32 bytes.
payment_preimage :: BS.ByteString -> Maybe PaymentPreimage
payment_preimage bs
  | BS.length bs == 32 = Just (PaymentPreimage bs)
  | otherwise          = Nothing
{-# INLINE payment_preimage #-}

-- | The bytes of a 'PaymentPreimage'.
un_payment_preimage :: PaymentPreimage -> BS.ByteString
un_payment_preimage (PaymentPreimage bs) = bs
{-# INLINE un_payment_preimage #-}

-- | A 32-byte per-commitment secret.
--
--   This is secret material: its 'Show' instance is redacted and its
--   'Eq' instance runs in constant time.
newtype PerCommitmentSecret = PerCommitmentSecret BS.ByteString

instance Eq PerCommitmentSecret where
  PerCommitmentSecret a == PerCommitmentSecret b = ct_eq a b

instance Show PerCommitmentSecret where
  showsPrec d _ = showParen (d > 10) $
    showString "PerCommitmentSecret <redacted>"

instance NFData PerCommitmentSecret where
  rnf (PerCommitmentSecret bs) = rnf bs

-- | Construct a 'PerCommitmentSecret' from exactly 32 bytes.
per_commitment_secret :: BS.ByteString -> Maybe PerCommitmentSecret
per_commitment_secret bs
  | BS.length bs == 32 = Just (PerCommitmentSecret bs)
  | otherwise          = Nothing
{-# INLINE per_commitment_secret #-}

-- | The bytes of a 'PerCommitmentSecret'.
un_per_commitment_secret :: PerCommitmentSecret -> BS.ByteString
un_per_commitment_secret (PerCommitmentSecret bs) = bs
{-# INLINE un_per_commitment_secret #-}

-- short channel identifiers --------------------------------------------------

-- | A short channel identifier: block height (3 bytes), transaction
--   index (3 bytes) and output index (2 bytes), packed big-endian into
--   a 'Word64'. Every 'Word64' is a valid packing.
newtype ShortChannelId = ShortChannelId Word64
  deriving (Eq, Ord, Show, Generic)

instance NFData ShortChannelId

-- | Construct a 'ShortChannelId' from its components. Fails if the
--   block height or transaction index exceed 24 bits.
--
--   >>> short_channel_id 539268 845 1
--   Just (ShortChannelId 592931436542885889)
short_channel_id
  :: Word32 -- ^ block height
  -> Word32 -- ^ transaction index
  -> Word16 -- ^ output index
  -> Maybe ShortChannelId
short_channel_id h t o
  | h > 0xFFFFFF = Nothing
  | t > 0xFFFFFF = Nothing
  | otherwise    = Just $! ShortChannelId $!
          (fi h `unsafeShiftL` 40)
      .|. (fi t `unsafeShiftL` 16)
      .|. fi o
{-# INLINE short_channel_id #-}

-- | The block height of a 'ShortChannelId'.
scid_block_height :: ShortChannelId -> Word32
scid_block_height (ShortChannelId w) = fi (w `unsafeShiftR` 40)
{-# INLINE scid_block_height #-}

-- | The transaction index of a 'ShortChannelId'.
scid_tx_index :: ShortChannelId -> Word32
scid_tx_index (ShortChannelId w) = fi ((w `unsafeShiftR` 16) .&. 0xFFFFFF)
{-# INLINE scid_tx_index #-}

-- | The output index of a 'ShortChannelId'.
scid_output_index :: ShortChannelId -> Word16
scid_output_index (ShortChannelId w) = fi (w .&. 0xFFFF)
{-# INLINE scid_output_index #-}

-- amounts --------------------------------------------------------------------

-- | An amount in satoshis, at most 21 million BTC ('max_satoshi').
newtype Satoshi = Satoshi Word64
  deriving (Eq, Ord, Show, Generic)

instance NFData Satoshi

-- | An amount in millisatoshis, at most 21 million BTC
--   ('max_milli_satoshi').
newtype MilliSatoshi = MilliSatoshi Word64
  deriving (Eq, Ord, Show, Generic)

instance NFData MilliSatoshi

-- | 21 million BTC, in satoshis (0x000775f05a074000).
max_satoshi :: Satoshi
max_satoshi = Satoshi 0x000775f05a074000

-- | 21 million BTC, in millisatoshis (0x1d24b2dfac520000).
max_milli_satoshi :: MilliSatoshi
max_milli_satoshi = MilliSatoshi 0x1d24b2dfac520000

-- | Construct a 'Satoshi' amount, failing above 21 million BTC.
--
--   >>> satoshi 1000
--   Just (Satoshi 1000)
--   >>> satoshi 0xffffffffffffffff
--   Nothing
satoshi :: Word64 -> Maybe Satoshi
satoshi w
  | w <= 0x000775f05a074000 = Just (Satoshi w)
  | otherwise               = Nothing
{-# INLINE satoshi #-}

-- | The number of satoshis.
un_satoshi :: Satoshi -> Word64
un_satoshi (Satoshi w) = w
{-# INLINE un_satoshi #-}

-- | Construct a 'MilliSatoshi' amount, failing above 21 million BTC.
milli_satoshi :: Word64 -> Maybe MilliSatoshi
milli_satoshi w
  | w <= 0x1d24b2dfac520000 = Just (MilliSatoshi w)
  | otherwise               = Nothing
{-# INLINE milli_satoshi #-}

-- | The number of millisatoshis.
un_milli_satoshi :: MilliSatoshi -> Word64
un_milli_satoshi (MilliSatoshi w) = w
{-# INLINE un_milli_satoshi #-}

-- | Convert satoshis to millisatoshis. Total, since the bounds
--   correspond.
--
--   >>> fmap sat_to_msat (satoshi 5)
--   Just (MilliSatoshi 5000)
sat_to_msat :: Satoshi -> MilliSatoshi
sat_to_msat (Satoshi s) = MilliSatoshi (s * 1000)
{-# INLINE sat_to_msat #-}

-- | Convert millisatoshis to satoshis, rounding down.
msat_to_sat :: MilliSatoshi -> Satoshi
msat_to_sat (MilliSatoshi m) = Satoshi (m `quot` 1000)
{-# INLINE msat_to_sat #-}

-- | Add two amounts, failing above 21 million BTC. (Amounts are
--   bounded well below 2^63, so the sum can't wrap.)
add_sat :: Satoshi -> Satoshi -> Maybe Satoshi
add_sat (Satoshi a) (Satoshi b) = satoshi (a + b)
{-# INLINE add_sat #-}

-- | Subtract the second amount from the first, failing if the result
--   would be negative.
sub_sat :: Satoshi -> Satoshi -> Maybe Satoshi
sub_sat (Satoshi a) (Satoshi b)
  | b > a     = Nothing
  | otherwise = Just (Satoshi (a - b))
{-# INLINE sub_sat #-}

-- | Add two amounts, failing above 21 million BTC.
add_msat :: MilliSatoshi -> MilliSatoshi -> Maybe MilliSatoshi
add_msat (MilliSatoshi a) (MilliSatoshi b) = milli_satoshi (a + b)
{-# INLINE add_msat #-}

-- | Subtract the second amount from the first, failing if the result
--   would be negative.
sub_msat :: MilliSatoshi -> MilliSatoshi -> Maybe MilliSatoshi
sub_msat (MilliSatoshi a) (MilliSatoshi b)
  | b > a     = Nothing
  | otherwise = Just (MilliSatoshi (a - b))
{-# INLINE sub_msat #-}

-- unsigned integers ----------------------------------------------------------

poke8 :: Ptr Word8 -> Int -> Word8 -> IO ()
poke8 = pokeByteOff
{-# INLINE poke8 #-}

-- | Encode a 'Word16' as 2 big-endian bytes.
--
--   >>> encode_u16 0x0102
--   "\SOH\STX"
encode_u16 :: Word16 -> BS.ByteString
encode_u16 w = BI.unsafeCreate 2 $ \p -> do
  poke8 p 0 (fi (w `unsafeShiftR` 8))
  poke8 p 1 (fi w)
{-# INLINE encode_u16 #-}

-- | Encode a 'Word32' as 4 big-endian bytes.
encode_u32 :: Word32 -> BS.ByteString
encode_u32 w = BI.unsafeCreate 4 $ \p -> do
  poke8 p 0 (fi (w `unsafeShiftR` 24))
  poke8 p 1 (fi (w `unsafeShiftR` 16))
  poke8 p 2 (fi (w `unsafeShiftR` 8))
  poke8 p 3 (fi w)
{-# INLINE encode_u32 #-}

-- | Encode a 'Word64' as 8 big-endian bytes.
encode_u64 :: Word64 -> BS.ByteString
encode_u64 w = BI.unsafeCreate 8 $ \p -> do
  poke8 p 0 (fi (w `unsafeShiftR` 56))
  poke8 p 1 (fi (w `unsafeShiftR` 48))
  poke8 p 2 (fi (w `unsafeShiftR` 40))
  poke8 p 3 (fi (w `unsafeShiftR` 32))
  poke8 p 4 (fi (w `unsafeShiftR` 24))
  poke8 p 5 (fi (w `unsafeShiftR` 16))
  poke8 p 6 (fi (w `unsafeShiftR` 8))
  poke8 p 7 (fi w)
{-# INLINE encode_u64 #-}

-- big-endian word from the first n bytes (n <= 8, length checked)
be :: Int -> BS.ByteString -> Word64
be n bs = go 0 0
  where
    go !acc !i
      | i == n    = acc
      | otherwise =
          go ((acc `unsafeShiftL` 8) .|. fi (BU.unsafeIndex bs i)) (i + 1)
{-# INLINE be #-}

-- | Decode 2 big-endian bytes, returning the remaining input.
--
--   >>> decode_u16 "\SOH\STXrest"
--   Just (258,"rest")
decode_u16 :: BS.ByteString -> Maybe (Word16, BS.ByteString)
decode_u16 bs
  | BS.length bs < 2 = Nothing
  | otherwise        = Just (fi (be 2 bs), BU.unsafeDrop 2 bs)
{-# INLINE decode_u16 #-}

-- | Decode 4 big-endian bytes, returning the remaining input.
decode_u32 :: BS.ByteString -> Maybe (Word32, BS.ByteString)
decode_u32 bs
  | BS.length bs < 4 = Nothing
  | otherwise        = Just (fi (be 4 bs), BU.unsafeDrop 4 bs)
{-# INLINE decode_u32 #-}

-- | Decode 8 big-endian bytes, returning the remaining input.
decode_u64 :: BS.ByteString -> Maybe (Word64, BS.ByteString)
decode_u64 bs
  | BS.length bs < 8 = Nothing
  | otherwise        = Just (be 8 bs, BU.unsafeDrop 8 bs)
{-# INLINE decode_u64 #-}

-- signed integers ------------------------------------------------------------

-- | Encode an 'Int8' as 1 byte (two's complement).
--
--   >>> encode_s8 (-42)
--   "\214"
encode_s8 :: Int8 -> BS.ByteString
encode_s8 = BS.singleton . fi
{-# INLINE encode_s8 #-}

-- | Encode an 'Int16' as 2 big-endian bytes (two's complement).
encode_s16 :: Int16 -> BS.ByteString
encode_s16 = encode_u16 . fi
{-# INLINE encode_s16 #-}

-- | Encode an 'Int32' as 4 big-endian bytes (two's complement).
encode_s32 :: Int32 -> BS.ByteString
encode_s32 = encode_u32 . fi
{-# INLINE encode_s32 #-}

-- | Encode an 'Int64' as 8 big-endian bytes (two's complement).
encode_s64 :: Int64 -> BS.ByteString
encode_s64 = encode_u64 . fi
{-# INLINE encode_s64 #-}

-- | Decode 1 byte (two's complement), returning the remaining input.
decode_s8 :: BS.ByteString -> Maybe (Int8, BS.ByteString)
decode_s8 bs = case BS.uncons bs of
  Just (h, t) -> Just (fi h, t)
  Nothing     -> Nothing
{-# INLINE decode_s8 #-}

-- | Decode 2 big-endian bytes (two's complement).
decode_s16 :: BS.ByteString -> Maybe (Int16, BS.ByteString)
decode_s16 bs = case decode_u16 bs of
  Just (w, t) -> Just (fi w, t)
  Nothing     -> Nothing
{-# INLINE decode_s16 #-}

-- | Decode 4 big-endian bytes (two's complement).
decode_s32 :: BS.ByteString -> Maybe (Int32, BS.ByteString)
decode_s32 bs = case decode_u32 bs of
  Just (w, t) -> Just (fi w, t)
  Nothing     -> Nothing
{-# INLINE decode_s32 #-}

-- | Decode 8 big-endian bytes (two's complement).
decode_s64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
decode_s64 bs = case decode_u64 bs of
  Just (w, t) -> Just (fi w, t)
  Nothing     -> Nothing
{-# INLINE decode_s64 #-}

-- truncated unsigned integers ------------------------------------------------

-- minimal big-endian encoding: no leading zero bytes (zero is empty)
encode_tu :: Word64 -> BS.ByteString
encode_tu w = BS.drop (lz 0) (encode_u64 w)
  where
    lz :: Int -> Int
    lz !i
      | i < 8 && (w `unsafeShiftR` (56 - 8 * i)) .&. 0xff == 0 = lz (i + 1)
      | otherwise = i

decode_tu :: Int -> BS.ByteString -> Maybe Word64
decode_tu maxlen bs
  | n > maxlen                       = Nothing
  | n > 0 && BU.unsafeIndex bs 0 == 0 = Nothing
  | otherwise                        = Just (be n bs)
  where
    !n = BS.length bs

-- | Encode a 'Word16' as a truncated integer (no leading zero bytes).
--
--   >>> encode_tu16 0
--   ""
--   >>> encode_tu16 0x0100
--   "\SOH\NUL"
encode_tu16 :: Word16 -> BS.ByteString
encode_tu16 = encode_tu . fi

-- | Encode a 'Word32' as a truncated integer (no leading zero bytes).
encode_tu32 :: Word32 -> BS.ByteString
encode_tu32 = encode_tu . fi

-- | Encode a 'Word64' as a truncated integer (no leading zero bytes).
encode_tu64 :: Word64 -> BS.ByteString
encode_tu64 = encode_tu

-- | Decode a truncated 16-bit integer occupying the whole input. Fails
--   on more than 2 bytes or a non-minimal encoding.
--
--   >>> decode_tu16 "\SOH\NUL"
--   Just 256
--   >>> decode_tu16 "\NUL\SOH"
--   Nothing
decode_tu16 :: BS.ByteString -> Maybe Word16
decode_tu16 = fmap fi . decode_tu 2

-- | Decode a truncated 32-bit integer occupying the whole input. Fails
--   on more than 4 bytes or a non-minimal encoding.
decode_tu32 :: BS.ByteString -> Maybe Word32
decode_tu32 = fmap fi . decode_tu 4

-- | Decode a truncated 64-bit integer occupying the whole input. Fails
--   on more than 8 bytes or a non-minimal encoding.
decode_tu64 :: BS.ByteString -> Maybe Word64
decode_tu64 = decode_tu 8

-- bigsize --------------------------------------------------------------------

-- | Encode a 'Word64' in the minimal BigSize format.
--
--   >>> encode_bigsize 252
--   "\252"
--   >>> encode_bigsize 253
--   "\253\NUL\253"
encode_bigsize :: Word64 -> BS.ByteString
encode_bigsize w
  | w < 0xfd        = BS.singleton (fi w)
  | w <= 0xffff     = BS.cons 0xfd (encode_u16 (fi w))
  | w <= 0xffffffff = BS.cons 0xfe (encode_u32 (fi w))
  | otherwise       = BS.cons 0xff (encode_u64 w)

-- | Why a BigSize failed to decode.
data BigSizeError
  = BigSizeTruncated   -- ^ input ended early
  | BigSizeNonMinimal  -- ^ a shorter encoding exists
  deriving (Eq, Show, Generic)

instance NFData BigSizeError

-- | Decode a BigSize, distinguishing truncation from non-minimal
--   encodings.
decode_bigsize_detailed
  :: BS.ByteString -> Either BigSizeError (Word64, BS.ByteString)
decode_bigsize_detailed bs = case BS.uncons bs of
  Nothing -> Left BigSizeTruncated
  Just (h, t) -> case h of
    0xfd -> wide 2 0xfd t
    0xfe -> wide 4 0x10000 t
    0xff -> wide 8 0x100000000 t
    _    -> Right (fi h, t)
  where
    wide n lo t = case take_n n t of
      Nothing -> Left BigSizeTruncated
      Just (v, r)
        | w < lo    -> Left BigSizeNonMinimal
        | otherwise -> Right (w, r)
        where
          !w = be n v

-- | Decode a minimally-encoded BigSize, returning the remaining input.
--
--   >>> decode_bigsize "\253\NUL\253rest"
--   Just (253,"rest")
--   >>> decode_bigsize "\253\NUL\252"
--   Nothing
decode_bigsize :: BS.ByteString -> Maybe (Word64, BS.ByteString)
decode_bigsize bs = case decode_bigsize_detailed bs of
  Right r -> Just r
  Left _  -> Nothing

-- length-prefixed bytes ------------------------------------------------------

-- | Prefix bytes with their length as a u16. Fails if the input
--   exceeds 65535 bytes.
--
--   >>> encode_u16_prefixed "abc"
--   Just "\NUL\ETXabc"
encode_u16_prefixed :: BS.ByteString -> Maybe BS.ByteString
encode_u16_prefixed bs
  | BS.length bs > 0xffff = Nothing
  | otherwise = Just (encode_u16 (fi (BS.length bs)) <> bs)

-- | Decode u16-length-prefixed bytes, returning the remaining input.
--
--   >>> decode_u16_prefixed "\NUL\ETXabcrest"
--   Just ("abc","rest")
decode_u16_prefixed
  :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
decode_u16_prefixed bs = do
  (n, rest) <- decode_u16 bs
  take_n (fi n) rest

-- fixed-size field codecs ----------------------------------------------------

-- | Decode a 32-byte 'ChainHash', returning the remaining input.
decode_chain_hash :: BS.ByteString -> Maybe (ChainHash, BS.ByteString)
decode_chain_hash bs = do
  (h, r) <- take_n 32 bs
  pure (ChainHash h, r)

-- | Decode a 32-byte 'ChannelId', returning the remaining input.
decode_channel_id :: BS.ByteString -> Maybe (ChannelId, BS.ByteString)
decode_channel_id bs = do
  (h, r) <- take_n 32 bs
  pure (ChannelId h, r)

-- | Decode a 64-byte 'Signature', returning the remaining input.
decode_signature :: BS.ByteString -> Maybe (Signature, BS.ByteString)
decode_signature bs = do
  (h, r) <- take_n 64 bs
  pure (Signature h, r)

-- | Decode a 33-byte 'Point', returning the remaining input. Fails if
--   the prefix byte is not 0x02 or 0x03.
decode_point :: BS.ByteString -> Maybe (Point, BS.ByteString)
decode_point bs = do
  (h, r) <- take_n 33 bs
  p <- point h
  pure (p, r)

-- | Decode a 32-byte 'PaymentHash', returning the remaining input.
decode_payment_hash :: BS.ByteString -> Maybe (PaymentHash, BS.ByteString)
decode_payment_hash bs = do
  (h, r) <- take_n 32 bs
  pure (PaymentHash h, r)

-- | Decode a 32-byte 'PaymentPreimage', returning the remaining input.
decode_payment_preimage
  :: BS.ByteString -> Maybe (PaymentPreimage, BS.ByteString)
decode_payment_preimage bs = do
  (h, r) <- take_n 32 bs
  pure (PaymentPreimage h, r)

-- | Decode a 32-byte 'PerCommitmentSecret', returning the remaining
--   input.
decode_per_commitment_secret
  :: BS.ByteString -> Maybe (PerCommitmentSecret, BS.ByteString)
decode_per_commitment_secret bs = do
  (h, r) <- take_n 32 bs
  pure (PerCommitmentSecret h, r)

-- | Encode a 'ShortChannelId' as 8 big-endian bytes.
encode_short_channel_id :: ShortChannelId -> BS.ByteString
encode_short_channel_id (ShortChannelId w) = encode_u64 w
{-# INLINE encode_short_channel_id #-}

-- | Decode an 8-byte 'ShortChannelId', returning the remaining input.
decode_short_channel_id
  :: BS.ByteString -> Maybe (ShortChannelId, BS.ByteString)
decode_short_channel_id bs = do
  (w, r) <- decode_u64 bs
  pure (ShortChannelId w, r)
{-# INLINE decode_short_channel_id #-}

-- | Encode a 'Satoshi' amount as a u64.
encode_satoshi :: Satoshi -> BS.ByteString
encode_satoshi (Satoshi w) = encode_u64 w
{-# INLINE encode_satoshi #-}

-- | Decode a u64 'Satoshi' amount, returning the remaining input. Fails
--   above 21 million BTC.
decode_satoshi :: BS.ByteString -> Maybe (Satoshi, BS.ByteString)
decode_satoshi bs = do
  (w, r) <- decode_u64 bs
  s <- satoshi w
  pure (s, r)
{-# INLINE decode_satoshi #-}

-- | Encode a 'MilliSatoshi' amount as a u64.
encode_milli_satoshi :: MilliSatoshi -> BS.ByteString
encode_milli_satoshi (MilliSatoshi w) = encode_u64 w
{-# INLINE encode_milli_satoshi #-}

-- | Decode a u64 'MilliSatoshi' amount, returning the remaining input.
--   Fails above 21 million BTC.
decode_milli_satoshi
  :: BS.ByteString -> Maybe (MilliSatoshi, BS.ByteString)
decode_milli_satoshi bs = do
  (w, r) <- decode_u64 bs
  m <- milli_satoshi w
  pure (m, r)
{-# INLINE decode_milli_satoshi #-}
