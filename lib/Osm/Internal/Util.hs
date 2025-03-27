{-# LANGUAGE BinaryLiterals #-}

-- |
-- Module    : Streaming.Osm.Internal.Util
-- Copyright : (c) Azavea, 2017 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Osm.Internal.Util
  ( foldBytes
  , breakOn0
  , pairs
  , both
  , unzig, undelta
  ) where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.List (scanl')
import           Data.Word

---

-- to16 :: Word8 -> Word16
-- to16 = fromIntegral

-- to8 :: Word16 -> Word8
-- to8 = fromIntegral

-- unkey :: Word8 -> Word8 -> Either (Word8, Word8) Word8
-- unkey f w = case (to8 $ shiftR smash 7, to8 $ smash .&. 0b01111111) of
--   (0, r) -> Right r
--   (l, r) -> Left (setBit r 7, l)
--   where smash = shift (to16 f) 3 .|. to16 w

-- | Discover a field's number and /Wire Type/. The wire type is expected to
-- be a value from 0 to 5. The field number itself can probably be any varint,
-- although in practice these are in `Word8` range.
--
-- The results are left as numbers, since pattern matching on those should
-- be faster.
-- key :: (Num t, Bits t) => t -> (t, t)
-- key w = (shiftR w 3, w .&. 0b00000111)

-- | For the case when two bytes denote the field number and /Wire Type/. We
-- know that for OSM data, the highest field number is 34. Encoding 34 with any
-- wire type takes 2 bytes, so we know we'll never need to check for more.
-- key2 :: Word8 -> Word8 -> (Word16, Word16)
-- key2 w1 w0 = key $ shift (to16 w0) 7 .|. to16 (clearBit w1 7)

-- `BS.foldr'` is tail-recursive, unlike List's foldr, so it should be just as fast as foldl.
-- | Fold a `BS.ByteString` into an `Int` which was parsed with wire-type 2
-- (Length-delimited). These follow the pattern @tagByte byteCount bytes@,
-- where we've parsed @byteCount@ and done an attoparsec @take@ on the @bytes@.
-- These bytes could be a packed repeated field of varints, meaning they could
-- all have different byte lengths.
--
-- This function uses the rules described
-- <https://developers.google.com/protocol-buffers/docs/encoding here> for
-- determining when to accumulate the current byte, or to consider it the first
-- byte of the next value. In short, the rule is:
--
--   1. If the MSB of a byte is 1, then expect at least the next byte to belong to this value.
--   2. If the MSB of a byte is 0, we're at the end of the current accumulating value (or
--      at the first byte of a single byte value).
foldBytes :: BS.ByteString -> Word8 -> Int
foldBytes bs b = BS.foldr' (\w acc -> shift acc 7 .|. clearBit (fromIntegral w) 7) (fromIntegral b) bs

-- | `words` for `Int`, where 0 is the whitespace. Implementation adapted
-- from `words`.
breakOn0 :: [Int] -> [[Int]]
breakOn0 [] = []
breakOn0 ns = case ys of
    []     -> [xs]
    _ : zs -> xs : breakOn0 zs
  where
    (xs, ys) = span (/= 0) ns

-- | A sort of "self-zip", forming pairs from every two elements in a list.
-- Assumes that the list is of even length.
pairs :: [a] -> [(a,a)]
pairs []       = []
pairs (x:y:zs) = (x,y) : pairs zs
pairs _        = error "List was of uneven length."

-- | Apply a function to both elements of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (a,b) = (f a, f b)

-- | Decode a Z-encoded `Int`.
unzig :: Int -> Int
unzig n = shift n (-1) `xor` negate (n .&. 1)

-- | Restore a list of numbers that have been Delta Encoded.
undelta :: [Int] -> [Int]
undelta []       = []
undelta (x : xs) = scanl' (+) x xs
