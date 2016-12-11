module RiscV.Internal.Util
  ( bitMaskFromTo
  , bitsFromTo
  , extractBits
  ) where

import Data.Bits
import Data.List (foldl')
import Data.Word

bitMaskFromTo :: Int -> Int -> Word32
bitMaskFromTo low high = foldl' (.|.) zeroBits (map bit [low..high])

-- | Bits not in the range are set to 0
bitsFromTo :: Int -> Int -> Word32 -> Word32
bitsFromTo low high w = w .&. bitMaskFromTo low high


-- | Zeroes bits outside the range and right-shifts the range to align it at bit 0
extractBits :: Int -> Int -> Word32 -> Word32
extractBits low high word = (bitsFromTo low high word) `shiftR` low
