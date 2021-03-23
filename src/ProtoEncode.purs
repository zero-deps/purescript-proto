module Proto.Encode where

import Data.Array (snoc)
import Data.Int.Bits (zshr, shr, (.&.), (.|.))
import Prelude
import Proto.Uint8Array (Uint8Array, length, concatAll, fromArray)
import Proto.Utf8 as Utf8
import Proto.BigInt (BigInt)

type Low = Int
type High = Int

-- | Converts a floating-point number into 64-bit IEEE representation.
foreign import splitFloat64 :: Number -> { low :: Low, high :: High }
-- Encodes a 64-bit integer in 32:32 split representation into its wire-format varint representation
foreign import writeSplitVarint64 :: Low -> High -> Uint8Array
-- | Splits a signed Javascript integer into two 32-bit halves.
foreign import splitInt64 :: Number -> { low :: Low, high :: High }

foreign import splitBigInt :: BigInt -> { low :: Low, high :: High }

-- | Encodes a 32-bit signed integer into its wire-format varint representation.
signedVarint32 :: Int -> Uint8Array
-- Use the unsigned version if the value is not negative.
signedVarint32 x | x >= 0 = unsignedVarint32 x
signedVarint32 x = fromArray $ loop [] x 0
  where
  loop :: Array Int -> Int -> Int -> Array Int
  -- Write nine bytes with a _signed_ right shift so we preserve the sign bit.
  loop acc val i | i < 9 = loop (acc `snoc` ((val .&. 0x7f) .|. 0x80)) (val `shr` 7) (i + 1)
  -- The above loop writes out 63 bits, so the last byte is always the sign bit
  -- which is always set for negative numbers.
  loop acc val i = acc `snoc` 1

-- | Encodes a 32-bit unsigned integer into its wire-format varint representation.
unsignedVarint32 :: Int -> Uint8Array
unsignedVarint32 = loop [] >>> fromArray
  where
  loop :: Array Int -> Int -> Array Int
  loop acc val = if val > 127 then loop (acc `snoc` ((val .&. 0x7f) .|. 0x80)) (val `zshr` 7) else (acc `snoc` val)

-- | Encodes a 64-bit signed integer into its wire-format varint representation.
-- | Integers that are not representable in 64 bits will be truncated.
signedVarint64 :: Number -> Uint8Array
signedVarint64 y = let x = splitInt64 y in writeSplitVarint64 x.low x.high

bigInt :: BigInt -> Uint8Array
bigInt y = let x = splitBigInt y in writeSplitVarint64 x.low x.high

double :: Number -> Uint8Array
double y = let x = splitFloat64 y in concatAll [ fixedUint32 x.low, fixedUint32 x.high ]
  where
  fixedUint32 :: Int -> Uint8Array
  fixedUint32 x = fromArray [ (x `zshr` 0) .&. 0xFF, (x `zshr` 8) .&. 0xFF, (x `zshr` 16) .&. 0xFF, (x `zshr` 24) .&. 0xFF ]

string :: String -> Uint8Array
string x = do
  let len = Utf8.numOfBytes x
  if len == 0
    then unsignedVarint32 0
    else concatAll [ unsignedVarint32 len, Utf8.toUint8Array x len ]

boolean :: Boolean -> Uint8Array
boolean true = fromArray [1]
boolean false = fromArray [0]

bytes :: Uint8Array -> Uint8Array
bytes xs = do
  let len = length xs
  if len == 0
    then unsignedVarint32 0
    else concatAll [ unsignedVarint32 len, xs ]

