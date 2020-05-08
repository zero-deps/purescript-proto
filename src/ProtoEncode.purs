module Proto.Encode where

import Data.Array (snoc)
import Data.Int.Bits (zshr, shr, (.&.), (.|.))
import Prelude
import Proto.Uint8Array (Uint8Array, length, concatAll, fromArray)
import Proto.Utf8 as Utf8

foreign import splitFloat64 :: Number -> { low :: Int, high :: Int }

int32 :: Int -> Uint8Array
int32 x | x >= 0 = uint32 x
int32 x = fromArray $ loop [] x 0
  where
  loop :: Array Int -> Int -> Int -> Array Int
  loop acc val i | i < 9 = loop (acc `snoc` ((val .&. 127) .|. 128)) (val `shr` 7) (i + 1)
  loop acc val i = acc `snoc` 1

uint32 :: Int -> Uint8Array
uint32 = loop [] >>> fromArray
  where
  loop :: Array Int -> Int -> Array Int
  loop acc val = if val > 127 then loop (acc `snoc` ((val .&. 127) .|. 128)) (val `zshr` 7) else (acc `snoc` val)

double :: Number -> Uint8Array
double y = let x = splitFloat64 y in concatAll [ fixedUint32 x.low, fixedUint32 x.high ]
  where
  fixedUint32 :: Int -> Uint8Array
  fixedUint32 x = fromArray [ (x `zshr` 0) .&. 0xFF, (x `zshr` 8) .&. 0xFF, (x `zshr` 16) .&. 0xFF, (x `zshr` 24) .&. 0xFF ]

string :: String -> Uint8Array
string x = do
  let len = Utf8.numOfBytes x
  if len == 0
    then uint32 0
    else concatAll [ uint32 len, Utf8.toUint8Array x len ]

boolean :: Boolean -> Uint8Array
boolean true = fromArray [1]
boolean false = fromArray [0]

bytes :: Uint8Array -> Uint8Array
bytes xs = do
  let len = length xs
  if len == 0
    then uint32 0
    else concatAll [ uint32 len, xs ]

