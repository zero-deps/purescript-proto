module Proto.BigInt where

import Prelude

foreign import data BigInt :: Type

foreign import fromNumber :: Number -> BigInt
foreign import unsafeToNumber :: BigInt -> Number
foreign import fromString :: String -> BigInt
foreign import showImpl :: BigInt -> String
foreign import eqImpl :: BigInt -> BigInt -> Boolean

instance showBigInt :: Show BigInt where
  show = showImpl

instance eqBigInt :: Eq BigInt where
  eq = eqImpl