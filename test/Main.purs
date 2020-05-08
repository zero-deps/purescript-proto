module Test.Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, pure, show, unit, negate, ($), (<>), (==))
import Proto.Uint8Array (Uint8Array, fromArray)
import Data.Either (Either(Left, Right))
import Node.Process (exit)
import Proto.Decode as Decode
import Proto.Encode as Encode

main :: Effect Unit
main = do
  case_int_zero
  case_int_positive
  case_int_negative
  case_eq

case_eq :: Effect Unit
case_eq = do
  let xs = fromArray []
  let ys = fromArray []
  if xs == ys then log "case_eq: ok"
  else do
    log "case_eq: failed"
    exit 99

case_int_zero :: Effect Unit
case_int_zero = do
  let x = 0
  let encoded = Encode.int32 x
  let decoded = Decode.int32 encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 1
    Right { val } -> assertEqual val x
  log "case_int_zero: ok"

case_int_positive :: Effect Unit
case_int_positive = do
  let x = 1000
  let encoded = Encode.int32 x
  let decoded = Decode.int32 encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 2
    Right { val } -> assertEqual val x
  log "case_int_positive: ok"

case_int_negative :: Effect Unit
case_int_negative = do
  let x = -1000
  let encoded = Encode.int32 x
  let decoded = Decode.int32 encoded 0
  case decoded of
    Left err -> do
      log $ printBytes encoded
      log $ show err
      exit 3
    Right { val } -> assertEqual val x
  log "case_int_negative: ok"

assertEqual :: forall a. Eq a => Show a => a -> a -> Effect Unit
assertEqual actual expected =
  if actual == expected then pure unit
  else do
    log $ "  actual: " <> show actual
    log $ "expected: " <> show expected
    exit 99

foreign import printBytes :: Uint8Array -> String
