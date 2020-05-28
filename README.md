# purescript-protobuf

Protobuf encoding/decoding library for PureScript

## Install

### spago

packages.dhall:
```
let additions =
      { protobuf =
        { dependencies = [ "integers", "arrays", "arraybuffer-types" ]
        , repo = "https://github.com/zero-deps/purescript-protobuf.git"
        , version = "1.1.5"
        }
      }
```

### bower

`bower.json`:
```json
{
  "dependencies": {
    "purescript-protobuf": "https://github.com/zero-deps/purescript-protobuf.git#1.1.5"
  }
}
```

## Build & Test

```bash
bin/com
bin/tes
spago repl
```

## Examples

```purescript
import Data.Either (Either(Left, Right))
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Prelude
import Proto.Decode as Decode
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll, fromArray)

type PageSeo = { descr :: String, order :: Number }
type PageSeo' = { descr :: Maybe String, order :: Maybe Number }

encodePageSeo :: PageSeo -> Uint8Array
encodePageSeo msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.descr
        , Encode.uint32 17
        , Encode.double msg.order
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]

decodePageSeo :: Uint8Array -> Int -> Decode.Result PageSeo
decodePageSeo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { descr: Nothing, order: Nothing } pos
  case val of
    { descr: Just descr, order: Just order } -> pure { pos: pos1, val: { descr, order } }
    _ -> Left $ Decode.MissingFields "PageSeo"
    where
    decode :: Int -> PageSeo' -> Int -> Decode.Result PageSeo'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { descr = Just val }) pos3
              2 ->
                case Decode.double _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { order = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }
```
