module Destiny.AnyBag exposing (AnyBag, Count, decode, empty, encode, values)

import Destiny.Utils exposing (decodeAnyDict)
import Dict.Any exposing (AnyDict)
import Json.Decode
import Json.Encode

type alias Count = Int

type AnyBag comparable a = AnyBag (AnyDict comparable a Count)

empty : (a -> comparable) -> AnyBag comparable a
empty = Dict.Any.empty >> AnyBag

values : AnyBag comparable a -> List a
values (AnyBag dict) =
  Dict.Any.toList dict
  |> List.concatMap (\(value, count) -> List.repeat count value)

decode : (String -> Maybe a) -> (a -> comparable) -> Json.Decode.Decoder (AnyBag comparable a)
decode toValue toComparable =
  decodeAnyDict (\v _ -> toValue v) toComparable Json.Decode.int
  |> Json.Decode.map AnyBag

encode : (a -> String) -> AnyBag comparable a -> Json.Encode.Value
encode toString (AnyBag dict) = Dict.Any.encode toString Json.Encode.int dict
