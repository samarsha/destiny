module Destiny.Utils exposing (decodeAnyDict, joinedMap)

import Dict
import Dict.Any exposing (AnyDict)
import Json.Decode exposing (Decoder)

joinedMap : (v -> a) -> AnyDict comparable k v -> List k -> List a
joinedMap f dict = List.filterMap (\key -> Dict.Any.get key dict |> Maybe.map f)

decodeAnyDict :
    (String -> v -> Maybe k)
    -> (k -> comparable)
    -> Decoder v
    -> Decoder (AnyDict comparable k v)
decodeAnyDict toKey toComparable valueDecoder =
  let
    insert key value dict = case toKey key value of
      Just k -> dict |> Dict.Any.insert k value |> Json.Decode.succeed
      Nothing -> "Key '" ++ key ++ "' cannot be decoded." |> Json.Decode.fail
    create key value acc = acc |> Json.Decode.andThen (insert key value)
  in
    Json.Decode.dict valueDecoder
    |> Json.Decode.andThen (Dict.foldr create (Dict.Any.empty toComparable |> Json.Decode.succeed))
