module Destiny.Utils exposing (joinedMap)

import Dict.Any exposing (AnyDict)

joinedMap : (v -> a) -> AnyDict comparable k v -> List k -> List a
joinedMap f dict = List.filterMap (\key -> Dict.Any.get key dict |> Maybe.map f)
