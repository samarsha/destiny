namespace Destiny.Shared

module internal Map =
    let change f key map =
        match Map.tryFind key map with
        | Some value -> Map.add key (f value) map
        | None -> map

module internal List =
    let add value list = list @ [ value ]

    let insertAt index value list =
        List.take index list @ value :: List.skip index list

    let remove value = List.filter ((<>) value)
