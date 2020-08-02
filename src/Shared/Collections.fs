namespace Destiny.Shared.Collections

module internal Map =
    let change f key map =
        match Map.tryFind key map with
        | Some value -> Map.add key (f value) map
        | None -> map

    let joinMap f map = List.choose <| fun key -> Map.tryFind key map |> Option.map f

module internal List =
    let add value xs = xs @ [ value ]

    let remove value = List.filter ((<>) value)

    let insertAt index value xs =
        List.take index xs @ value :: List.skip index xs

    let initial (xs : _ list) = xs.[.. List.length xs - 2]
