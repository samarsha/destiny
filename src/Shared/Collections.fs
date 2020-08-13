namespace Destiny.Shared.Collections

module List =
    let add value xs = xs @ [ value ]

    let addIfNew value xs =
        if List.contains value xs
        then xs
        else add value xs

    let remove value = List.filter ((<>) value)

    let internal insertAt index value xs =
        List.take index xs @ value :: List.skip index xs

    let initial (xs : _ list) = xs.[.. List.length xs - 2]

module Map =
    let addIfNew key value map =
        if Map.containsKey key map
        then map
        else Map.add key value map

    let change f key map =
        match Map.tryFind key map with
        | Some value -> Map.add key (f value) map
        | None -> map

    let joinMap f map = List.choose <| fun key -> Map.tryFind key map |> Option.map f

module Option =
    let unwrap defaultValue mapper =
        Option.map mapper >> Option.defaultValue defaultValue

type OptionBuilder () =
    member _.Bind (option, binder) = Option.bind binder option

    member _.Return value = Some value

    member _.Zero () = None

module OptionBuilder =
    let option = OptionBuilder ()
