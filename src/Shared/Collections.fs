namespace Destiny.Shared.Collections

module internal List =
    let add value xs = xs @ [ value ]

    let remove value = List.filter ((<>) value)

    let insertAt index value xs =
        List.take index xs @ value :: List.skip index xs

    let initial (xs : _ list) = xs.[.. List.length xs - 2]

    let orElse ifEmpty = function
        | [] -> ifEmpty
        | xs -> xs

module internal Map =
    let change f key map =
        match Map.tryFind key map with
        | Some value -> Map.add key (f value) map
        | None -> map

    let joinMap f map = List.choose <| fun key -> Map.tryFind key map |> Option.map f

type internal OptionBuilder () =
    member _.Bind (option, binder) = Option.bind binder option

    member _.Return value = Some value

    member _.Zero () = None

module internal OptionBuilder =
    let option = OptionBuilder ()
