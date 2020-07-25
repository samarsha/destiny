module internal Destiny.Client.Collections

let joinMap f map = List.choose <| fun key -> Map.tryFind key map |> Option.map f
