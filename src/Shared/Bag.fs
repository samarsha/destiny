namespace Destiny.Shared.Bag

type Count = int

type 'a Bag when 'a : comparison = private Bag of Map<'a, Count>

module Bag =
    let internal empty = Bag (Map.empty)

    let isEmpty (Bag bag) = Map.isEmpty bag

    let internal add value (Bag bag) =
        let count =
            Map.tryFind value bag
            |> Option.map ((+) 1)
            |> Option.defaultValue 1
        Bag <| Map.add value count bag

    let internal remove value (Bag bag) =
        match Map.tryFind value bag with
        | Some count when count > 1 -> Bag <| Map.add value (count - 1) bag
        | Some _ -> Bag <| Map.remove value bag
        | None -> Bag bag

    let contains value (Bag bag) = Map.containsKey value bag

    let toList (Bag bag) =
        Map.toList bag
        |> List.collect (fun (value, count) -> List.replicate count value)
