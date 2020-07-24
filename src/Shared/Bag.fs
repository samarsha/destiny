namespace Destiny.Shared.Bag

type Count = int

type 'a Bag when 'a : comparison = private Bag of Map<'a, Count>

module internal Bag =
    let empty = Bag (Map.empty)

    let add value (Bag bag) =
        Map.tryFind value bag
        |> Option.map ((+) 1)
        |> Option.defaultValue 0
        |> Map.add value

    let remove value (Bag bag) =
        match Map.tryFind value bag with
        | Some count when count > 1 -> Bag <| Map.add value (count - 1) bag
        | Some _ -> Bag <| Map.remove value bag
        | None -> Bag bag

    let count key (Bag bag) = Map.tryFind key bag |> Option.defaultValue 0

    let toSeq (Bag bag) =
        Map.toSeq bag
        |> Seq.collect (fun (value, count) -> Seq.replicate count value)
