namespace Destiny.Shared

open System

type 'a Id =
    private | Id of Guid

    override this.ToString () = match this with Id guid -> guid.ToString () 

module Id =
    let tryParse (s : string) =
        match Guid.TryParse s with
        | true, guid -> Id guid |> Some
        | _ -> None

    let parse (s : string) = Guid.Parse s |> Id

    let random () = Guid.NewGuid () |> Id

    let ofBytes bytes = bytes |> List.toArray |> Guid |> Id
