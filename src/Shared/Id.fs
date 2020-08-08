namespace Destiny.Shared

open System

type 'a Id =
    private
    | Id of Guid

    override this.ToString () = match this with Id guid -> guid.ToString () 

module Id =
    let tryParse (str : string) =
        match Guid.TryParse str with
        | true, guid -> Some <| Id guid
        | _ -> None

    let random () = Id <| Guid.NewGuid ()
