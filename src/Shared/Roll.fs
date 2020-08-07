namespace Destiny.Shared.Roll

open Destiny.Shared
open Destiny.Shared.Board

type Invoke =
    { Source : string
      Role : Role
      Result : int }

type Roll =
    { Id : Roll Id
      Role : Role
      Name : string
      Result : int
      Modifier : int
      Invokes : Invoke list }

type RollLog =
    { Map : Map<Roll Id, Roll>
      Order : Roll Id list }

module internal RollLog =
    let empty =
        { Map = Map.empty
          Order = [] }
