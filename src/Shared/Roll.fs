namespace Destiny.Shared.Roll

open Destiny.Shared
open Destiny.Shared.Profile

type Invoke =
    { Entity : string
      Aspect : string
      Role : Role
      Result : int }

type Roll =
    { Id : Roll Id
      Entity : string
      Stat : string
      Role : Role
      Result : int
      Modifier : int
      Invokes : Invoke list }

type RollLog =
    { Map : Map<Roll Id, Roll>
      Order : Roll Id list }

module RollLog =
    let empty =
        { Map = Map.empty
          Order = [] }
