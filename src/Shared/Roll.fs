namespace Destiny.Shared

type Invoke =
    { Entity : string
      Aspect : string
      Team : Team
      Result : int }

type Roll =
    { Id : Roll Id
      Entity : string
      Stat : string
      Team : Team
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
