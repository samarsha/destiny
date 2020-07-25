namespace Destiny.Shared.World

open Destiny.Shared.Board

type Invoke =
    { Source : string
      Role : Role
      Result : int }

type Roll =
    { Id : Roll Id
      Role : Role
      StatName : string
      StatResult : int
      StatBase : int
      Invokes : Invoke list }

type RollLog =
    { Map : Map<Roll Id, Roll>
      Order : Roll Id list }

module internal RollLog =
    let empty =
        { Map = Map.empty
          Order = [] }

type World =
    { Board : Board
      Rolls : RollLog }

module internal World =
    let empty =
        { Board = Board.empty
          Rolls = RollLog.empty }
