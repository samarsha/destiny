module internal Destiny.Client.RollView

open Destiny.Client.React
open Destiny.Shared.Board
open Destiny.Shared.Collections
open Destiny.Shared.Roll
open Fable.React
open Fable.React.Props

let private viewDie role result =
    let roleClass =
        match role with
        | Player -> "die-player"
        | DM -> "die-dm"
    let resultClass =
        match result with
        | 1 -> Some "die-bad"
        | 6 -> Some "die-good"
        | _ -> None
    let classes =
        "die" :: roleClass :: Option.toList resultClass
        |> String.concat " "
        |> Class
    span [ classes ] [ str <| result.ToString () ]

let private viewInvoke (invoke : Invoke) =
    div [ Class "roll-row" ]
        [ span [ Class "roll-result" ]
               [ str " + "
                 viewDie invoke.Role invoke.Result ]
          span [ Class "roll-annotation preserve-whitespace"
                 Title invoke.Entity ]
               [ str invoke.Aspect ] ]

let private viewRoll roll =
    let roleClass =
        match roll.Role with
        | Player -> "roll-entity-player"
        | DM -> "roll-entity-dm"
    let baseRoll =
        div [ Class "roll-row" ]
            [ span [ Class "roll-result" ]
                   [ viewDie roll.Role roll.Result
                     str <| if roll.Modifier = 0 then "" else " + " + roll.Modifier.ToString () ]
              span [ Class "roll-annotation preserve-whitespace"
                     Title roll.Entity ]
                   [ str roll.Stat ] ]
    let invokes = List.map viewInvoke roll.Invokes
    let total =
        roll.Result +
        roll.Modifier +
        (roll.Invokes |> List.sumBy (fun invoke -> invoke.Result))
    let equals =
        if roll.Modifier = 0 && List.isEmpty roll.Invokes
        then None
        else div [ Class "roll-row" ] [ span [ Class "roll-result" ] [ str <| " = " + total.ToString () ] ] |> Some
    div [ Class "roll" ] <|
        h3 [ Class <| "roll-entity " + roleClass ] [ str roll.Entity ]
        :: baseRoll
        :: invokes
        @ Option.toList equals

let private render rolls =
    Map.joinMap viewRoll rolls.Map rolls.Order
    |> div [ Class "messages"
             ref <| fun element -> element.scrollTop <- element.scrollHeight ]

let view = FunctionComponent.Of (render, memoizeWith = equalsButFunctions)
