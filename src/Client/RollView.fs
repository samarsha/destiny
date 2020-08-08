﻿module internal Destiny.Client.RollView

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
    div [ Class "roll-line" ]
        [ str " + "
          viewDie invoke.Role invoke.Result
          span [ Class "annotation preserve-whitespace"
                 Title invoke.Entity ]
               [ str invoke.Aspect ] ]

let private viewRoll roll =
    let baseRoll =
        div [ Class "roll-line" ]
            [ viewDie roll.Role roll.Result
              str <| " + " + roll.Modifier.ToString ()
              span [ Class "annotation preserve-whitespace"
                     Title roll.Entity ]
                   [ str roll.Stat ] ]
    let invokes = List.map viewInvoke roll.Invokes
    let total =
        roll.Result +
        roll.Modifier +
        (roll.Invokes |> List.sumBy (fun invoke -> invoke.Result))
    let equals =
        div [] [ str <| " = " + total.ToString () ]
    div [ Class "roll" ] <|
        span [] [ str roll.Entity ]
        :: baseRoll
        :: invokes
        @ [ equals ]

let private render rolls =
    Map.joinMap viewRoll rolls.Map rolls.Order
    |> div [ Class "messages"
             ref <| fun element -> element.scrollTop <- element.scrollHeight ]

let view = FunctionComponent.Of (render, memoizeWith = equalsButFunctions)
