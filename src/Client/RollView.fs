module internal Destiny.Client.RollView

open Destiny.Shared.Board
open Destiny.Shared.Collections
open Destiny.Shared.World
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

let private viewAnnotation text = span [ Class "annotation" ] [ str text ]

let private viewInvoke (invoke : Invoke) =
    div [ Class "roll-line" ]
        [ str " + "
          viewDie invoke.Role invoke.Result
          viewAnnotation invoke.Source ]

let private viewRoll roll =
    let baseRoll =
        div [ Class "roll-line" ]
            [ viewDie roll.Role roll.StatResult
              str <| " + " + roll.StatBase.ToString ()
              viewAnnotation roll.StatName ]
    let invokes = List.map viewInvoke roll.Invokes
    let total =
        roll.StatResult
        + roll.StatBase
        + List.sumBy (fun invoke -> invoke.Result) roll.Invokes
    let equals =
        div [] [ str <| " = " + total.ToString () ]
    div [ Class "roll" ] <| baseRoll :: invokes @ [ equals ]

let private render rolls =
    Map.joinMap viewRoll rolls.Map rolls.Order
    |> div [ Class "messages"
             Ref <| fun element ->
                 if not <| isNull element
                 then element.scrollTop <- element.scrollHeight ]

let view = FunctionComponent.Of (render, memoizeWith = equalsButFunctions)
