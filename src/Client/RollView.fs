﻿module internal Destiny.Client.RollView

open Destiny.Client.React
open Destiny.Shared
open Fable.React
open Fable.React.Props

type ViewModel =
    { Rolls : RollLog
      ActiveRoll : Roll Id option }

type Message =
    | ContinueRoll of Roll Id

let private viewDie team result =
    let teamClass =
        match team with
        | Player -> "die-player"
        | DM -> "die-dm"
    let resultClass =
        match result with
        | 1 -> Some "die-bad"
        | 6 -> Some "die-good"
        | _ -> None
    let classes =
        "die" :: teamClass :: Option.toList resultClass
        |> String.concat " "
        |> Class
    span [ classes ] [ str <| result.ToString () ]

let private viewInvoke (invoke : Invoke) =
    div [ Class "roll-row" ]
        [ span [ Class "roll-result" ]
               [ str " + "
                 viewDie invoke.Team invoke.Result ]
          span [ Class "roll-annotation preserve-whitespace"
                 Title invoke.Entity ]
               [ str invoke.Aspect ] ]

let private viewRoll dispatch model (roll : Roll) =
    let teamClass =
        match roll.Team with
        | Player -> "roll-entity-player"
        | DM -> "roll-entity-dm"
    let baseRoll =
        div [ Class "roll-row" ]
            [ span [ Class "roll-result" ]
                   [ viewDie roll.Team roll.Result
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
        else
            div [ Class "roll-row" ]
                [ span [ Class "roll-result" ] [ str <| " = " + total.ToString () ] ]
            |> Some
    h3 [ Class <| "roll-entity " + teamClass ] [ str roll.Entity ]
    :: baseRoll
    :: invokes
    @ Option.toList equals
    |> div [ Class <| "roll " + if Option.contains roll.Id model.ActiveRoll then "roll-active" else ""
             OnClick <| fun _ -> ContinueRoll roll.Id |> dispatch ]

let private render (dispatch, model) =
    (model.Rolls.Map, model.Rolls.Order)
    ||> Map.innerJoinKey (viewRoll dispatch model)
    |> div [ Class "messages"
             ref <| fun element -> element.scrollTop <- element.scrollHeight ]

let view = FunctionComponent.Of (render, memoizeWith = equalsButFunctions)
