module private Destiny.Client

open Destiny.Shared
open Elmish
open Elmish.Bridge
open Elmish.React
open Fable.React
open Fulma

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

type private Model = { Counter : Counter option }

let private init () = { Counter = None }, Cmd.none

let private update message model =
    match model.Counter, message with
    | Some counter, Increment -> { Counter = Some { Value = counter.Value + 1 } }, Cmd.bridgeSend Increment
    | Some counter, Decrement -> { Counter = Some { Value = counter.Value - 1 } }, Cmd.bridgeSend Decrement
    | _, Set counter -> { Counter = Some counter }, Cmd.none
    | _ -> model, Cmd.none

let private show = function
    | { Counter = Some counter } -> string counter.Value
    | { Counter = None } -> "Loading...?"

let private button text onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str text ]

let private view model send =
    div []
        [ Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str <| "Press buttons to manipulate counter: " + show model ] ]
                Columns.columns []
                    [ Column.column [] [ button "-" <| fun _ -> send Decrement ]
                      Column.column [] [ button "+" <| fun _ -> send Increment ] ] ] ]

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withBridge socket
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
