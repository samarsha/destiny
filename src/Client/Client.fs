module private Destiny.Client

open Destiny.Shared
open Destiny.Shared.Scene
open Elmish
open Elmish.Bridge
open Elmish.React
open Fable.React
open Fulma

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let private init () = Scene.empty, Cmd.none

let private update message model =
    match message with
    | SetScene scene -> scene, Cmd.none
    | AddEntity -> model, Cmd.bridgeSend AddEntity

let private show = sprintf "%A"

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
                    [ Heading.h3 [] [ str <| show model ] ]
                Columns.columns []
                    [ Column.column [] [ button "+" <| fun _ -> send AddEntity ] ] ] ]

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withBridge Message.socket
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
