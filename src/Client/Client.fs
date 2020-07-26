module private Destiny.Client.Program

open Destiny.Client
open Destiny.Shared.Command
open Destiny.Shared.Board
open Destiny.Shared.World
open Elmish
open Elmish.Bridge
open Elmish.React
open Fable.React
open Fable.React.Props

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

type private Model =
    { World : World
      Role : Role
      BoardView : BoardView.Model }

type private Message =
    | Command of ClientCommand
    | BoardView of BoardView.Message

// Model

let private empty =
    let world = World.empty
    let role = Player
    { World = world
      Role = role
      BoardView = BoardView.empty world.Board role }

let private init () = empty, Cmd.none

// View

let private flipRole = function
    | Player -> DM
    | DM -> Player

let private view model dispatch =
    let dmCheckbox =
        label []
            [ input
                  [ Type "checkbox"
                    Checked (model.Role = DM)
                    OnChange <| fun _ -> () (* TODO: flipRole model.Role |> SetRole |> dispatch *) ]
              str "DM" ]
    let main =
        div [ Class "main" ]
            [ BoardView.viewBoard model.BoardView (BoardView >> dispatch)
              div [ Class "messages" ] [ RollView.view model.World.Rolls ] ]
    div [ Class "app" ]
        [ BoardView.viewRollBar model.BoardView (BoardView >> dispatch)
          dmCheckbox
          main ]

// Update

let private applyBoardCommand command model =
    let world' = { model.World with Board = BoardCommand.update model.Role command model.World.Board }
    { model with
          World = world'
          BoardView = BoardView.setBoard world'.Board model.BoardView }

let private applyClientCommand command model =
    match command with
    | UpdateClientBoard command' -> applyBoardCommand command' model
    | SetWorld world ->
        { model with
              World = world
              BoardView = BoardView.setBoard world.Board model.BoardView }

let private update message model =
    match message with
    | BoardView (BoardView.Command command) ->
        let model' =
            // Apply all server board commands locally before sending them.
            match command with
            | UpdateServerBoard command' -> applyBoardCommand command' model
            | _ -> model
        model', Cmd.bridgeSend command
    | BoardView (BoardView.Private message') ->
        let boardView', boardCommand = BoardView.update message' model.BoardView
        let model' = { model with BoardView = boardView' }
        let command =
            boardCommand
            |> Option.map (UpdateServerBoard >> Cmd.bridgeSend)
            |> Option.defaultValue Cmd.none
        model', command
    | Command command -> applyClientCommand command model, Cmd.none

let private bridgeConfig =
    Bridge.endpoint Command.socket
    |> Bridge.withMapping Command

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withBridgeConfig bridgeConfig
|> Program.withReactBatched "app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
