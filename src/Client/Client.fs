module private Destiny.Client.Program

open Destiny.Client
open Destiny.Shared.Collections
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
      ServerBoard : Board
      Unconfirmed : BoardMessage list
      Role : Role
      BoardView : BoardView.Model }

type private Message =
    | Receive of ClientCommand
    | Send of ServerCommand
    | BoardView of BoardView.Message

// Model

let private empty =
    let world = World.empty
    let role = Player
    { World = world
      ServerBoard = Board.empty
      Unconfirmed = []
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
                    OnChange <| fun _ -> flipRole model.Role |> SetRole |> Send |> dispatch ]
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

let private applyBoardMessage model command =
    let board' = BoardCommand.update command.Command model.World.Board
    { model with
          World = { model.World with Board = board' }
          BoardView = BoardView.setBoard board' model.BoardView }

let private reapplyUnconfirmed model =
    let board' =
        model.Unconfirmed
        |> List.map (fun message -> BoardCommand.update message.Command)
        |> List.fold (|>) model.ServerBoard
    { model with
          World = { model.World with Board = board' }
          BoardView = BoardView.setBoard board' model.BoardView }

/// Applies a command sent from the client to the server.
let private applyServerCommand model = function
    // Apply board commands locally before sending them to the server to make typing, dragging, etc. more responsive.
    | UpdateBoard command ->
        let model' = applyBoardMessage model command
        { model' with Unconfirmed = List.add command model'.Unconfirmed }
    | _ -> model

/// Applies a command received by the client from the server.
let private applyClientCommand model = function
    | BoardUpdated message ->
        let model' = { model with ServerBoard = BoardCommand.update message.Command model.ServerBoard }
        if List.contains message model.Unconfirmed
        then { model' with Unconfirmed = List.remove message model'.Unconfirmed }
        else reapplyUnconfirmed model'
    | WorldInitialized world ->
        { model with
              World = world
              ServerBoard = world.Board
              Unconfirmed = []
              BoardView = BoardView.setBoard world.Board model.BoardView }
    | RollLogUpdated rollLog -> { model with World = { model.World with Rolls = rollLog } }
    | RoleChanged role -> { model with Role = role }

let private update message model =
    match message with
    | Receive command -> applyClientCommand model command, Cmd.none
    | Send command
    | BoardView (BoardView.Command command) -> applyServerCommand model command, Cmd.bridgeSend command
    | BoardView (BoardView.Private message') ->
        let boardView, serverCommand = BoardView.update message' model.BoardView
        let model' = { model with BoardView = boardView }
        let model'' = serverCommand |> Option.map (applyServerCommand model') |> Option.defaultValue model'
        let command = serverCommand |> Option.map Cmd.bridgeSend |> Option.defaultValue Cmd.none
        model'', command

let private bridgeConfig =
    Bridge.endpoint Command.socket
    |> Bridge.withMapping Receive

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
