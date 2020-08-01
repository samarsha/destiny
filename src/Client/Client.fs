module private Destiny.Client.Program

open Destiny.Client
open Destiny.Shared.Board
open Destiny.Shared.Collections
open Destiny.Shared.Message
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
    | Receive of ServerMessage
    | Send of ClientMessage
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

let private applyBoardCommand model command =
    let board' = BoardCommand.update command model.World.Board
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

/// Applies a message sent by the client to the server.
let private applyClientMessage model = function
    | UpdateBoard message ->
        // Apply board commands before sending them to the server to make typing, dragging, etc. more responsive.
        let model' = applyBoardCommand model message.Command
        { model' with Unconfirmed = List.add message model'.Unconfirmed }
    | _ -> model

/// Applies a message received by the client from the server.
let private applyServerMessage model = function
    | ClientConnected world ->
        { model with
              World = world
              ServerBoard = world.Board
              Unconfirmed = []
              BoardView = BoardView.setBoard world.Board model.BoardView }
    | BoardUpdated message ->
        let model' = { model with ServerBoard = BoardCommand.update message.Command model.ServerBoard }
        if List.contains message model.Unconfirmed
        then { model' with Unconfirmed = List.remove message model'.Unconfirmed }
        else reapplyUnconfirmed model'
    | RollLogUpdated rollLog -> { model with World = { model.World with Rolls = rollLog } }
    | RoleChanged role -> { model with Role = role }

let private update message model =
    match message with
    | Receive serverMessage -> applyServerMessage model serverMessage, Cmd.none
    | Send clientMessage
    | BoardView (BoardView.Send clientMessage) -> applyClientMessage model clientMessage, Cmd.bridgeSend clientMessage
    | BoardView (BoardView.Private message') ->
        let boardView, clientMessage = BoardView.update message' model.BoardView
        let model' = { model with BoardView = boardView }
        let model'' = clientMessage |> Option.map (applyClientMessage model') |> Option.defaultValue model'
        let command = clientMessage |> Option.map Cmd.bridgeSend |> Option.defaultValue Cmd.none
        model'', command

let private bridgeConfig =
    Bridge.endpoint Message.socket
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
