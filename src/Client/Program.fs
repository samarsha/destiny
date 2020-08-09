module private Destiny.Client.Program

open Destiny.Client
open Destiny.Client.Tabler
open Destiny.Shared
open Destiny.Shared.Board
open Destiny.Shared.Collections
open Destiny.Shared.Message
open Destiny.Shared.Roll
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
    { ActiveRoll : Roll Id option
      Board : Board
      BoardView : BoardView.Model
      Connected : bool
      Role : Role
      Rolls : RollLog
      ServerBoard : Board
      Unconfirmed : BoardMessage list }

type private Message =
    | BoardView of BoardView.Message
    | Disconnected
    | Receive of ServerMessage
    | Send of ClientMessage
    | SetActiveRoll of Roll Id option

// Model

let private empty =
    let role = Player
    { ActiveRoll = None
      Board = Board.empty
      BoardView = BoardView.empty
      Connected = false
      Role = role
      Rolls = RollLog.empty
      ServerBoard = Board.empty
      Unconfirmed = [] }

let private init () = empty, Cmd.none

// View

let private flipRole = function
    | Player -> DM
    | DM -> Player

let private view model dispatch =
    let connection =
        if model.Connected
        then div [ Class "connected" ] []
        else div [ Class "connecting" ] [ str "Trying to connect..." ]
    let spareRollButton =
        button [ Title "Roll a spare die"
                 OnClick <| fun _ ->
                     let rollId = model.ActiveRoll |> Option.defaultWith Id.random
                     Some rollId |> SetActiveRoll |> dispatch
                     RollSpare rollId |> Send |> dispatch ]
               [ icon "Dice" [ Tabler.Size 32 ] ]
    let dmCheckbox =
        label [ Class "dm-toggle" ]
            [ input [ Type "checkbox"
                      Checked (model.Role = DM)
                      OnChange <| fun _ -> flipRole model.Role |> SetRole |> Send |> dispatch ]
              str "DM" ]
    let toolbar =
        div [ Class "toolbar" ]
            [ button [ OnClick <| fun _ -> Send Undo |> dispatch ] [ icon "ArrowBackUp" [ Tabler.Size 32 ] ]
              button [ OnClick <| fun _ -> Send Redo |> dispatch ] [ icon "ArrowForwardUp" [ Tabler.Size 32 ] ]
              spareRollButton
              dmCheckbox ]
    let boardModel = BoardView.makeViewModel model.BoardView model.ActiveRoll model.Board model.Role
    let main =
        div [ Class "main" ]
            [ BoardView.viewBoard boardModel (BoardView >> dispatch)
              RollView.view model.Rolls ]
    div [ Class "app" ] <|
        [ connection
          BoardView.viewRollBar boardModel (BoardView >> dispatch)
          toolbar
          main ]

// Update

let private applyBoardCommand model command =
    { model with Board = BoardCommand.update command model.Board }

let private reapplyUnconfirmed model =
    let board =
        model.Unconfirmed
        |> List.map (fun message -> BoardCommand.update message.Command)
        |> List.fold (|>) model.ServerBoard
    { model with Board = board }

/// Applies a message sent by the client to the server.
let private applyClientMessage model = function
    | UpdateBoard message when model.Connected ->
        // Apply board commands before sending them to the server to make typing, dragging, etc. more responsive.
        let model' = applyBoardCommand model message.Command
        { model' with Unconfirmed = List.add message model'.Unconfirmed }
    | _ -> model

/// Applies a message received by the client from the server.
let private applyServerMessage model = function
    | ClientConnected (board, rolls) ->
        { model with
              Board = board
              Connected = true
              Rolls = rolls
              ServerBoard = board
              Unconfirmed = [] }
    | BoardUpdated message ->
        let model' = { model with ServerBoard = BoardCommand.update message.Command model.ServerBoard }
        if List.contains message model.Unconfirmed
        then { model' with Unconfirmed = List.remove message model'.Unconfirmed }
        else reapplyUnconfirmed model'
    | BoardReplaced board -> reapplyUnconfirmed { model with ServerBoard = board }
    | RollLogUpdated rolls -> { model with Rolls = rolls }
    | RoleChanged role -> { model with Role = role }

let rec private update message model =
    match message with
    | Receive serverMessage -> applyServerMessage model serverMessage, Cmd.none
    | Send clientMessage -> applyClientMessage model clientMessage, Cmd.bridgeSend clientMessage
    | SetActiveRoll rollId -> { model with ActiveRoll = rollId }, Cmd.none
    | BoardView message ->
        let boardView, event = BoardView.update message model.BoardView model.Board
        let model' = { model with BoardView = boardView }
        match event with
        | BoardView.Nothing -> model', Cmd.none
        | BoardView.Send clientMessage -> update (Send clientMessage) model'
        | BoardView.SetActiveRoll rollId -> { model with ActiveRoll = rollId }, Cmd.none
    | Disconnected -> { model with Connected = false }, Cmd.none

let private bridgeConfig =
    Bridge.endpoint Message.socket
    |> Bridge.withMapping Receive
    |> Bridge.withWhenDown Disconnected

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withBridgeConfig bridgeConfig
|> Program.withReactSynchronous "app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
