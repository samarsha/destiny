module private Destiny.Client.Main

open Destiny.Client
open Destiny.Shared
open Destiny.Shared.Board
open Elmish
open Elmish.Bridge
open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

type private Model =
    { Board : Board
      Role : Role
      BoardView : BoardView.Model }

type private Message =
    | Server of Command
    | Client of BoardView.Message

let private empty =
    let board = Board.empty
    let role = Player
    { Board = board
      Role = role
      BoardView = BoardView.empty board role }

let private init () = empty, Cmd.none

let private applyCommand command model =
    let board' = Command.update model.Role command model.Board
    { model with Board = board'
                 BoardView = BoardView.setBoard board' model.BoardView }

let private update message model =
    match message with
    | Client (BoardView.Command command) -> applyCommand command model, Cmd.bridgeSend command
    | Client (BoardView.Event event) ->
        let model' = { model with BoardView = BoardView.update event model.BoardView }
        model', Cmd.none
    | Server command -> applyCommand command model, Cmd.none

let private view model dispatch = BoardView.view model.BoardView (Client >> dispatch)

let private bridgeConfig =
    Bridge.endpoint Command.socket
    |> Bridge.withMapping Server

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withBridgeConfig bridgeConfig
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
