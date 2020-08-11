module private Destiny.Client.Program

open Destiny.Client
open Destiny.Client.Tabler
open Destiny.Shared
open Destiny.Shared.Collections
open Destiny.Shared.Functions
open Destiny.Shared.Message
open Destiny.Shared.Roll
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
    { ActiveBoard : Board Id option
      ActiveRoll : Roll Id option
      BoardView : BoardView.Model
      Connected : bool
      Role : Role
      Rolls : RollLog
      ServerWorld : World
      Unconfirmed : WorldMessage list
      World : World }

type private Message =
    | BoardView of BoardView.Message
    | Disconnected
    | Receive of ServerMessage
    | Send of ClientMessage
    | SetActiveRoll of Roll Id option

// Model

let private empty =
    let role = Player
    { ActiveBoard = List.tryHead World.empty.BoardList
      ActiveRoll = None
      BoardView = BoardView.empty
      Connected = false
      Role = role
      Rolls = RollLog.empty
      ServerWorld = World.empty
      Unconfirmed = []
      World = World.empty }

let private init () = empty, Cmd.none

// View

let private flipRole = function
    | Player -> DM
    | DM -> Player

let private activeBoard model =
    model.ActiveBoard |> Option.bind (flip Map.tryFind model.World.Boards)

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
    let boardModel = activeBoard model |> Option.map (fun board ->
        BoardView.makeViewModel model.BoardView model.ActiveRoll board model.World.Catalog model.Role)
    let boardView =
        boardModel |> Option.unwrap
            (div [ Class "board" ] [])
            (BoardView >> dispatch |> flip BoardView.viewBoard)
    let rollBar =
        boardModel |> Option.unwrap
            (div [] [])
            (BoardView >> dispatch |> flip BoardView.viewRollBar)
    let main =
        div [ Class "main" ]
            [ boardView
              RollView.view model.Rolls ]
    div [ Class "app" ] <|
        [ connection
          rollBar
          toolbar
          main ]

// Update

let private applyWorldCommand model command =
    { model with World = WorldCommand.update command model.World }

let private reapplyUnconfirmed model =
    let world =
        model.Unconfirmed
        |> List.map (fun message -> WorldCommand.update message.Command)
        |> List.fold (|>) model.ServerWorld
    { model with World = world }

/// Applies a message sent by the client to the server.
let private applyClientMessage model = function
    | UpdateWorld message when model.Connected ->
        // Apply world commands before sending them to the server to make typing, dragging, etc. more responsive.
        let model' = applyWorldCommand model message.Command
        { model' with Unconfirmed = List.add message model'.Unconfirmed }
    | _ -> model

/// Applies a message received by the client from the server.
let private applyServerMessage model = function
    | ClientConnected (world, rolls) ->
        { model with
              Connected = true
              Rolls = rolls
              ServerWorld = world
              Unconfirmed = []
              World = world }
    | WorldUpdated message ->
        let model' = { model with ServerWorld = WorldCommand.update message.Command model.ServerWorld }
        if List.contains message model.Unconfirmed
        then { model' with Unconfirmed = List.remove message model'.Unconfirmed }
        else reapplyUnconfirmed model'
    | WorldReplaced world -> reapplyUnconfirmed { model with ServerWorld = world }
    | RollLogUpdated rolls -> { model with Rolls = rolls }
    | RoleChanged role -> { model with Role = role }

let rec private update message model =
    match message with
    | Receive serverMessage -> applyServerMessage model serverMessage, Cmd.none
    | Send clientMessage -> applyClientMessage model clientMessage, Cmd.bridgeSend clientMessage
    | SetActiveRoll rollId -> { model with ActiveRoll = rollId }, Cmd.none
    | BoardView message ->
        activeBoard model |> Option.unwrap (model, Cmd.none) (fun board ->
            let boardView, event = BoardView.update message model.BoardView model.World.Catalog board
            let model' = { model with BoardView = boardView }
            match event with
            | BoardView.Nothing -> model', Cmd.none
            | BoardView.Send clientMessage -> update (Send clientMessage) model'
            | BoardView.SetActiveRoll rollId -> { model with ActiveRoll = rollId }, Cmd.none)
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
