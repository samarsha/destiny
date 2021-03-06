module private Destiny.Client.Program

open Browser
open Destiny.Client.Tabler
open Destiny.Shared
open Destiny.Shared.Functions
open Elmish
open Elmish.Bridge
open Elmish.React
open Fable.React
open Fable.React.Props

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

type private Sidebar =
    | RollLog
    | SaveList

type private Model =
    { ActiveBoard : Board Id option
      ActiveRoll : Roll Id option
      BoardView : BoardView.Model
      Connected : bool
      Impersonation : Team
      Login : Login.Model
      Profile : Profile option
      Rolls : RollLog
      ServerWorld : World
      Sidebar : Sidebar
      Unconfirmed : WorldMessage list
      World : World }

type private Message =
    | BoardView of BoardView.Message
    | Disconnected
    | Login of Login.Message
    | Receive of ServerMessage
    | RollView of RollView.Message
    | Send of ClientMessage
    | SetActiveRoll of Roll Id option
    | SetSidebar of Sidebar
    | TabBar of Board TabBar.Message

// Model

let private init () =
    { ActiveBoard = List.tryHead World.empty.BoardList
      ActiveRoll = None
      BoardView = BoardView.empty
      Connected = false
      Impersonation = Player
      Login = Login.init
      Profile = None
      Rolls = RollLog.empty
      ServerWorld = World.empty
      Sidebar = RollLog
      Unconfirmed = []
      World = World.empty },
    Cmd.none

let private canEdit model = model.Connected && Option.isSome model.Profile

let private team = function
    | Some (profile : Profile) -> profile.Team
    | None -> Player

let private flipTeam = function
    | Player -> DM
    | DM -> Player

// View

let private activeBoard model =
    model.ActiveBoard |> Option.bind (flip Map.tryFind model.World.Boards)

let private viewSaveList model dispatch =
    let viewItem (entity : Entity) =
        let onClick _ =
            match model.ActiveBoard with
            | Some board -> LinkEntity (entity.Id, board) |> WorldMessage.create |> UpdateWorld |> Send |> dispatch
            | None -> ()
        li [ Class "save-item" ]
           [ button [ canEdit model |> not |> Disabled
                      OnClick onClick ]
                    [ icon "UserPlus" []
                      label [] [ str entity.Name ] ] ]
    model.World.Catalog.Entities
    |> Map.filter (fun _ entity -> entity.Saved)
    |> Map.toList
    |> List.map snd
    |> List.sortBy (fun entity -> entity.Name)
    |> List.map viewItem
    |> ul [ Class "save-list" ]

let private viewSidebar model dispatch =
    let selectedClass panel = if model.Sidebar = panel then Class "sidebar-selected" else Class ""
    let selector =
        div [ Class "sidebar-selector" ]
            [ button [ selectedClass RollLog
                       OnClick <| fun _ -> SetSidebar RollLog |> dispatch ]
                     [ icon "Messages" [ Tabler.Size 32 ] ]
              button [ selectedClass SaveList
                       OnClick <| fun _ -> SetSidebar SaveList |> dispatch ]
                     [ icon "Users" [ Tabler.Size 32 ] ] ]
    let content =
        match model.Sidebar with
        | RollLog -> RollView.view (RollView >> dispatch, { Rolls = model.Rolls; ActiveRoll = model.ActiveRoll })
        | SaveList -> viewSaveList model dispatch
    div [ Class "sidebar" ]
        [ selector
          content ]

let private view model dispatch =
    let connection =
        if model.Connected
        then div [ Class "connected" ] []
        else div [ Class "connecting" ] [ str "Trying to connect..." ]
    let spareRollButton =
        button [ Title "Roll a spare die"
                 canEdit model |> not |> Disabled
                 OnClick <| fun _ ->
                     let rollId = model.ActiveRoll |> Option.defaultWith Id.random
                     Some rollId |> SetActiveRoll |> dispatch
                     RollSpare (rollId, { Team = model.Impersonation }) |> Send |> dispatch ]
               [ icon "Dice" [ Tabler.Size 32 ] ]
    let toolbar =
        div [ Class "toolbar" ]
            [ button [ canEdit model |> not |> Disabled
                       OnClick <| fun _ -> Send Undo |> dispatch ]
                     [ icon "ArrowBackUp" [ Tabler.Size 32 ] ]
              button [ canEdit model |> not |> Disabled
                       OnClick <| fun _ -> Send Redo |> dispatch ]
                     [ icon "ArrowForwardUp" [ Tabler.Size 32 ] ]
              spareRollButton
              Login.view (Login.makeViewModel model.Login model.Profile model.Impersonation) (Login >> dispatch) ]
    let boardModel = activeBoard model |> Option.map (fun board ->
        BoardView.makeViewModel model.BoardView
            (model.ActiveRoll,
             board,
             canEdit model,
             model.World.Catalog,
             model.Impersonation,
             model.Profile))
    let boardView =
        boardModel |> Option.unwrap
            (div [ Class "board" ] [])
            (BoardView >> dispatch |> flip BoardView.viewBoard)
    let rollBar =
        boardModel |> Option.unwrap
            (div [] [])
            (BoardView >> dispatch |> flip BoardView.viewRollBar)
    let tabBar =
        TabBar >> dispatch |> TabBar.view
            { TabBar.Tabs = Map.innerJoinKey id model.World.Boards model.World.BoardList
              TabBar.Active = activeBoard model
              TabBar.Format = fun board -> board.Name
              TabBar.Kind = "Board"
              TabBar.CanEdit = canEdit model } 
    div [ Class "app" ] <|
        [ connection
          rollBar
          div [ Class "main" ]
              [ div [ Class "screen" ]
                    [ toolbar
                      tabBar
                      boardView ]
                viewSidebar model dispatch ] ]

// Update

let private applyWorldCommand model command =
    { model with World = WorldCommand.update command model.World }

let private reapplyUnconfirmed model =
    let world =
        model.Unconfirmed
        |> List.map (fun message -> WorldCommand.update message.Command)
        |> List.fold (|>) model.ServerWorld
    { model with World = world }

let private validateActiveBoard model =
    let activeBoard' = model.ActiveBoard |> Option.bind (fun board ->
        if Map.containsKey board model.World.Boards
        then Some board
        else List.tryHead model.World.BoardList)
    { model with ActiveBoard = activeBoard' }

/// Applies a message sent by the client to the server.
let private applyClientMessage model = function
    | UpdateWorld message ->
        // Apply world commands before sending them to the server to make typing, dragging, etc. more responsive.
        let model' = applyWorldCommand model message.Command
        { model' with Unconfirmed = List.add message model'.Unconfirmed }
        |> validateActiveBoard
    | _ -> model

let private send model message =
    applyClientMessage model message, Cmd.bridgeSend message

/// Applies a message received by the client from the server.
let private applyServerMessage model = function
    | ClientConnected (world, rolls) ->
        let model' =
            { model with
                  ActiveBoard = List.tryHead world.BoardList
                  Connected = true
                  Impersonation = Player
                  Profile = None
                  Rolls = rolls
                  ServerWorld = world
                  Unconfirmed = []
                  World = world }
        Option.ofObj localStorage.["session"]
        |> Option.bind Id.tryParse
        |> Option.unwrap (model', Cmd.none) (RestoreSession >> send model')
    | LoginResult result ->
        match result with
        | Ok session ->
            localStorage.["session"] <- session.Id.ToString ()
            { model with
                  Impersonation = session.Profile.Team
                  Profile = Some session.Profile },
            Cmd.none
        | Error error ->
            localStorage.removeItem "session"
            window.alert error
            model, Cmd.none
    | WorldUpdated message ->
        let model' = { model with ServerWorld = WorldCommand.update message.Command model.ServerWorld }
        let model'' =
            if List.contains message model.Unconfirmed
            then { model' with Unconfirmed = List.remove message model'.Unconfirmed }
            else reapplyUnconfirmed model'
        validateActiveBoard model'', Cmd.none
    | WorldReplaced world ->
        reapplyUnconfirmed { model with ServerWorld = world }
        |> validateActiveBoard,
        Cmd.none
    | RollLogUpdated rolls ->
        { model with Rolls = rolls }, Cmd.none

let private updateLogin message model =
    let login, event = Login.update message model.Login
    let model' = { model with Login = login }
    match event with
    | Login.NoEvent -> model', Cmd.none
    | Login.LogIn (username, password) -> model', LogIn (username, password) |> Cmd.bridgeSend
    | Login.SignUp (username, password) -> model', SignUp (username, password) |> Cmd.bridgeSend
    | Login.Impersonate team -> { model' with Impersonation = team }, Cmd.none

let private updateBoardView message model =
    activeBoard model |> Option.unwrap (model, Cmd.none) (fun board ->
        let boardView, event = BoardView.update message model.BoardView model.World.Catalog board
        let model' = { model with BoardView = boardView }
        match event with
        | BoardView.Nothing -> model', Cmd.none
        | BoardView.Send clientMessage -> send model' clientMessage
        | BoardView.SetActiveRoll rollId -> { model with ActiveRoll = rollId }, Cmd.none)

let private updateTabBar (message : Board TabBar.Message) model =
    match message with
    | TabBar.AddTab ->
        let id = Id.random ()
        let model', command = AddBoard id |> WorldMessage.create |> UpdateWorld |> send model
        { model' with ActiveBoard = Some id }, command
    | TabBar.SwitchTab board ->
        { model with ActiveBoard = Some board.Id }, Cmd.none
    | TabBar.SetTabName (board, name) ->
        SetBoardName (board.Id, name) |> WorldMessage.create |> UpdateWorld |> send model
    | TabBar.RemoveTab board ->
        RemoveBoard board.Id |> WorldMessage.create |> UpdateWorld |> send model

let private update message model =
    match message with
    | BoardView boardMessage -> updateBoardView boardMessage model
    | Disconnected -> { model with Connected = false }, Cmd.none
    | Login loginMessage -> updateLogin loginMessage model
    | Receive serverMessage -> applyServerMessage model serverMessage
    | RollView (RollView.ContinueRoll rollId) -> { model with ActiveRoll = Some rollId }, Cmd.none
    | Send clientMessage -> send model clientMessage
    | SetActiveRoll rollId -> { model with ActiveRoll = rollId }, Cmd.none
    | SetSidebar sidebar -> { model with Sidebar = sidebar }, Cmd.none
    | TabBar tabMessage -> updateTabBar tabMessage model

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
