open Destiny.Server
open Destiny.Shared.Board
open Destiny.Shared.Lens
open Destiny.Shared.Message
open Elmish
open Elmish.Bridge
open Giraffe.Serialization.Json
open Saturn
open System
open System.IO
open System.Threading
open Thoth.Json.Giraffe

type private Client = Client of Role

// TODO: Read a saved universe.
let private universeVar = MVar.create Universe.empty

let private saveInterval = TimeSpan.FromSeconds 30.0

let private savePath = "universe.json"

let private hub = ServerHub ()

let private random = Random ()

let private serializer = ThothSerializer () :> IJsonSerializer

let private commitBefore = function
    | AddEntity
    | RemoveEntity
    | AddStatGroup
    | RemoveStatGroup
    | AddStat
    | RemoveStat
    | AddAspect
    | RemoveAspect
    | AddDie
    | RemoveDie -> true
    | _ -> false

let private init dispatch () =
    MVar.read universeVar |> Universe.toWorld |> ClientConnected |> dispatch
    Client Player, Cmd.none

let private update dispatch message (Client role as client) =
    match message with
    | UpdateBoard message ->
        if commitBefore message.Command then Timeline.commit else id
        >> Timeline.update (BoardCommand.update message.Command)
        |> over Universe.boards
        |> MVar.update universeVar
        |> ignore
        BoardUpdated message |> hub.BroadcastClient
        client, Cmd.none
    | RollStat (statId, rollId) ->
        let universe = Universe.rollStat random role statId rollId |> MVar.update universeVar
        RollLogUpdated universe.Rolls |> hub.BroadcastClient
        client, Cmd.none
    | RollAspect (aspectId, rollId) ->
        let die = Die role
        let universe = Universe.rollAspect random die aspectId rollId |> MVar.update universeVar
        RollLogUpdated universe.Rolls |> hub.BroadcastClient
        RemoveDie (aspectId, die) |> BoardMessage.create |> BoardUpdated |> hub.BroadcastClient
        client, Cmd.none
    | Undo ->
        let universe = over Universe.boards Timeline.undo |> MVar.update universeVar
        Timeline.present universe.Boards |> BoardReplaced |> hub.BroadcastClient
        client, Cmd.none
    | Redo ->
        let universe = over Universe.boards Timeline.redo |> MVar.update universeVar
        Timeline.present universe.Boards |> BoardReplaced |> hub.BroadcastClient
        client, Cmd.none
    | SetRole role ->
        RoleChanged role |> dispatch
        Client role, Cmd.none

let private save () =
    let universe = over Universe.boards Timeline.commit |> MVar.update universeVar
    File.WriteAllText (savePath, serializer.SerializeToString universe)

new Timer ((fun _ -> save ()), (), saveInterval, saveInterval) |> ignore

let private app =
    Bridge.mkServer Message.socket init update
    |> Bridge.withServerHub hub
    |> Bridge.run Giraffe.server

run <| application {
    app_config Giraffe.useWebSockets
    memory_cache
    url "http://0.0.0.0:8085/"
    use_gzip
    use_json_serializer serializer
    use_router app
    use_static (Path.GetFullPath "../Client/assets")
}
