open System
open Destiny.Server
open Destiny.Server.Roll
open Destiny.Shared.Board
open Destiny.Shared.Command
open Destiny.Shared.Lens
open Destiny.Shared.World
open Elmish
open Elmish.Bridge
open Saturn
open System.IO
open Thoth.Json.Giraffe

type private Client = { Role : Role }

let private worldVar = MVar.create World.empty

let private hub = ServerHub ()

let private random = Random ()

let private init dispatch () =
    dispatch (WorldUpdated <| MVar.read worldVar)
    { Role = Player }, Cmd.none

let private updateWorld updater =
    MVar.update worldVar updater
    |> WorldUpdated
    |> hub.BroadcastClient

let private update dispatch message client =
    // TODO: If the command was a board command, broadcast to every other client but not the original client.
    let client' =
        match message with
        | UpdateBoard command ->
            over World.board (BoardCommand.update client.Role command) |> updateWorld
            client
        | RollStat (statId, rollId) ->
            rollStat random client.Role statId rollId |> updateWorld
            client
        | RollAspect (aspectId, rollId) ->
            rollAspect random (Die client.Role) aspectId rollId |> updateWorld
            client
        | SetRole role ->
            RoleChanged role |> dispatch
            { Role = role }
    client', Cmd.none

let private app =
    Bridge.mkServer Command.socket init update
    |> Bridge.withServerHub hub
    |> Bridge.run Giraffe.server

run <| application {
    url "http://0.0.0.0:8085/"
    app_config Giraffe.useWebSockets
    use_router app
    use_static (Path.GetFullPath "../Client/assets")
    use_json_serializer (ThothSerializer ())
    use_gzip
    memory_cache
}
