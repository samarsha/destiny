open Destiny.Server
open Destiny.Shared
open Elmish
open Elmish.Bridge
open Saturn
open System.IO
open Thoth.Json.Giraffe

let private worldVar = MVar.create { Value = 42 }

let private hub = ServerHub ()

let private init send () =
    send <| Set (MVar.read worldVar)
    (), Cmd.none

let private update _ message _ =
    match message with
    | Increment -> MVar.update worldVar (fun world -> { world with Value = world.Value + 1 })
    | Decrement -> MVar.update worldVar (fun world -> { world with Value = world.Value - 1 })
    | Set world -> MVar.update worldVar (fun _ -> world)
    hub.BroadcastClient <| Set (MVar.read worldVar)
    (), Cmd.none

let private app =
    Bridge.mkServer socket init update
    |> Bridge.withServerHub hub
    |> Bridge.run Giraffe.server

run <| application {
    url "http://0.0.0.0:8085/"
    app_config Giraffe.useWebSockets
    use_router app
    use_static (Path.GetFullPath "../Client/public")
    use_json_serializer (ThothSerializer())
    use_gzip
    memory_cache
}
