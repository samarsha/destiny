open Destiny.Server.MVar
open Destiny.Shared
open Destiny.Shared.Scene
open Elmish
open Elmish.Bridge
open Saturn
open System.IO
open Thoth.Json.Giraffe

let private sceneVar = MVar.create Scene.empty

let private hub = ServerHub ()

let private init send () =
    send (SetScene <| MVar.read sceneVar)
    (), Cmd.none

let private update _ message _ =
    match message with
    | SetScene scene -> MVar.update sceneVar (fun _ -> scene)
    | AddEntity -> MVar.update sceneVar (Scene.addEntity <| Scene.randomId ())
    hub.BroadcastClient (SetScene <| MVar.read sceneVar)
    (), Cmd.none

let private app =
    Bridge.mkServer Message.socket init update
    |> Bridge.withServerHub hub
    |> Bridge.run Giraffe.server

run <| application {
    url "http://0.0.0.0:8085/"
    app_config Giraffe.useWebSockets
    use_router app
    use_static (Path.GetFullPath "../Client/assets")
    use_json_serializer (ThothSerializer())
    use_gzip
    memory_cache
}
