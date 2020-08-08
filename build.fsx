#r "paket: groupref build"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO

Target.initEnvironment ()

let serverPath = Path.getFullName "src/Server"

let clientDeployPath = Path.getFullName "src/Client/deploy"

let deployPath = Path.getFullName "deploy"

let platformTool tool winTool =
    if Environment.isUnix then tool else winTool
    |> ProcessUtils.tryFindFileOnPath
    |> Option.defaultWith (fun () -> tool + " was not found in your path. " |> failwith)

let yarn = platformTool "yarn" "yarn.cmd"

let runTool command args workingDir =
    let args' = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (command, args')
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let runDotNet command workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) command ""
    if result.ExitCode <> 0
    then failwithf "'dotnet %s' failed in %s" command workingDir

let openBrowser url =
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

Target.create "Clean" <| fun _ ->
    [ deployPath; clientDeployPath ] |> Shell.cleanDirs

Target.create "InstallClient" <| fun _ ->
    runTool yarn "install --frozen-lockfile" __SOURCE_DIRECTORY__

Target.create "Build" <| fun _ ->
    runDotNet "build" serverPath
    runTool yarn "webpack-cli --env.production -p" __SOURCE_DIRECTORY__

Target.create "Run" <| fun _ ->
    let server = async { runDotNet "watch run" serverPath }
    let client = async { runTool yarn "webpack-dev-server --env.development" __SOURCE_DIRECTORY__ }
    let browser =
        async {
            do! Async.Sleep 5000
            openBrowser "http://localhost:8080"
        }
    Async.Parallel [ server; client; browser ] |> Async.RunSynchronously |> ignore

Target.create "Bundle" <| fun _ ->
    let serverDir = Path.combine deployPath "Server"
    let publish = sprintf "publish -c Release -o \"%s\"" serverDir
    runDotNet publish serverPath

    let clientDir = Path.combine deployPath "Client"
    let assetsDir = Path.combine clientDir "assets"
    Shell.copyDir assetsDir clientDeployPath FileFilter.allFiles

"Clean"
    ==> "InstallClient"
    ==> "Build"
    ==> "Bundle"

"Clean"
    ==> "InstallClient"
    ==> "Run"

Target.runOrDefaultWithArguments "Build"
