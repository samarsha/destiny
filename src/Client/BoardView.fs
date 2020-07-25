module internal Destiny.Client.BoardView

open System

open Destiny.Client.Collections
open Destiny.Shared.Bag
open Destiny.Shared.Board
open Destiny.Shared.Command
open Destiny.Shared.World
open Elmish.React
open Fable.React
open Fable.React.Props

type Model =
    private
        { Board : Board
          Role : Role
          Rolling : Roll Id option
          Editing : Entity Id option }

type PrivateMessage =
    private
    | StartRoll of Roll Id
    | StopRoll
    | ToggleEdit of Entity Id

type Message =
    | Command of ServerCommand
    | Private of PrivateMessage

type private Mode = View | Edit

// Model

let empty board role =
    { Board = board
      Role = role
      Rolling = None
      Editing = None }

// View

let private whenEdit mode item =
    match mode with
    | Edit -> Some item
    | View -> None

let private boardCommand = UpdateServerBoard >> Command

let private startRoll dispatch statId =
    let rollId = Board.randomId ()
    StartRoll rollId |> Private |> dispatch 
    RollStat (statId, rollId) |> Command |> dispatch

let private viewStat mode model dispatch (stat : Stat) =
    let name =
        match mode with
        | View -> span [ Class "stat-name" ] [ str stat.Name ]
        | Edit -> input [
            Class "stat-name"
            OnChange <| fun event -> SetStatName (stat.Id, event.Value) |> boardCommand |> dispatch
            Placeholder "Name this stat"
            valueOrDefault stat.Name ]
    let score =
        match mode with
        | View -> span [ Class "stat-score" ] [ str <| stat.Score.ToString () ]
        | Edit -> input [
            Class "stat-score"
            Type "number"
            OnChange <| fun event ->
                match Int32.TryParse event.Value with
                | true, score -> SetStatScore (stat.Id, score) |> boardCommand |> dispatch
                | _ -> ()
            valueOrDefault stat.Score ]
    let rollButton =
        button
            [ Disabled <| Option.isSome model.Rolling
              OnClick <| fun _ -> startRoll dispatch stat.Id ]
            [ str "🎲" ]
    div [ Class "stat"; Key <| stat.Id.ToString () ] <| List.choose id
        [ Some name
          Some score
          Some rollButton
          whenEdit mode <| button [ OnClick <| fun _ -> RemoveStat stat.Id |> boardCommand |> dispatch ] [ str "✖" ] ]

let private viewStatGroup mode model dispatch (group : StatGroup) =
    let name =
        match mode with
        | View -> span [ Class "stat-group-name" ] [ str group.Name ]
        | Edit -> input [
            Class "stat-group-name"
            OnChange <| fun event -> SetStatGroupName (group.Id, event.Value) |> boardCommand |> dispatch
            Placeholder "Name this group"
            valueOrDefault group.Name ]
    let controls = List.choose id [
        Some name
        whenEdit mode <| button
            [ OnClick <| fun _ -> AddStat (Board.randomId (), group.Id) |> boardCommand |> dispatch ]
            [ str "+" ]
        whenEdit mode <| button
            [ OnClick <| fun _ -> RemoveStatGroup group.Id |> boardCommand |> dispatch ]
            [ str "✖" ] ]
    let stats = joinMap (viewStat mode model dispatch) model.Board.Stats group.Stats
    div [ Class "stat-group"; Key <| group.Id.ToString () ] <|
        div [ Class "stat-group-controls" ] controls
        :: stats

let private viewAspectDie model dispatch (aspect : Aspect) (Die role) =
    let roleClass =
        match role with
        | Player -> Class "die-player"
        | DM -> Class "die-dm"
    button
        [ roleClass
          Disabled (Option.isNone model.Rolling || model.Role <> role)
          OnClick <| fun _ ->
              match model.Rolling with
              | Some rollId -> RollAspect (aspect.Id, rollId) |> Command |> dispatch
              | None -> () ]
        [ str "🎲" ]

let private viewAspect mode model dispatch aspect =
    let description =
        match mode with
        | View -> div [ Class "aspect-description" ] [ str aspect.Description ]
        | Edit ->
            div [ Data ("autoexpand", aspect.Description) ]
                [ textarea
                      [ Class "aspect-description"
                        Placeholder "Describe this aspect."
                        OnChange <| fun event ->
                            SetAspectDescription (aspect.Id, event.Value) |> boardCommand |> dispatch
                        valueOrDefault aspect.Description ]
                      [] ]
    div [ Class "aspect"; Key <| aspect.Id.ToString () ] <| List.choose id
        [ whenEdit mode <| button [ OnClick <| fun _ -> RemoveAspect aspect.Id |> boardCommand |> dispatch ] [ str "✖" ]
          Some description
          Some <| span [] (Bag.toSeq aspect.Dice |> Seq.map (viewAspectDie model dispatch aspect))
          Some <| button [ OnClick <| fun _ -> AddDie aspect.Id |> boardCommand |> dispatch ] [ str "+" ]
          Some <| button [ OnClick <| fun _ -> RemoveDie aspect.Id |> boardCommand |> dispatch ] [ str "-" ] ]

let private viewEntity model dispatch (entity : Entity) =
    let mode = if model.Editing |> Option.contains entity.Id then Edit else View
    let name =
        match mode with
        | View -> div [ Class "name" ] [ str entity.Name ]
        | Edit -> input [
            Class "name"
            Placeholder "Name this entity."
            OnChange <| fun event -> SetEntityName (entity.Id, event.Value) |> boardCommand |> dispatch
            valueOrDefault entity.Name ]
    let hideButton =
        button
            [ OnClick <| fun _ -> CollapseEntity entity.Id |> boardCommand |> dispatch ]
            [ str <| if entity.Collapsed then "Show" else "Hide" ]
    let editButton =
        button
            [ OnClick <| fun _ -> ToggleEdit entity.Id |> Private |> dispatch ]
            [ str "📝" ]
    let addGroupButton =
        button
            [ OnClick <| fun _ -> AddStatGroup (Board.randomId (), entity.Id) |> boardCommand |> dispatch ]
            [ str "+ Stat Group" ]
    let stats =
        joinMap (viewStatGroup mode model dispatch) model.Board.StatGroups entity.StatGroups
        @ Option.toList (whenEdit mode addGroupButton)
    let aspects = joinMap (viewAspect mode model dispatch) model.Board.Aspects entity.Aspects
    let content =
        List.choose id
            [ Some <| div [ Class "stats" ] stats
              Some <| div [ Class "aspects" ] aspects
              whenEdit mode <| button
                  [ OnClick <| fun _ -> AddAspect (Board.randomId (), entity.Id) |> boardCommand |> dispatch ]
                  [ str "+ Aspect" ] ]
    div [ Class "entity"; Key <| entity.Id.ToString () ] <| List.choose id
        [ Some name
          Some hideButton
          Some editButton
          whenEdit mode <| button
              [ OnClick <| fun _ -> RemoveEntity entity.Id |> boardCommand |> dispatch ]
              [ str "✖" ] ]
        @ if entity.Collapsed then [] else content

let viewBoard model dispatch =
    div []
        [ button
              [ OnClick <| fun _ -> Board.randomId () |> AddEntity |> boardCommand |> dispatch ]
              [ str "+" ]
          div [ Class "entities" ] <|
              joinMap (viewEntity model dispatch) model.Board.Entities model.Board.Order ]

let viewRollBar model dispatch =
    match model.Rolling with
    | Some _ ->
        div [ Class "active-roll" ]
            [ str "You're on a roll!"
              button [ OnClick <| fun _ -> StopRoll |> Private |> dispatch ] [ str "Finish" ] ]
    | None -> div [ Class "inactive-roll" ] []

// Update

let setBoard board model = { model with Model.Board = board }

let update message model =
    match message with
    | ToggleEdit id ->
        let editing' = if model.Editing |> Option.contains id then None else Some id
        { model with Editing = editing' }
    | StartRoll rollId -> { model with Rolling = Some rollId }
    | StopRoll -> { model with Rolling = None }
