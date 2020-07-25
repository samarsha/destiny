module internal Destiny.Client.BoardView

open System

open Destiny
open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Board
open Elmish.React
open Fable.React
open Fable.React.Props

type Model =
    private
        { Board : Board
          Role : Role
          Editing : Entity Id option }

type Event =
    private
    | ToggleEdit of Entity Id

type Message =
    | Command of Shared.Command
    | Event of Event

type private Mode = View | Edit

// Model

let empty board role =
    { Board = board
      Role = role
      Editing = None }

// View

let private joinMap f map = List.choose <| fun key -> Map.tryFind key map |> Option.map f

let private whenEdit mode item =
    match mode with
    | Edit -> Some item
    | View -> None

let private viewStat mode model dispatch (stat : Stat) =
    let name =
        match mode with
        | View -> span [ Class "stat-name" ] [ str stat.Name ]
        | Edit -> input [
            Class "stat-name"
            OnChange <| fun event -> SetStatName (stat.Id, event.Value) |> Command |> dispatch
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
                | true, score -> SetStatScore (stat.Id, score) |> Command |> dispatch
                | _ -> ()
            valueOrDefault stat.Score ]
    let rollButton = button [] [ str "🎲" ]
    div [ Class "stat"; Key <| stat.Id.ToString () ] <| List.choose id
        [ Some name
          Some score
          Some rollButton
          whenEdit mode <| button [ OnClick <| fun _ -> RemoveStat stat.Id |> Command |> dispatch ] [ str "✖" ] ]

let private viewStatGroup mode model dispatch (group : StatGroup) =
    let name =
        match mode with
        | View -> span [ Class "stat-group-name" ] [ str group.Name ]
        | Edit -> input [
            Class "stat-group-name"
            OnChange <| fun event -> SetStatGroupName (group.Id, event.Value) |> Command |> dispatch
            Placeholder "Name this stat group"
            valueOrDefault group.Name ]
    let controls = List.choose id [
        Some name
        whenEdit mode <| button
            [ OnClick <| fun _ -> AddStat (Board.randomId (), group.Id) |> Command |> dispatch ]
            [ str "+" ]
        whenEdit mode <| button [ OnClick <| fun _ -> RemoveStatGroup group.Id |> Command |> dispatch ] [ str "✖" ] ]
    let stats = joinMap (viewStat mode model dispatch) model.Board.Stats group.Stats
    div [ Class "stat-group"; Key <| group.Id.ToString () ] <|
        div [ Class "stat-group-controls" ] controls
        :: stats

let private viewAspectDie model dispatch aspect (Die role) =
    let roleClass =
        match role with
        | Player -> Class "die-player"
        | DM -> Class "die-dm"
    button [ roleClass ] [ str "🎲" ]

let private viewAspect mode model dispatch aspect =
    let description =
        match mode with
        | View -> div [ Class "aspect-description" ] [ str aspect.Description ]
        | Edit ->
            div [ Data ("autoexpand", aspect.Description) ]
                [ textarea
                      [ Class "aspect-description"
                        Placeholder "Describe this aspect."
                        OnChange <| fun event -> SetAspectDescription (aspect.Id, event.Value) |> Command |> dispatch
                        valueOrDefault aspect.Description ]
                      [] ]
    div [ Class "aspect"; Key <| aspect.Id.ToString () ] <| List.choose id
        [ whenEdit mode <| button [ OnClick <| fun _ -> RemoveAspect aspect.Id |> Command |> dispatch ] [ str "✖" ]
          Some description
          Some <| span [] (Bag.toSeq aspect.Dice |> Seq.map (viewAspectDie model dispatch aspect))
          Some <| button [ OnClick <| fun _ -> AddDie aspect.Id |> Command |> dispatch ] [ str "+" ]
          Some <| button [ OnClick <| fun _ -> RemoveDie aspect.Id |> Command |> dispatch ] [ str "-" ] ]

let private viewEntity model dispatch entity =
    let mode = if model.Editing |> Option.contains entity.Id then Edit else View
    let name =
        match mode with
        | View -> div [ Class "name" ] [ str entity.Name ]
        | Edit -> input [
            Class "name"
            Placeholder "Name this entity."
            OnChange <| fun event -> SetEntityName (entity.Id, event.Value) |> Command |> dispatch
            valueOrDefault entity.Name ]
    let hideButton =
        button
            [ OnClick <| fun _ -> CollapseEntity entity.Id |> Command |> dispatch ]
            [ str <| if entity.Collapsed then "Show" else "Hide" ]
    let editButton =
        button
            [ OnClick <| fun _ -> ToggleEdit entity.Id |> Event |> dispatch ]
            [ str "📝" ]
    let addGroupButton =
        button
            [ OnClick <| fun _ -> AddStatGroup (Board.randomId (), entity.Id) |> Command |> dispatch ]
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
                  [ OnClick <| fun _ -> AddAspect (Board.randomId (), entity.Id) |> Command |> dispatch ]
                  [ str "+ Aspect" ] ]
    div [ Class "entity"; Key <| entity.Id.ToString () ] <| List.choose id
        [ Some name
          Some hideButton
          Some editButton
          whenEdit mode <| button [ OnClick <| fun _ -> RemoveEntity entity.Id |> Command |> dispatch ] [ str "✖" ] ]
        @ if entity.Collapsed then [] else content

let view model send =
    div []
        [ button
              [ OnClick <| fun _ -> Board.randomId () |> AddEntity |> Command |> send ]
              [ str "+" ]
          div [ Class "entities" ] <|
              joinMap (viewEntity model send) model.Board.Entities model.Board.Sequence ]

// Update

let setBoard board model = { model with Board = board }

let update event model =
    match event with
    | ToggleEdit id ->
        let editing' = if model.Editing |> Option.contains id then None else Some id
        { model with Editing = editing' }
