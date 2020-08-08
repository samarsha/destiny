﻿module internal Destiny.Client.BoardView

open System

open Browser.Types
open Destiny.Client
open Destiny.Client.React
open Destiny.Client.Tabler
open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Board
open Destiny.Shared.Collections
open Destiny.Shared.Message
open Destiny.Shared.Roll
open Fable.React
open Fable.React.Props

open Destiny.Shared.Collections.OptionBuilder

type Model =
    private
        { Board : Board
          Role : Role
          Rolling : Roll Id option
          Editing : Entity Id option
          Drag : Drag.Model }

type Event =
    | Nothing
    | Send of ClientMessage

type Message =
    private
    | StartRoll of Roll Id
    | StopRoll
    | StartEdit of Entity Id
    | StopEdit
    | Drag of Drag.Message
    | Event of Event

type private Mode =
    | View
    | Edit

// Model

let empty board role =
    { Board = board
      Role = role
      Rolling = None
      Editing = None
      Drag = Drag.empty }

let private entityIndex (board : Board) id = List.tryFindIndex ((=) id) board.Order

let private aspectIndex board aspectId entityId =
    Map.tryFind entityId board.Entities
    |> Option.bind (fun entity -> List.tryFindIndex ((=) aspectId) entity.Aspects)

let private tryFindStringId map id =
    Id.tryParse id
    |> Option.bind (fun id' -> Map.tryFind id' map)

let private tryDragEntity model =
    let targetIndex target =
        match Id.tryParse target |> Option.bind (entityIndex model.Board) with
        | Some index -> Some (target, index)
        | None -> None
    option {
        let! entity = Drag.current model.Drag |> Option.bind (tryFindStringId model.Board.Entities)
        let! target, index = Drag.targets model.Drag |> List.choose targetIndex |> List.tryHead
        return target, MoveEntity (entity.Id, index)
    }

let private tryDragAspect model =
    let tryFindTarget map = Drag.targets model.Drag |> List.choose (tryFindStringId map) |> List.tryHead
    option {
        let! aspect = Drag.current model.Drag |> Option.bind (tryFindStringId model.Board.Aspects)
        let! parent = tryFindTarget model.Board.Entities
        match tryFindTarget model.Board.Aspects with
        | Some target ->
            let! index = aspectIndex model.Board target.Id parent.Id
            return target.Id.ToString (), MoveAspect (aspect.Id, parent.Id, index)
        | None ->
            if List.isEmpty parent.Aspects
            then return parent.Id.ToString (), MoveAspect (aspect.Id, parent.Id, 0)
    }

// View

let private when' condition item = if condition then Some item else None

let private command = BoardMessage.create >> UpdateBoard >> Send

let private commandEvent = command >> Event

let private dragStyle id model =
    if Drag.current model.Drag |> Option.contains (id.ToString ())
    then [ Visibility "hidden" ]
    else []

let private dragTargetClass id model =
    let isTarget id model =
        match tryDragEntity model |> Option.orElseWith (fun () -> tryDragAspect model) with
        | Some (target, _) when id = target -> true
        | _ -> false
    if isTarget id model then "drag-target" else ""

let private startRoll dispatch statId =
    let rollId = Id.random ()
    StartRoll rollId |> dispatch 
    RollStat (statId, rollId) |> Send |> Event |> dispatch

let private expandingTextarea containerProps textareaProps value =
    div (upcast Data ("autoexpand", value) :: containerProps)
        [ textarea (upcast Value value :: textareaProps) [] ]

let private viewStat mode model dispatch (stat : Stat) =
    let name =
        match mode with
        | View -> span [ Class "stat-name preserve-whitespace" ] [ str stat.Name ]
        | Edit ->
            expandingTextarea
                [ Class "stat-name" ]
                [ OnChange <| fun event -> SetStatName (stat.Id, event.Value) |> commandEvent |> dispatch
                  Placeholder "Name this stat" ]
                stat.Name
    let score =
        match mode with
        | View -> span [ Class "stat-score" ] [ str <| stat.Score.ToString () ]
        | Edit -> input [
            Class "stat-score"
            Type "number"
            OnChange <| fun event ->
                match Int32.TryParse event.Value with
                | true, score -> SetStatScore (stat.Id, score) |> commandEvent |> dispatch
                | _ -> ()
            Value stat.Score ]
    let rollButton =
        button
            [ Disabled <| Option.isSome model.Rolling
              OnClick <| fun _ -> startRoll dispatch stat.Id ]
            [ icon "Dice" [] ]
    div [ Class "stat"; Key <| stat.Id.ToString () ] <| List.choose id
        [ Some name
          Some score
          Some rollButton
          when' (mode = Edit) <| button
              [ Class "stat-remove"
                OnClick <| fun _ -> RemoveStat stat.Id |> commandEvent |> dispatch ]
              [ icon "Trash" [] ] ]

let private viewStatGroup mode model dispatch (group : StatGroup) =
    let name =
        match mode with
        | View -> span [ Class "stat-group-name preserve-whitespace" ] [ str group.Name ]
        | Edit ->
            expandingTextarea
                [ Class "stat-group-name" ]
                [ OnChange <| fun event -> SetStatGroupName (group.Id, event.Value) |> commandEvent |> dispatch
                  Placeholder "Name this group" ]
                group.Name
    let header = div [ Class "stat-group-header" ] <| List.choose id [
        Some name
        when' (mode = Edit) <| button
            [ Class "stat-remove"
              OnClick <| fun _ -> RemoveStatGroup group.Id |> commandEvent |> dispatch ]
            [ icon "Trash" [] ] ]
    let stats = Map.joinMap (viewStat mode model dispatch) model.Board.Stats group.Stats
    let addStatButton =
        button [ Class "stat-add"
                 OnClick <| fun _ -> AddStat (Id.random (), group.Id) |> commandEvent |> dispatch ]
               [ icon "Plus" []
                 label [] [ str "Stat" ] ]
    div [ Class "stat-group"; Key <| group.Id.ToString () ]
    <| header :: stats @ Option.toList (when' (mode = Edit) addStatButton)

let private viewAspectDie model dispatch (aspect : Aspect) (die : Die) =
    let roleClass =
        match die.Role with
        | Player -> Class "die-player"
        | DM -> Class "die-dm"
    button
        [ roleClass
          Disabled (Option.isNone model.Rolling || model.Role <> die.Role)
          OnClick <| fun _ ->
              match model.Rolling with
              | Some rollId -> RollAspect (aspect.Id, rollId) |> Send |> Event |> dispatch
              | None -> () ]
        [ icon "Dice" [] ]

let private viewAspect mode model dispatch (aspect : Aspect) =
    let dice =
        List.choose id
            [ when' (not <| Bag.isEmpty aspect.Dice) <| button
                  [ Class "die-control"
                    Title "Remove a free invoke"
                    OnClick <| fun _ -> RemoveDie (aspect.Id, { Role = model.Role }) |> commandEvent |> dispatch ]
                  [ icon "SquareMinus" [] ]
              Some <| button
                  [ Class "die-control"
                    Title "Add a free invoke"
                    OnClick <| fun _ -> AddDie (aspect.Id, { Role = model.Role }) |> commandEvent |> dispatch ]
                  [ icon "SquarePlus" [] ] ]
            @ (Bag.toList aspect.Dice |> List.map (viewAspectDie model dispatch aspect))
        |> div [ Class "aspect-dice"
                 ref <| fun element ->
                     let element' = element :?> HTMLElement
                     let onFirstLine = element'.offsetTop < element'.offsetHeight / 2.0
                     element.classList.toggle ("aspect-dice-top", onFirstLine) |> ignore ]
    let description =
        match mode with
        | View ->
            [ span [ Class "aspect-description preserve-whitespace" ]
                   [ str aspect.Description ]
              dice ]
        | Edit ->
            [ expandingTextarea
                  [ Class "aspect-description" ]
                  [ Placeholder "Describe this aspect."
                    OnChange <| fun event -> SetAspectDescription (aspect.Id, event.Value) |> commandEvent |> dispatch ]
                  aspect.Description ]
    List.map Some description
    @ [ when' (mode = Edit) <| button
            [ Class "aspect-remove"
              OnClick <| fun _ -> RemoveAspect aspect.Id |> commandEvent |> dispatch ]
            [ icon "Trash" [] ]
        when' (mode = Edit) dice ]
    |> List.choose id
    |> div [ Class <| "aspect " + dragTargetClass (aspect.Id.ToString ()) model
             Style <| dragStyle aspect.Id model
             Key <| aspect.Id.ToString ()
             Data ("draggable", aspect.Id)
             Drag.draggableListener (Drag >> dispatch) ]

let private toggleEdit mode entityId =
    match mode with
    | View -> StartEdit entityId
    | Edit -> StopEdit

let private viewEntity model dispatch (entity : Entity) =
    let mode = if model.Editing |> Option.contains entity.Id then Edit else View
    let name =
        match mode with
        | View -> span [ Class "entity-name preserve-whitespace" ] [ str entity.Name ]
        | Edit ->
            expandingTextarea
                [ Class "entity-name" ]
                [ Placeholder "Name this entity"
                  OnChange <| fun event -> SetEntityName (entity.Id, event.Value) |> commandEvent |> dispatch ]
                entity.Name
    let hideButton =
        button [ OnClick <| fun _ -> SetEntityCollapsed (entity.Id, not entity.Collapsed) |> commandEvent |> dispatch ]
               [ [] |> if entity.Collapsed then icon "ChevronDown" else icon "ChevronUp" ]
    let editButton =
        button [ OnClick <| fun _ -> toggleEdit mode entity.Id |> dispatch ]
               [ icon "Edit" [] ]
    let toolbar =
        List.choose id
            [ when' (mode = Edit) <| button
                  [ OnClick <| fun _ -> RemoveEntity entity.Id |> commandEvent |> dispatch ]
                  [ icon "Trash" [] ]
              Some editButton
              Some hideButton ]
    let addGroupButton =
        button [ Class "stat-add"
                 OnClick <| fun _ -> AddStatGroup (Id.random (), entity.Id) |> commandEvent |> dispatch ]
               [ icon "FolderPlus" []
                 label [] [ str "Stat Group" ] ]
    let stats =
        Map.joinMap (viewStatGroup mode model dispatch) model.Board.StatGroups entity.StatGroups
        @ Option.toList (when' (mode = Edit) addGroupButton)
    let aspects = Map.joinMap (viewAspect mode model dispatch) model.Board.Aspects entity.Aspects
    let addAspectButton =
        button [ Class "aspect-add"
                 OnClick <| fun _ ->
                     AddAspect (Id.random (), entity.Id) |> commandEvent |> dispatch
                     StartEdit entity.Id |> dispatch ]
               [ icon "Plus" []
                 label [] [ str "Aspect" ] ]
    div [ Class <| "entity " + dragTargetClass (entity.Id.ToString ()) model
          Style <| dragStyle entity.Id model
          Key <| entity.Id.ToString ()
          Data ("draggable", entity.Id)
          Drag.draggableListener (Drag >> dispatch) ]
    <| (div [ Class "entity-header" ] <| name :: toolbar)
    :: if entity.Collapsed then []
       else [ div [ Class "stats" ] stats
              div [ Class "aspects" ] <| aspects @ [ addAspectButton ] ]

let private viewDrag model dispatch id =
    let tryView source viewer =
        tryFindStringId source id
        |> Option.map (viewer { model with Drag = Drag.empty } dispatch)
    tryView model.Board.Entities viewEntity
    |> Option.orElseWith (fun () -> tryView model.Board.Aspects <| viewAspect View)
    |> Option.defaultWith (fun () -> failwith <| "Invalid drag ID: " + id)

let viewBoard model dispatch =
    let addButton =
        button
            [ Class "entity-add"
              Style [ FontSize "20pt" ]
              OnClick <| fun _ ->
                  let entityId = Id.random ()
                  AddEntity entityId |> commandEvent |> dispatch
                  StartEdit entityId |> dispatch ]
            [ icon "Plus" [ Tabler.Size 38; Tabler.StrokeWidth 1.0 ]
              label [] [ str "Entity" ] ]
    div (upcast Class "board"
         :: Drag.areaListeners model.Drag (Drag >> dispatch))
        [ div [ Class "entities" ] <|
              Map.joinMap (viewEntity model dispatch) model.Board.Entities model.Board.Order
              @ [ addButton ]
          Drag.view (viewDrag model dispatch) model.Drag ]

let viewRollBar model dispatch =
    match model.Rolling with
    | Some _ ->
        div [ Class "active-roll" ]
            [ str "You're on a roll!"
              button [ OnClick <| fun _ -> StopRoll |> dispatch ] [ icon "Checkbox" [] ] ]
    | None -> div [ Class "inactive-roll" ] []

// Update

let setBoard board model = { model with Model.Board = board }

let setRole role model = { model with Model.Role = role }

let private updateDrag message model =
    let drag', event = Drag.update message model.Drag
    let event' =
        match event with
        | Drag.Drop ->
            tryDragEntity model
            |> Option.orElseWith (fun () -> tryDragAspect model)
            |> Option.map (snd >> command)
            |> Option.defaultValue Nothing
        | Drag.Nothing -> Nothing
    { model with Drag = drag' }, event'

let update message model =
    match message with
    | StartEdit id -> { model with Editing = Some id }, Nothing
    | StopEdit -> { model with Editing = None }, Nothing
    | StartRoll rollId -> { model with Rolling = Some rollId }, Nothing
    | StopRoll -> { model with Rolling = None }, Nothing
    | Drag message -> updateDrag message model
    | Event event -> model, event
