module internal Destiny.Client.BoardView

open System

open Browser.Types
open Destiny.Client.React
open Destiny.Client.Tabler
open Destiny.Shared
open Fable.React
open Fable.React.Props

open Destiny.Shared.OptionBuilder

type private BoardId =
    | AspectId of Aspect Id
    | EntityId of Entity Id
    | StatGroupId of StatGroup Id
    | StatId of Stat Id

type Model =
    private
        { Drag : Drag.Model
          Editing : Entity Id option
          JustAdded : BoardId option }

type ViewModel =
    private
        { ActiveRoll : Roll Id option
          Board : Board
          CanEdit : bool
          Catalog : Catalog
          Drag : Drag.Model
          Editing : Entity Id option
          Impersonation : Team
          JustAdded : BoardId option
          Profile : Profile option }

type Event =
    | Nothing
    | Send of ClientMessage
    | SetActiveRoll of Roll Id option

type Message =
    private
    | Drag of Drag.Message
    | Event of Event
    | StartEdit of Entity Id
    | StopEdit

type private EditMode =
    | View
    | Edit

// Model

let empty =
    { Editing = None
      Drag = Drag.empty
      JustAdded = None }

let makeViewModel (model : Model) (activeRoll, board, canEdit, catalog, impersonation, profile) =
    { ActiveRoll = activeRoll
      Board = board
      CanEdit = canEdit
      Catalog = catalog
      Drag = model.Drag
      Editing = model.Editing
      Impersonation = impersonation
      JustAdded = model.JustAdded
      Profile = profile }

let private entityIndex board id = List.tryFindIndex ((=) id) board.Entities

let private aspectIndex (catalog : Catalog) aspectId entityId =
    Map.tryFind entityId catalog.Entities
    |> Option.bind (fun entity -> List.tryFindIndex ((=) aspectId) entity.Aspects)

let private tryFindStringId map id =
    Id.tryParse id
    |> Option.bind (fun id' -> Map.tryFind id' map)

let private tryDragEntity drag (catalog : Catalog) board =
    let targetIndex target =
        let id = Id.tryParse target
        match id, Option.bind (entityIndex board) id with
        | Some id', Some index -> Some (id', index)
        | _ -> None
    option {
        let! entity = Drag.current drag |> Option.bind (tryFindStringId catalog.Entities)
        let! target, index = Drag.targets drag |> List.choose targetIndex |> List.tryHead
        return EntityId target, MoveEntity (entity.Id, board.Id, index)
    }

let private tryDragAspect drag catalog =
    let tryFindTarget map = Drag.targets drag |> List.choose (tryFindStringId map) |> List.tryHead
    option {
        let! aspect = Drag.current drag |> Option.bind (tryFindStringId catalog.Aspects)
        let! parent = tryFindTarget catalog.Entities
        match tryFindTarget catalog.Aspects with
        | Some target ->
            let! index = aspectIndex catalog target.Id parent.Id
            return AspectId target.Id, MoveAspect (aspect.Id, parent.Id, index)
        | None ->
            if List.isEmpty parent.Aspects
            then return EntityId parent.Id, MoveAspect (aspect.Id, parent.Id, 0)
    }

let private entityEditMode model = function
    | Some (entity : Entity) -> if Option.contains entity.Id model.Editing then Edit else View
    | None -> View

let shouldAddHidden model =
    Option.exists <| fun entity ->
        entity.Hidden &&
        model.Profile |> Option.exists (fun profile -> entity.User = profile.Username)

// View

let private command = WorldMessage.create >> UpdateWorld >> Send

let private commandEvent = command >> Event

let private dragStyle id drag =
    if Drag.current drag |> Option.contains (id.ToString ())
    then [ Visibility "hidden" ]
    else []

let private dragTargetClass id model =
    let isTarget id model =
        let action =
            tryDragEntity model.Drag model.Catalog model.Board
            |> Option.orElseWith (fun () -> tryDragAspect model.Drag model.Catalog)
        match action with
        | Some (target, _) when id = target -> true
        | _ -> false
    if isTarget id model then "drag-target" else ""

let private startRoll dispatch statId die =
    let rollId = Id.random ()
    Some rollId |> SetActiveRoll |> Event |> dispatch 
    RollStat (statId, rollId, die) |> Send |> Event |> dispatch

let private expandingTextarea containerProps textareaProps value =
    div (upcast Data ("autoexpand", value) :: containerProps)
        [ textarea (upcast Value value :: textareaProps) [] ]

let private viewStat model dispatch (stat : Stat) =
    let editMode = Catalog.statEntity model.Catalog stat.Id |> entityEditMode model
    let name =
        match editMode with
        | View -> span [ Class "stat-name preserve-whitespace" ] [ str stat.Name ]
        | Edit ->
            expandingTextarea
                [ Class "stat-name" ]
                [ AutoFocus <| Option.contains (StatId stat.Id) model.JustAdded
                  OnChange <| fun event -> SetStatName (stat.Id, event.Value) |> commandEvent |> dispatch
                  Placeholder "Name this stat" ]
                stat.Name
    let score =
        match editMode with
        | View -> span [ Class "stat-score" ] [ str <| stat.Score.ToString () ]
        | Edit -> input [
            Class "stat-score"
            Type "number"
            OnChange <| fun event ->
                match Int32.TryParse event.Value with
                | true, score -> SetStatScore (stat.Id, score) |> commandEvent |> dispatch
                | _ -> ()
            Value stat.Score ]
    div [ Class "stat"; Key <| stat.Id.ToString () ] <| List.choose id
        [ Some name
          Some score
          button [ Disabled (not model.CanEdit || Option.isSome model.ActiveRoll)
                   OnClick <| fun _ -> startRoll dispatch stat.Id { Team = model.Impersonation } ]
                 [ icon "Dice" [] ]
          |> Some
          button [ OnClick <| fun _ -> SetStatHidden (stat.Id, not stat.Hidden) |> commandEvent |> dispatch ]
                 [ [] |> if stat.Hidden then icon "ClosedEye" else icon "Eye" ]
          |> Option.iff
                 (editMode = Edit &&
                  model.Profile |> Option.exists (fun profile -> Catalog.isStatOwner model.Catalog profile stat.Id))
          button [ Class "stat-remove"
                   OnClick <| fun _ -> RemoveStat stat.Id |> commandEvent |> dispatch ]
                 [ icon "Trash" [] ]
          |> Option.iff (editMode = Edit) ]

let private viewObscuredStat statId =
    div [ Class "stat"
          Key <| statId.ToString () ]
        [ span [ Class "stat-name hidden" ] [ str "Secret stat" ] ]

let private viewStatGroup model dispatch (group : StatGroup) =
    let editMode = Catalog.statGroupEntity model.Catalog group.Id |> entityEditMode model
    let name =
        match editMode with
        | View -> span [ Class "stat-group-name preserve-whitespace" ] [ str group.Name ]
        | Edit ->
            expandingTextarea
                [ Class "stat-group-name" ]
                [ AutoFocus <| Option.contains (StatGroupId group.Id) model.JustAdded
                  OnChange <| fun event -> SetStatGroupName (group.Id, event.Value) |> commandEvent |> dispatch
                  Placeholder "Name this group" ]
                group.Name
    let header = div [ Class "stat-group-header" ] <| List.choose id [
        Some name
        button [ Class "stat-remove"
                 OnClick <| fun _ -> RemoveStatGroup group.Id |> commandEvent |> dispatch ]
               [ icon "Trash" [] ]
        |> Option.iff (editMode = Edit) ]
    let stats = Map.leftJoinKey viewObscuredStat (viewStat model dispatch) model.Catalog.Stats group.Stats
    let addHidden = Catalog.statGroupEntity model.Catalog group.Id |> shouldAddHidden model
    let addStatButton =
        button [ Class "stat-add"
                 OnClick <| fun _ -> AddStat (Id.random (), group.Id, addHidden) |> commandEvent |> dispatch ]
               [ icon "Plus" []
                 label [] [ str "Stat" ] ]
    header
    :: stats
    @ Option.toList (Option.iff (editMode = Edit) addStatButton)
    |> div [ Class "stat-group"
             Key <| group.Id.ToString () ]

let private viewAspectDie model dispatch (aspect : Aspect) (die : Die) =
    let teamClass =
        match die.Team with
        | Player -> Class "die-player"
        | DM -> Class "die-dm"
    button
        [ teamClass
          Disabled (not model.CanEdit || Option.isNone model.ActiveRoll || model.Impersonation <> die.Team)
          OnClick <| fun _ ->
              match model.ActiveRoll with
              | Some rollId -> RollAspect (aspect.Id, rollId, die) |> Send |> Event |> dispatch
              | None -> () ]
        [ icon "Dice" [] ]

let private viewAspect model dispatch (aspect : Aspect) =
    let editMode = Catalog.aspectEntity model.Catalog aspect.Id |> entityEditMode model
    let dice =
        List.choose id
            [ button [ Class "die-control"
                       Title "Remove a free invoke"
                       OnClick <| fun _ ->
                           RemoveDie (aspect.Id, { Team = model.Impersonation }) |> commandEvent |> dispatch ]
                     [ icon "SquareMinus" [] ]
              |> Option.iff (model.CanEdit && not <| Bag.isEmpty aspect.Dice)
              button [ Class "die-control"
                       Title "Add a free invoke"
                       OnClick <| fun _ ->
                           AddDie (aspect.Id, { Team = model.Impersonation }) |> commandEvent |> dispatch ]
                     [ icon "SquarePlus" [] ]
              |> Option.iff model.CanEdit ]
            @ (Bag.toList aspect.Dice |> List.map (viewAspectDie model dispatch aspect))
        |> div [ Class "aspect-dice"
                 ref <| fun element ->
                     let element' = element :?> HTMLElement
                     let onFirstLine = element'.offsetTop < element'.offsetHeight / 2.0
                     element.classList.toggle ("aspect-dice-top", onFirstLine) |> ignore ]
    let description =
        match editMode with
        | View ->
            [ span [ Class "aspect-description preserve-whitespace" ]
                   [ str aspect.Description ]
              dice ]
        | Edit ->
            [ expandingTextarea
                  [ Class "aspect-description" ]
                  [ AutoFocus <| Option.contains (AspectId aspect.Id) model.JustAdded
                    OnChange <| fun event -> SetAspectDescription (aspect.Id, event.Value) |> commandEvent |> dispatch
                    Placeholder "Describe this aspect." ]
                  aspect.Description ]
    let controls =
        List.choose id
            [ button [ Class "aspect-remove"
                       OnClick <| fun _ -> RemoveAspect aspect.Id |> commandEvent |> dispatch ]
                   [ icon "Trash" [] ]
              |> Option.iff (editMode = Edit)
              button [ OnClick <| fun _ -> SetAspectHidden (aspect.Id, not aspect.Hidden) |> commandEvent |> dispatch ]
                     [ [] |> if aspect.Hidden then icon "ClosedEye" else icon "Eye" ]
              |> Option.iff
                     (editMode = Edit &&
                      model.Profile
                      |> Option.exists (fun profile -> Catalog.isAspectOwner model.Catalog profile aspect.Id)) ]
        |> span [ Class "aspect-controls" ]
    description
    @ [ controls ]
    @ (dice |> Option.iff (editMode = Edit) |> Option.toList)
    |> div [ Class <| "aspect " + dragTargetClass (AspectId aspect.Id) model
             Style <| dragStyle aspect.Id model.Drag
             Key <| aspect.Id.ToString ()
             Data ("draggable", aspect.Id)
             Drag.draggableListener (Drag >> dispatch) ]

let private viewObscuredAspect aspectId =
    div [ Class "aspect"
          Key <| aspectId.ToString () ]
        [ span [ Class "aspect-description hidden" ] [ str "Secret aspect" ] ]

let private toggleEdit mode entityId =
    match mode with
    | View -> StartEdit entityId
    | Edit -> StopEdit

let private viewEntity model dispatch (entity : Entity) =
    let editMode = if model.CanEdit && model.Editing |> Option.contains entity.Id then Edit else View
    let name =
        match editMode with
        | View -> span [ Class "entity-name preserve-whitespace" ] [ str entity.Name ]
        | Edit ->
            expandingTextarea
                [ Class "entity-name" ]
                [ AutoFocus <| Option.contains (EntityId entity.Id) model.JustAdded
                  OnChange <| fun event -> SetEntityName (entity.Id, event.Value) |> commandEvent |> dispatch
                  Placeholder "Name this entity" ]
                entity.Name
    let editButton =
        button [ OnClick <| fun _ -> toggleEdit editMode entity.Id |> dispatch ]
               [ icon "Edit" [] ]
    let saveButton =
        button [ Disabled <| not model.CanEdit
                 Title <| if entity.Saved then "Don't save this entity" else "Save this entity in the sidebar"
                 OnClick <| fun _ -> SetEntitySaved (entity.Id, not entity.Saved) |> commandEvent |> dispatch ]
               [ if entity.Saved then icon "FilledStar" [] else icon "Star" [] ]
    let collapseButton =
        button [ Disabled <| not model.CanEdit
                 OnClick <| fun _ -> SetEntityCollapsed (entity.Id, not entity.Collapsed) |> commandEvent |> dispatch ]
               [ [] |> if entity.Collapsed then icon "ChevronDown" else icon "ChevronUp" ]
    let toolbar =
        List.choose id
            [ button [ OnClick <| fun _ -> RemoveEntity (entity.Id, model.Board.Id) |> commandEvent |> dispatch ]
                     [ icon "Trash" [] ]
              |> Option.iff (editMode = Edit)
              button [ OnClick <| fun _ -> SetEntityHidden (entity.Id, not entity.Hidden) |> commandEvent |> dispatch ]
                     [ [] |> if entity.Hidden then icon "ClosedEye" else icon "Eye" ]
              |> Option.iff
                     (editMode = Edit &&
                      model.Profile
                      |> Option.exists (fun profile -> Catalog.isEntityOwner model.Catalog profile entity.Id))
              Some saveButton
              editButton |> Option.iff model.CanEdit
              Some collapseButton ]
    let addGroupButton =
        button [ Class "stat-add"
                 OnClick <| fun _ -> AddStatGroup (Id.random (), entity.Id) |> commandEvent |> dispatch ]
               [ icon "FolderPlus" []
                 label [] [ str "Stat Group" ] ]
    let stats =
        Map.innerJoinKey (viewStatGroup model dispatch) model.Catalog.StatGroups entity.StatGroups
        @ Option.toList (Option.iff (editMode = Edit) addGroupButton)
    let aspects = Map.leftJoinKey viewObscuredAspect (viewAspect model dispatch) model.Catalog.Aspects entity.Aspects
    let addHidden = Some entity |> shouldAddHidden model
    let addAspectButton =
        button [ Class "aspect-add"
                 OnClick <| fun _ -> AddAspect (Id.random (), entity.Id, addHidden) |> commandEvent |> dispatch ]
               [ icon "Plus" []
                 label [] [ str "Aspect" ] ]
    div [ Class <| "entity " + dragTargetClass (EntityId entity.Id) model
          Style <| dragStyle entity.Id model.Drag
          Key <| entity.Id.ToString ()
          Data ("draggable", entity.Id)
          Drag.draggableListener (Drag >> dispatch) ]
    <| (div [ Class "entity-header" ] <| name :: toolbar)
    :: if entity.Collapsed then []
       else [ div [ Class "stats" ] stats
              div [ Class "aspects" ] <| aspects @ Option.toList (Option.iff model.CanEdit addAspectButton) ]

let private viewDrag model dispatch id =
    let tryView source viewer =
        tryFindStringId source id
        |> Option.map (viewer { model with ViewModel.Drag = Drag.empty } dispatch)
    tryView model.Catalog.Entities viewEntity
    |> Option.orElseWith (fun () -> tryView model.Catalog.Aspects viewAspect)
    |> Option.defaultWith (fun () -> failwith <| "Invalid drag ID: " + id)

let viewBoard model dispatch =
    let addButton =
        button
            [ Class "entity-add"
              Style [ FontSize "20pt" ]
              OnClick <| fun _ ->
                  match model.Profile with
                  | Some profile ->
                      AddEntity (Id.random (), model.Board.Id, profile.Username) |> commandEvent |> dispatch
                  | None -> () ]
            [ icon "Plus" [ Tabler.Size 38; Tabler.StrokeWidth 1.0 ]
              label [] [ str "Entity" ] ]
    div (upcast Class "board"
         :: Drag.areaListeners model.Drag (Drag >> dispatch))
        [ div [ Class "entities" ] <|
              Map.innerJoinKey (viewEntity model dispatch) model.Catalog.Entities model.Board.Entities
              @ Option.toList (Option.iff model.CanEdit addButton)
          Drag.view (viewDrag model dispatch) model.Drag ]

let viewRollBar model dispatch =
    match model.ActiveRoll with
    | Some _ ->
        div [ Class "active-roll" ]
            [ str "You're on a roll!"
              button [ OnClick <| fun _ -> SetActiveRoll None |> Event |> dispatch ] [ icon "Checkbox" [] ] ]
    | None -> div [ Class "inactive-roll" ] []

// Update

let private updateDrag message drag catalog board =
    let drag', event = Drag.update message drag
    let event' =
        match event with
        | Drag.Drop ->
            tryDragEntity drag catalog board
            |> Option.orElseWith (fun () -> tryDragAspect drag catalog)
            |> Option.map (snd >> command)
            |> Option.defaultValue Nothing
        | Drag.Nothing -> Nothing
    drag', event'

let private updateEvent event model =
    match event with
    | Send (UpdateWorld { Command = command }) ->
        match command with
        | AddStat (id, _, _) -> { model with Model.JustAdded = StatId id |> Some }
        | AddStatGroup (id, _) -> { model with JustAdded = StatGroupId id |> Some }
        | AddAspect (aspectId, entityId, _) ->
            { model with
                  Editing = Some entityId
                  JustAdded = AspectId aspectId |> Some }
        | AddEntity (id, _, _) ->
            { model with
                  Editing = Some id
                  JustAdded = EntityId id |> Some }
        | _ -> model
    | _ -> model

let update message (model : Model) catalog board =
    match message with
    | Drag dragMessage ->
        let drag, event = updateDrag dragMessage model.Drag catalog board
        { model with Drag = drag }, event
    | Event event -> updateEvent event model, event
    | StartEdit id -> { model with Editing = Some id }, Nothing
    | StopEdit -> { model with Editing = None; JustAdded = None }, Nothing
