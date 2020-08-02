module internal Destiny.Client.BoardView

open System

open Destiny.Client
open Destiny.Client.Tabler
open Destiny.Shared
open Destiny.Shared.Bag
open Destiny.Shared.Board
open Destiny.Shared.Collections
open Destiny.Shared.Message
open Destiny.Shared.Roll
open Fable.React
open Fable.React.Props

type Model =
    private
        { Board : Board
          Role : Role
          Rolling : Roll Id option
          Editing : Entity Id option
          Drag : Drag.Model }

type PrivateMessage =
    private
    | StartRoll of Roll Id
    | StopRoll
    | StartEdit of Entity Id
    | StopEdit
    | Drag of Drag.Message

type Message =
    | Send of ClientMessage
    | Private of PrivateMessage

type private Mode = View | Edit

// Model

let empty board role =
    { Board = board
      Role = role
      Rolling = None
      Editing = None
      Drag = Drag.empty }

let private entityIndex model id = List.tryFindIndex ((=) id) model.Board.Order

let private aspectIndex model aspectId entityId =
    Map.tryFind entityId model.Board.Entities
    |> Option.bind (fun entity -> List.tryFindIndex ((=) aspectId) entity.Aspects)

let private tryFindStringId map id =
    Id.tryParse id
    |> Option.bind (fun id' -> Map.tryFind id' map)

// View

let private whenEdit mode item =
    match mode with
    | Edit -> Some item
    | View -> None

let private updateBoard = BoardMessage.create >> UpdateBoard

let private boardCommand = updateBoard >> Send

let private dragClass id model =
    if Drag.current model.Drag |> Option.contains (id.ToString ())
    then Some "drag-removed"
    else None

let private startRoll dispatch statId =
    let rollId = Id.random ()
    StartRoll rollId |> Private |> dispatch 
    RollStat (statId, rollId) |> Send |> dispatch

let private viewStat mode model dispatch (stat : Stat) =
    let name =
        match mode with
        | View -> span [ Class "stat-name" ] [ str stat.Name ]
        | Edit -> input [
            Class "stat-name"
            OnChange <| fun event -> SetStatName (stat.Id, event.Value) |> boardCommand |> dispatch
            Placeholder "Name this stat"
            Value stat.Name ]
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
          whenEdit mode <| button [ OnClick <| fun _ -> RemoveStat stat.Id |> boardCommand |> dispatch ] [ str "✖" ] ]

let private viewStatGroup mode model dispatch (group : StatGroup) =
    let name =
        match mode with
        | View -> span [ Class "stat-group-name" ] [ str group.Name ]
        | Edit -> input [
            Class "stat-group-name"
            OnChange <| fun event -> SetStatGroupName (group.Id, event.Value) |> boardCommand |> dispatch
            Placeholder "Name this group"
            Value group.Name ]
    let controls = List.choose id [
        Some name
        whenEdit mode <| button
            [ OnClick <| fun _ -> AddStat (Id.random (), group.Id) |> boardCommand |> dispatch ]
            [ str "+" ]
        whenEdit mode <| button
            [ OnClick <| fun _ -> RemoveStatGroup group.Id |> boardCommand |> dispatch ]
            [ str "✖" ] ]
    let stats = Map.joinMap (viewStat mode model dispatch) model.Board.Stats group.Stats
    div [ Class "stat-group"; Key <| group.Id.ToString () ] <|
        div [ Class "stat-group-controls" ] controls
        :: stats

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
              | Some rollId -> RollAspect (aspect.Id, rollId) |> Send |> dispatch
              | None -> () ]
        [ icon "Dice" [] ]

let private viewAspect mode model dispatch (aspect : Aspect) =
    let classes = String.concat " " <| List.choose id [ Some "aspect"; dragClass aspect.Id model ]
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
                        Value aspect.Description ]
                      [] ]
    div [ Class classes
          Key <| aspect.Id.ToString ()
          Data ("draggable", aspect.Id)
          Drag.draggableListener (Drag >> Private >> dispatch) ]
    <| List.choose id
        [ whenEdit mode <| button [ OnClick <| fun _ -> RemoveAspect aspect.Id |> boardCommand |> dispatch ] [ str "✖" ]
          Some description
          Some <| span [] (Bag.toSeq aspect.Dice |> Seq.map (viewAspectDie model dispatch aspect))
          Some <| button
              [ OnClick <| fun _ -> AddDie (aspect.Id, { Role = model.Role }) |> boardCommand |> dispatch ]
              [ icon "SquarePlus" [] ]
          Some <| button
              [ OnClick <| fun _ -> RemoveDie (aspect.Id, { Role = model.Role }) |> boardCommand |> dispatch ]
              [ icon "SquareMinus" [] ] ]

let private toggleEdit mode entityId =
    match mode with
    | View -> StartEdit entityId
    | Edit -> StopEdit

let private viewEntity model dispatch (entity : Entity) =
    let classes = String.concat " " <| List.choose id [ Some "entity"; dragClass entity.Id model ]
    let mode = if model.Editing |> Option.contains entity.Id then Edit else View
    let name =
        match mode with
        | View -> div [ Class "name" ] [ str entity.Name ]
        | Edit -> input [
            Class "name"
            Placeholder "Name this entity."
            OnChange <| fun event -> SetEntityName (entity.Id, event.Value) |> boardCommand |> dispatch
            Value entity.Name ]
    let hideButton =
        button [ OnClick <| fun _ -> CollapseEntity entity.Id |> boardCommand |> dispatch ]
               [ [] |> if entity.Collapsed then icon "ChevronDown" else icon "ChevronUp" ]
    let editButton =
        button [ OnClick <| fun _ -> toggleEdit mode entity.Id |> Private |> dispatch ]
               [ icon "Edit" [] ]
    let addGroupButton =
        button [ OnClick <| fun _ -> AddStatGroup (Id.random (), entity.Id) |> boardCommand |> dispatch ]
               [ str "+ Stat Group" ]
    let stats =
        Map.joinMap (viewStatGroup mode model dispatch) model.Board.StatGroups entity.StatGroups
        @ Option.toList (whenEdit mode addGroupButton)
    let aspects = Map.joinMap (viewAspect mode model dispatch) model.Board.Aspects entity.Aspects
    let addAspectButton =
        button [ OnClick <| fun _ ->
                     AddAspect (Id.random (), entity.Id) |> boardCommand |> dispatch
                     StartEdit entity.Id |> Private |> dispatch ]
               [ icon "Plus" [ Tabler.Size 32; Tabler.StrokeWidth 1.0 ] ]
    let content =
        [ div [ Class "stats" ] stats
          div [ Class "aspects" ] <| aspects @ [ addAspectButton ] ]
    div [ Class classes
          Key <| entity.Id.ToString ()
          Data ("draggable", entity.Id)
          Drag.draggableListener (Drag >> Private >> dispatch) ]
    <| List.choose id
        [ Some name
          Some hideButton
          Some editButton
          whenEdit mode <| button
              [ OnClick <| fun _ -> RemoveEntity entity.Id |> boardCommand |> dispatch ]
              [ str "✖" ] ]
        @ if entity.Collapsed then [] else content

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
            [ Class "add-entity"
              OnClick <| fun _ ->
                  let entityId = Id.random ()
                  AddEntity entityId |> boardCommand |> dispatch
                  StartEdit entityId |> Private |> dispatch ]
            [ icon "Plus" [ Tabler.Size 64; Tabler.StrokeWidth 1.0 ] ]
    div (upcast Class "board"
         :: Drag.areaListeners model.Drag (Drag >> Private >> dispatch))
        [ div [ Class "entities" ] <|
              Map.joinMap (viewEntity model dispatch) model.Board.Entities model.Board.Order
              @ [ addButton ]
          Drag.view (viewDrag model dispatch) model.Drag ]

let viewRollBar model dispatch =
    match model.Rolling with
    | Some _ ->
        div [ Class "active-roll" ]
            [ str "You're on a roll!"
              button [ OnClick <| fun _ -> StopRoll |> Private |> dispatch ] [ str "Finish" ] ]
    | None -> div [ Class "inactive-roll" ] []

// Update

let setBoard board model = { model with Model.Board = board }

let setRole role model = { model with Model.Role = role }

let private dragEntityCommand model (entity : Entity) =
    let targetIndex =
        Drag.targets model.Drag
        |> List.choose (Id.tryParse >> Option.bind (entityIndex model))
        |> List.tryHead
    match targetIndex with
    | Some index -> MoveEntity (entity.Id, index) |> updateBoard |> Some
    | None -> None

let private dragAspectCommand model (aspect : Aspect) =
    let tryFindTarget map = Drag.targets model.Drag |> List.choose (tryFindStringId map) |> List.tryHead
    match tryFindTarget model.Board.Entities,
          tryFindTarget model.Board.Aspects with
    | Some parent, Some target ->
        aspectIndex model target.Id parent.Id
        |> Option.map (fun index -> MoveAspect (aspect.Id, parent.Id, index) |> updateBoard)
    | Some parent, None ->
        if List.isEmpty parent.Aspects
        then MoveAspect (aspect.Id, parent.Id, 0) |> updateBoard |> Some
        else None
    | _ -> None

let private updateDrag message model =
    let model' = { model with Drag = Drag.update message model.Drag }
    let command = Drag.current model'.Drag |> Option.bind (fun id ->
        match tryFindStringId model'.Board.Entities id,
              tryFindStringId model'.Board.Aspects id with
        | Some entity', _ -> dragEntityCommand model' entity'
        | _, Some aspect' -> dragAspectCommand model' aspect'
        | _ -> None)
    model', command

let update message model =
    match message with
    | StartEdit id -> { model with Editing = Some id }, None
    | StopEdit -> { model with Editing = None }, None
    | StartRoll rollId -> { model with Rolling = Some rollId }, None
    | StopRoll -> { model with Rolling = None }, None
    | Drag message -> updateDrag message model
