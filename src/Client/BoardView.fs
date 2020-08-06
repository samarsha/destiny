module internal Destiny.Client.BoardView

open System

open Browser.Types
open Destiny.Client
open Destiny.Client.Helpers
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

let private when' condition item = if condition then Some item else None

let private updateBoard = BoardMessage.create >> UpdateBoard

let private boardCommand = updateBoard >> Send

let private dragStyle id model =
    if Drag.current model.Drag |> Option.contains (id.ToString ())
    then [ Visibility "hidden" ]
    else []

let private startRoll dispatch statId =
    let rollId = Id.random ()
    StartRoll rollId |> Private |> dispatch 
    RollStat (statId, rollId) |> Send |> dispatch

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
                [ OnChange <| fun event -> SetStatName (stat.Id, event.Value) |> boardCommand |> dispatch
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
          when' (mode = Edit) <| button
              [ Class "stat-remove"
                OnClick <| fun _ -> RemoveStat stat.Id |> boardCommand |> dispatch ]
              [ icon "Trash" [] ] ]

let private viewStatGroup mode model dispatch (group : StatGroup) =
    let name =
        match mode with
        | View -> span [ Class "stat-group-name preserve-whitespace" ] [ str group.Name ]
        | Edit ->
            expandingTextarea
                [ Class "stat-group-name" ]
                [ OnChange <| fun event -> SetStatGroupName (group.Id, event.Value) |> boardCommand |> dispatch
                  Placeholder "Name this group" ]
                group.Name
    let header = div [ Class "stat-group-header" ] <| List.choose id [
        Some name
        when' (mode = Edit) <| button
            [ Class "stat-remove"
              OnClick <| fun _ -> RemoveStatGroup group.Id |> boardCommand |> dispatch ]
            [ icon "Trash" [] ] ]
    let stats = Map.joinMap (viewStat mode model dispatch) model.Board.Stats group.Stats
    let addStatButton =
        button [ Class "stat-add"
                 OnClick <| fun _ -> AddStat (Id.random (), group.Id) |> boardCommand |> dispatch ]
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
              | Some rollId -> RollAspect (aspect.Id, rollId) |> Send |> dispatch
              | None -> () ]
        [ icon "Dice" [] ]

let private viewAspect mode model dispatch (aspect : Aspect) =
    let dice =
        List.choose id
            [ when' (not <| Bag.isEmpty aspect.Dice) <| button
                  [ Class "die-control"
                    Title "Remove a free invoke"
                    OnClick <| fun _ -> RemoveDie (aspect.Id, { Role = model.Role }) |> boardCommand |> dispatch ]
                  [ icon "SquareMinus" [] ]
              Some <| button
                  [ Class "die-control"
                    Title "Add a free invoke"
                    OnClick <| fun _ -> AddDie (aspect.Id, { Role = model.Role }) |> boardCommand |> dispatch ]
                  [ icon "SquarePlus" [] ] ]
            @ (Bag.toList aspect.Dice |> List.map (viewAspectDie model dispatch aspect))
        |> div [ Class "aspect-dice"
                 ref' <| fun element ->
                     let element' = element :?> HTMLElement
                     let onFirstLine = element'.offsetTop < element'.offsetHeight
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
                    OnChange <| fun event -> SetAspectDescription (aspect.Id, event.Value) |> boardCommand |> dispatch ]
                  aspect.Description ]
    List.map Some description
    @ [ when' (mode = Edit) <| button
            [ Class "aspect-remove"
              OnClick <| fun _ -> RemoveAspect aspect.Id |> boardCommand |> dispatch ]
            [ icon "Trash" [] ]
        when' (mode = Edit) dice ]
    |> List.choose id
    |> div [ Class "aspect"
             Style <| dragStyle aspect.Id model
             Key <| aspect.Id.ToString ()
             Data ("draggable", aspect.Id)
             Drag.draggableListener (Drag >> Private >> dispatch) ]

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
                  OnChange <| fun event -> SetEntityName (entity.Id, event.Value) |> boardCommand |> dispatch ]
                entity.Name
    let hideButton =
        button [ OnClick <| fun _ -> SetEntityCollapsed (entity.Id, not entity.Collapsed) |> boardCommand |> dispatch ]
               [ [] |> if entity.Collapsed then icon "ChevronDown" else icon "ChevronUp" ]
    let editButton =
        button [ OnClick <| fun _ -> toggleEdit mode entity.Id |> Private |> dispatch ]
               [ icon "Edit" [] ]
    let toolbar =
        List.choose id
            [ when' (mode = Edit) <| button
                  [ OnClick <| fun _ -> RemoveEntity entity.Id |> boardCommand |> dispatch ]
                  [ icon "Trash" [] ]
              Some editButton
              Some hideButton ]
    let addGroupButton =
        button [ Class "stat-add"
                 OnClick <| fun _ -> AddStatGroup (Id.random (), entity.Id) |> boardCommand |> dispatch ]
               [ icon "FolderPlus" []
                 label [] [ str "Stat Group" ] ]
    let stats =
        Map.joinMap (viewStatGroup mode model dispatch) model.Board.StatGroups entity.StatGroups
        @ Option.toList (when' (mode = Edit) addGroupButton)
    let aspects = Map.joinMap (viewAspect mode model dispatch) model.Board.Aspects entity.Aspects
    let addAspectButton =
        button [ Class "aspect-add"
                 OnClick <| fun _ ->
                     AddAspect (Id.random (), entity.Id) |> boardCommand |> dispatch
                     StartEdit entity.Id |> Private |> dispatch ]
               [ icon "Plus" []
                 label [] [ str "Aspect" ] ]
    div [ Class "entity"
          Style <| dragStyle entity.Id model
          Key <| entity.Id.ToString ()
          Data ("draggable", entity.Id)
          Drag.draggableListener (Drag >> Private >> dispatch) ]
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
                  AddEntity entityId |> boardCommand |> dispatch
                  StartEdit entityId |> Private |> dispatch ]
            [ icon "Plus" [ Tabler.Size 64; Tabler.StrokeWidth 1.0 ]
              label [] [ str "Entity" ] ]
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
              button [ OnClick <| fun _ -> StopRoll |> Private |> dispatch ] [ icon "Checkbox" [] ] ]
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
