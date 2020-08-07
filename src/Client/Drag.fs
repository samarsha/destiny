module internal Destiny.Client.Drag

open Browser.Types
open Fable.React
open Fable.React.Props

type private Position =
    { X : float
      Y : float }

type private Rectangle =
    { TopLeft : Position
      Width : float
      Height : float }

type private Draggable =
    { Id : string
      Region : Rectangle }

type private Tentative =
    { Id : string
      DraggablePosition : Position
      PointerPosition : Position }

type private Active =
    { Source : string
      Targets : string list
      Offset : Position
      PointerPosition : Position }

type Model =
    private
    | Inactive
    | Tentative of Tentative
    | Active of Active

type Message =
    private
    | Start of Tentative
    | Move of Position * string list
    | Stop

// Model

let empty = Inactive

let current = function
    | Active { Source = source } -> Some source
    | _ -> None

let targets = function
    | Active { Targets = targets } -> targets
    | _ -> []

let private canDrag (name : string) =
    not <| List.contains (name.ToLower ()) [ "button"; "input"; "textarea" ]

let private threshold = 10.0

let private draggableData = "draggable"

let private draggableSelector = "[data-" + draggableData + "]"

let private (--) p1 p2 = { X = p1.X - p2.X; Y = p1.Y - p2.Y }

let private distance p1 p2 =
    let delta = p2 -- p1
    delta.X * delta.X + delta.Y * delta.Y |> sqrt

let private contains position rect =
    rect.TopLeft.X <= position.X && position.X <= rect.TopLeft.X + rect.Width &&
    rect.TopLeft.Y <= position.Y && position.Y <= rect.TopLeft.Y + rect.Height

// View

let private boundingRect (element : Element) =
    let rect = element.getBoundingClientRect ()
    { TopLeft = { X = rect.left; Y = rect.top }
      Width = rect.width
      Height = rect.height }

let draggableListener dispatch =
    OnMouseDown <| fun event ->
        let target = event.target :?> HTMLElement
        match target.closest draggableSelector with
        | Some draggable when canDrag target.tagName ->
            { Id = (draggable :?> HTMLElement).dataset.[draggableData]
              DraggablePosition = (boundingRect draggable).TopLeft
              PointerPosition = { X = event.clientX; Y = event.clientY } }
            |> Start
            |> dispatch
            event.preventDefault ()
        | _ -> ()
    :> IHTMLProp

let private draggables (element : Element) =
    let draggables = element.querySelectorAll draggableSelector
    [ 0 .. draggables.length - 1 ] |> List.map (fun i ->
        let draggable = draggables.[i] :?> HTMLElement
        { Id = draggable.dataset.[draggableData]
          Region = boundingRect draggable })

let areaListeners model dispatch : IHTMLProp list =
    match model with
    | Inactive -> []
    | _ ->
        [ OnMouseMove <| fun event ->
              let position = { X = event.clientX; Y = event.clientY }
              let targets =
                  draggables (event.currentTarget :?> Element)
                  |> List.filter (fun draggable -> contains position draggable.Region)
                  |> List.map (fun draggable -> draggable.Id)
              Move (position, targets) |> dispatch
          OnMouseUp <| fun _ -> dispatch Stop ]

let view viewById = function
    | Active { Source = source; Offset = offset; PointerPosition = position } ->
        let dragPosition = position -- offset
        div [ Class "dragging"
              Style [ CSSProp.Position PositionOptions.Absolute
                      Left (dragPosition.X.ToString () + "px")
                      Top (dragPosition.Y.ToString () + "px") ] ]
            [ viewById source ]
    | _ -> div [ Class "inactive-drag" ] []

// Update

let update message model =
    match message, model with
    | Start tentative, _ -> Tentative tentative
    | Move _, Inactive -> Inactive
    | Move (position, targets), Tentative tentative ->
        if distance position tentative.PointerPosition < threshold
        then model
        else Active { Source = tentative.Id
                      Targets = List.filter ((<>) tentative.Id) targets
                      Offset = position -- tentative.DraggablePosition
                      PointerPosition = position }
    | Move (position, targets), Active active ->
        Active { active with Targets = List.filter ((<>) active.Source) targets
                             PointerPosition = position }
    | Stop, _ -> Inactive
