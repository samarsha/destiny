module internal Destiny.Client.TabBar

open Destiny.Client.Tabler
open Destiny.Shared.Collections
open Destiny.Shared.Functions
open Fable.React
open Fable.React.Props

type 'a ViewModel =
    { Tabs : 'a list
      Active : 'a option
      Format : 'a -> string
      Kind : string
      CanEdit : bool }

type 'a Message =
    | AddTab
    | SwitchTab of 'a
    | SetTabName of 'a * string
    | RemoveTab of 'a

let private removeButton dispatch tab =
    button [ Title "Remove this board"
             OnClick <| fun _ -> RemoveTab tab |> dispatch ]
           [ icon "Trash" [] ]

let private viewInactiveTab model dispatch tab =
    List.choose id
        [ span [ Class "tab-name" ] [ model.Format tab |> str ] |> Some
          removeButton dispatch tab |> Option.iff (model.CanEdit) ]
    |> span [ Class "tab tab-inactive"
              OnClick <| fun _ -> SwitchTab tab |> dispatch ]

let private viewActiveTab model dispatch tab =
    List.choose id
        [ if model.CanEdit
          then input [ OnChange <| fun event -> SetTabName (tab, event.Value) |> dispatch
                       Placeholder <| "Name this " + model.Kind.ToLower ()
                       Value <| upcast model.Format tab ]
          else span [ Class "tab-name" ] [ model.Format tab |> str ]
          |> Some
          removeButton dispatch tab |> Option.iff (model.CanEdit) ]
    |> span [ Class "tab tab-active" ]

let private viewNewTab model dispatch =
    button [ Class "tab tab-add"
             OnClick <| fun _ -> dispatch AddTab ]
           [ icon "Plus" []
             label [] [ str model.Kind ] ]

let view model dispatch =
    model.Tabs
    |> List.map (fun tab ->
           (model, dispatch, tab)
           |||> if Option.contains tab model.Active then viewActiveTab else viewInactiveTab)
    |> flip List.append (Option.iff model.CanEdit (viewNewTab model dispatch) |> Option.toList)
    |> List.add (span [ Class "tab-excess" ] [])
    |> div [ Class "tabs" ]
