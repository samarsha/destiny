module internal Destiny.Client.TabBar

open Destiny.Client.Tabler
open Destiny.Shared.Collections
open Fable.React
open Fable.React.Props

type 'a ViewModel =
    { Tabs : 'a list
      Active : 'a option
      Format : 'a -> string
      Kind : string }

type 'a Message =
    | AddTab
    | SwitchTab of 'a
    | SetTabName of 'a * string
    | RemoveTab of 'a

let private viewInactiveTab model dispatch tab =
    span [ Class "tab-inactive" ]
         [ button [ OnClick <| fun _ -> SwitchTab tab |> dispatch ]
                  [ model.Format tab |> str ]
           button [ OnClick <| fun _ -> RemoveTab tab |> dispatch ]
                  [ icon "X" [] ] ]

let private viewActiveTab model dispatch tab =
    span [ Class "tab-active" ]
         [ input [ OnChange <| fun event -> SetTabName (tab, event.Value) |> dispatch
                   Placeholder <| "Name this " + model.Kind.ToLower ()
                   Value <| upcast model.Format tab ]
           button [ OnClick <| fun _ -> RemoveTab tab |> dispatch ]
                  [ icon "X" [] ] ]

let private viewNewTab model dispatch =
    button [ Class "tab-add"
             OnClick <| fun _ -> dispatch AddTab ]
           [ icon "Plus" []
             label [] [ str model.Kind ] ]

let view model dispatch =
    model.Tabs
    |> List.map (fun tab ->
           (model, dispatch, tab)
           |||> if Option.contains tab model.Active then viewActiveTab else viewInactiveTab)
    |> List.add (viewNewTab model dispatch) 
    |> div [ Class "tabs" ]
