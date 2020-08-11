module internal Destiny.Client.TabBar

open Destiny.Client.Tabler
open Destiny.Shared.Collections
open Fable.React
open Fable.React.Props

type 'a Message =
    | ChangeTab of 'a
    | RemoveTab of 'a
    | AddTab

let private viewTab dispatch format tab =
    span [ Class "tab" ]
         [ button [ OnClick <| fun _ -> ChangeTab tab |> dispatch ]
                  [ format tab |> str ]
           button [ OnClick <| fun _ -> RemoveTab tab |> dispatch ]
                  [ icon "X" [] ] ]

let private viewNewTab dispatch =
    button [ Class "tab-add"
             OnClick <| fun _ -> dispatch AddTab ]
           [ icon "Plus" []
             label [] [ str "Board" ] ]

let view dispatch tabs format =
    tabs
    |> List.map (viewTab dispatch format)
    |> List.add (viewNewTab dispatch) 
    |> div [ Class "tabs" ]
