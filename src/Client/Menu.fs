module internal Destiny.Client.Menu

open Destiny.Client.Tabler
open Destiny.Shared
open Fable.React
open Fable.React.Props

type 'a Item =
    { Name : string
      Icon : ReactElement
      Message : 'a }

type Message<'id, 'item> =
    | Open of 'id
    | Close
    | Select of 'item

let private viewItem dispatch item =
    [ button
          [ OnClick <| fun _ -> Select item.Message |> dispatch ]
          [ item.Icon; str item.Name ] ]
    |> li [ Class "menu-item" ]

let view dispatch menuId openId items =
    let isOpen = openId |> Option.contains menuId
    let menuButton =
        button
            [ Class "menu-button"
              OnClick <| fun _ -> dispatch <| if isOpen then Close else Open menuId ]
            [ icon "DotsVertical" [] ]
    let menuItems = items |> List.map (viewItem dispatch) |> ul [ Class "menu-items" ]

    [ Some menuButton
      Option.iff isOpen menuItems ]
    |> List.choose id
    |> div [ Class "menu" ]
