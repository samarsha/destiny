namespace Destiny.Shared

type Username = Username of string

module Username =
    let toString (Username username) = username

type Password = Password of string

module Password =
    let toString (Password password) = password

type Team =
    | Player
    | DM

module Team =
    let ofString = function
        | "Player" -> Some Player
        | "DM" -> Some DM
        | _ -> None

type Profile =
    { Username : Username
      Team : Team }

type Session =
    { Id : Session Id
      Profile : Profile }
