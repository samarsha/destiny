namespace Destiny.Shared.Profile

type Username = Username of string

module Username =
    let toString (Username username) = username

type Password = Password of string

module Password =
    let toString (Password password) = password

type Role =
    | Player
    | DM

type Profile =
    { Username : Username
      Role : Role }
