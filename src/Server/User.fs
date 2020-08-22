namespace Destiny.Server

open Destiny.Shared
open Microsoft.AspNetCore.Cryptography.KeyDerivation
open System.Security.Cryptography

type internal User =
    { Profile : Profile
      Salt : byte list
      Hash : byte list }

module internal User =
    let private hash (Password password) salt =
        KeyDerivation.Pbkdf2 (password, List.toArray salt, KeyDerivationPrf.HMACSHA512, 10000, 32)
        |> Array.toList

    let private randomBytes (random : RandomNumberGenerator) length =
        let bytes = Array.zeroCreate length
        random.GetBytes bytes
        Array.toList bytes

    let private sessionId random = randomBytes random 16 |> Id.ofBytes

    let signUp random profile password =
        let salt = randomBytes random 16
        let user =
            { Profile = profile
              Salt = salt
              Hash = hash password salt }
        user, { Id = sessionId random; Profile = profile }

    let logIn random user password =
        if hash password user.Salt = user.Hash
        then Some { Id = sessionId random; Profile = user.Profile }
        else None

    let restore sessionId user = { Id = sessionId; Profile = user.Profile }
