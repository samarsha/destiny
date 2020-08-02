namespace Destiny.Server

open System

type private 'a Message =
    | Put of 'a
    | Take of 'a AsyncReplyChannel

type internal 'a MVar =
    private
    | MVar of 'a Message MailboxProcessor

    interface IDisposable with
        member this.Dispose () =
            match this with
            | MVar mailbox -> (mailbox :> IDisposable).Dispose ()

module internal MVar =
    let private toPut = function
        | Put value -> Some value
        | Take _ -> None

    let private toTake = function
        | Put _ -> None
        | Take channel -> Some channel

    let private body initial (inbox : _ MailboxProcessor) =
        let rec receive = function
            | Some value ->
                async {
                    let! channel = inbox.Scan (toTake >> Option.map async.Return)
                    channel.Reply value
                    return! receive None
                }
            | None ->
                async {
                    let! value = inbox.Scan (toPut >> Option.map async.Return)
                    return! receive <| Some value
                }
        receive initial

    let createEmpty () = MVar <| MailboxProcessor.Start (body None)

    let create initial = MVar <| MailboxProcessor.Start (body <| Some initial)

    let update (MVar mailbox) mapper =
        let value = mailbox.PostAndReply Take
        let value' = mapper value
        mailbox.Post <| Put value'
        value'

    let read var = update var id
