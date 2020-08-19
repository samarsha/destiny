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
                    return! Some value |> receive
                }
        receive initial

    let createEmpty () = MailboxProcessor.Start (body None) |> MVar

    let create initial = Some initial |> body |> MailboxProcessor.Start |> MVar

    let updateResult (MVar mailbox) mapper =
        let value, result = mailbox.PostAndReply Take |> mapper
        Put value |> mailbox.Post
        result

    let update var mapper =
        updateResult var (fun value -> let value' = mapper value in value', value')

    let read var = update var id
