namespace Destiny.Server.MVar

type private 'a Message =
    | Put of 'a
    | Take of 'a AsyncReplyChannel

type internal 'a MVar = private MVar of 'a Message MailboxProcessor

module internal MVar =
    let private toPut = function
        | Put value -> Some value
        | Take _ -> None

    let private toTake = function
        | Put _ -> None
        | Take channel -> Some channel

    let private body initial (inbox : _ MailboxProcessor) =
        let rec update = function
            | Some value -> async {
                let! channel = inbox.Scan (toTake >> Option.map async.Return)
                channel.Reply value
                return! update None }
            | None -> async {
                let! value = inbox.Scan (toPut >> Option.map async.Return)
                return! update <| Some value }
        update initial

    let createEmpty () = MVar <| MailboxProcessor.Start (body None)

    let create initial = MVar <| MailboxProcessor.Start (body <| Some initial)

    let updateReturn (MVar mailbox) f =
        let value = mailbox.PostAndReply Take
        let value', ret = f value
        mailbox.Post <| Put value'
        ret

    let update var f = updateReturn var <| fun value -> f value, ()

    let read var = updateReturn var <| fun value -> value, value
