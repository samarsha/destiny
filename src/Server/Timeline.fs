namespace Destiny.Server

open Destiny.Shared.Collections

type internal 'a Timeline =
    private
        { Past : 'a list
          Present : 'a
          Future : 'a list
          Committed : bool }

module internal Timeline =
    let private maxLength = 50

    let private length timeline = List.length timeline.Past + 1 + List.length timeline.Future

    let singleton value =
        { Past = []
          Present = value
          Future = []
          Committed = true }

    let present timeline = timeline.Present

    let undo = function
        | { Past = present' :: past' } as timeline ->
            { Past = past'
              Present = present'
              Future = timeline.Present :: timeline.Future
              Committed = true }
        | timeline -> timeline

    let redo = function
        | { Future = present' :: future' } as timeline ->
            { Past = timeline.Present :: timeline.Past
              Present = present'
              Future = future'
              Committed = true }
        | timeline -> timeline

    let commit timeline = { timeline with Committed = true }

    let set value timeline =
        let timeline' =
            { timeline with
                  Present = value
                  Future = []
                  Committed = false }
        if timeline.Committed && length timeline = maxLength
        then { timeline' with Past = timeline.Present :: List.initial timeline.Past }
        elif timeline.Committed
        then { timeline' with Past = timeline.Present :: timeline.Past }
        else timeline'

    let update f timeline = set (f timeline.Present) timeline
