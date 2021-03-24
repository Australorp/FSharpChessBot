namespace global

[<RequireQualifiedAccess>]
module Async =
    let runParallelSize (size: int) tasks =
        Async.Parallel(tasks, size)


[<RequireQualifiedAccess>]
module Parallel =
    let run threadCount actions =
        actions
        |> Seq.map (fun action -> async { return action() })
        |> Async.runParallelSize threadCount
        |> Async.RunSynchronously

[<RequireQualifiedAccess>]
module Seq =
    let mapParallel threadCount action seq =
        seq
        |> Seq.map (fun item -> async { return action item })
        |> Async.runParallelSize threadCount
        |> Async.RunSynchronously