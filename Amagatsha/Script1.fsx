
open System

let ts = "20170710"
let protected = "Protected"
let empty = ""
let xx = "slkdfklsdjfskjdf"

[
    ts, protected, xx
    ts, empty, xx
]
|> List.iter (fun x -> x |||> printfn "%s   %-12s %s")

