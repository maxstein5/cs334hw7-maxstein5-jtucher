open System
open CS334

[<EntryPoint>]
let main argv =
    let asto = parse "((Lx.x)(Lx.y))"
    match asto with
    | Some ast -> printfn "%A" (prettyprint ast)
    | None -> printfn "Invalid program."
    0

