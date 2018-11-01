open System
open CS334

[<EntryPoint>]
let main argv =
    let printparse x = 
        match x with
        | Some x -> printfn "%A" (prettyprint x)
        | None -> printfn "Invalid program."
    let asto = parse "((Lx.x)(Lx.y))"
    printparse asto
    let y = parse "(((Lx.((xx)(ax)))(Lx.(bx)))a)"
    printparse y
    let z = parse "(Lx.xx)(ay)"
    printparse z
    0

