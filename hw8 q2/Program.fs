open System

open LambdaParser
open CS334

[<EntryPoint>]
let main argv =
    let printparse x = 
        match x with
        | Some x -> printfn "%A" (lambdaprint x)
        | None -> printfn "Invalid program."
    let asto = parse "((Lx.x)(Lx.y))"
    printparse asto
    let y = parse "(((Lx.((xx)(ax)))(Lx.(bx)))a)"
    printparse y
    let z = parse "(Lx.xx)(ay)"
    printparse z
    let alph x = 
        match x with
        | Some x -> (alphanorm x Set.empty Map.empty)
        | None -> exit(1)
    match alph y with
    | (e, b) -> printfn "%A" (lambdaprint e)
    0
