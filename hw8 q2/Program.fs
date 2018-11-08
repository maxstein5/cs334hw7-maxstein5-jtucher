open System

open LambdaParser
open CS334

[<EntryPoint>]
let main argv =
    let printparse x = 
        match x with
        | Some x -> printfn "%A" (lambdaprint x)
        | None -> printfn "Invalid program."
    let asto = parse "((Lx.(Lx.x))(Lx.(xx)))"
    printparse asto
    let y = parse "(((Lx.((xx)(ax)))(Lx.(bx)))a)"
    printparse y
    let z = parse "((Lx.x)(Lx.x))"
    printparse z
    let alph y = 
        let u = 
            match y with
            | Some x -> x
            | None -> 
                printfn "Invalid expression cannot be alpha normalized"
                exit(0)
        match (alphanorm u (fv u) Map.empty) with
        | (e, b) -> printfn "%A" (lambdaprint e)
    alph asto
    alph y
    alph z
    0
