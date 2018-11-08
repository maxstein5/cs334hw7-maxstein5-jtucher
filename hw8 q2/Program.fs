open System

open LambdaParser
open CS334

[<EntryPoint>]
let main argv =
    let argparse argv =
        if Array.length argv <> 1 then
            printfn "Need a lambda expression"
            exit(1)

        let s: String =
            try
                argv.[0]
            with
            | :? System.FormatException as e ->
                printfn "Not a string"
                exit(1)
        s

    let printparse x = 
        match x with
        | Some x -> printfn "%A" (lambdaprint x)
        | None -> printfn "Invalid program."

    let asto = parse (argparse argv)
    printparse asto
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
    0
