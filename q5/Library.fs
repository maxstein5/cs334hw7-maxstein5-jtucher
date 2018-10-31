module CS334

open Parser

type Expr =
| Variable of char
| Abstraction of char * Expr
| Application of Expr * Expr

let expr, exprImpl = recparser()

let variable    : Parser<Expr> = pitem |>> (fun c -> Variable c) <!> "variable" (* variable parser implementation *)

let abstraction : Parser<Expr> = pbetween (pbetween (pseq (pchar '(') (pchar 'L')) (pchar '.') pitem) (pchar ')') expr (fun (a,b) -> Abstraction(a,b)) <!> "abstraction"

let application : Parser<Expr> = pseq expr expr (fun (a,b) -> Application(a,b)) <!> "application" (* application parser implementation *)

exprImpl := variable <|> abstraction <|> application <!> "expr" (* expr parser implementation *)

let grammar = pleft expr peof <!> "top"

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(res, _) -> Some res
    | Failure -> None

let rec repl() : unit =
    printf "Enter an expression: "
    let input = System.Console.ReadLine()
    if input = "quit" then
        printfn "Goodbye!"
        exit 0
    else
        let asto = parse input
        match asto with
        | Some ast -> printfn "%A" ast
        | None     -> printfn "Invalid expression."
        repl()