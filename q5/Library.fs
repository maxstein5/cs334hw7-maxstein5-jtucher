module CS334

open Parser

type Expr =
| Variable of char
| Abstraction of char * Expr
| Application of Expr * Expr

let expr, exprImpl = recparser()

let variable    : Parser<Expr> = pletter |>> (fun c -> Variable c) <!> "variable"

let abstraction : Parser<Expr> = pseq (pbetween (pstr "(L") (pchar '.') pletter) (pleft expr (pchar ')')) (fun (a,b) -> Abstraction(a,b)) <!> "abstraction"

let application : Parser<Expr> = pseq (pright (pchar '(') expr) (pleft expr (pchar ')')) (fun (a,b) -> Application(a,b)) <!> "application" 

exprImpl := variable <|> abstraction <|> application <!> "expr"

let grammar = pleft expr peof <!> "top"

let parse input : Expr option =
    match grammar (prepare input) with
    | Success(res, _) -> Some res
    | Failure -> None

let rec prettyprint(e: Expr) : string =
    match e with
    | Variable(c) -> "Variable(" + c.ToString() + ")"
    | Abstraction(a, b) -> "Abstraction(Variable(" + a.ToString() + "), " + (prettyprint b) + ")"
    | Application(a, b) -> "Application(" + (prettyprint a) + ", " + (prettyprint b) + ")"

