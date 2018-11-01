module CS334

open Parser

///<summary> The data type Expr is used to data for a valid expression in the Labda calculus grammar.</summary>///
type Expr =
| Variable of char
| Abstraction of char * Expr
| Application of Expr * Expr


///<summary> The following parsers (expr, exprImpl, variable, abstraction, application, grammar) 
///define parsers that correspond to the elements on the Lambda calculus grammar. </summary>///

let expr, exprImpl = recparser()

let variable    : Parser<Expr> = pletter |>> (fun c -> Variable c) <!> "variable"

let abstraction : Parser<Expr> = pseq (pbetween (pstr "(L") (pchar '.') pletter) (pleft expr (pchar ')')) (fun (a,b) -> Abstraction(a,b)) <!> "abstraction"

let application : Parser<Expr> = pseq (pright (pchar '(') expr) (pleft expr (pchar ')')) (fun (a,b) -> Application(a,b)) <!> "application" 

exprImpl := variable <|> abstraction <|> application <!> "expr"

let grammar = pleft expr peof <!> "top"


///<summary> Parse is a function that takes in an input string 'input', prepares it, and returns an option of type Expr. </summary>///
let parse input : Expr option =
    match grammar (prepare input) with
    | Success(res, _) -> Some res
    | Failure -> None


///<summary> Prettyprint takes in an Expr 'e' and returns a string that represents given Lambda calculus expression. </summary>///
let rec prettyprint(e: Expr) : string =
    match e with
    | Variable(c) -> "Variable(" + c.ToString() + ")"
    | Abstraction(a, b) -> "Abstraction(Variable(" + a.ToString() + "), " + (prettyprint b) + ")"
    | Application(a, b) -> "Application(" + (prettyprint a) + ", " + (prettyprint b) + ")"

