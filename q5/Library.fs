module CS334

open Parser

type Expr =
| Variable of char
| Abstraction of char * Expr
| Application of Expr * Expr

let expr, exprImpl = recparser()

let variable    : Parser<Expr> = pitem |>> (fun c -> Variable c) <|> "variable" (* variable parser implementation *)

let abstraction : Parser<Expr> = pseq variable exprImpl |>> (fun (a,b) -> Abstraction(a,b)) <|> "abstraction" (* abstraction parser implementation *)

let application : Parser<Expr> = pseq exprImpl exprImpl |>> (fun (a,b) -> Application(a,b)) <|> "application" (* application parser implementation *)

let expr = variable <|> abstraction <|> application <|> "expr"

exprImpl:= pmany1 expr (* expr parser implementation *)
