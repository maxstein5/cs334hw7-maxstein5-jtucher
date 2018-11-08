module CS334

open LambdaParser

/// <summary>Prints an AST in the exact form that the user typed it in.</summary>
/// <param name="e">An Expr.</param>
/// <returns>A string.</returns>
let rec lambdaprint(e: Expr) : string =
    match e with
    | Variable(c) -> c.ToString()
    | Abstraction(a, b) -> "(L" + a.ToString() + "." + (lambdaprint b) + ")"
    | Application(a, b) -> "(" + (lambdaprint a) + (lambdaprint b) + ")"

/// <summary>Finds and returns a set of all free variables in a lambda expression.</summary>
/// <param name="e">An Expr.</param>
/// <returns>A set of chars containing all free variables.</returns>
let fv (e: Expr) : Set<char> =
    let rec helper (e: Expr)(bv: Set<char>) : Set<char> =
        match e with
        | Variable(c) -> if(not (Set.contains c bv)) then Set.add c (Set.empty : Set<char>) else (Set.empty : Set<char>)
        | Abstraction(a, b) -> helper b (Set.add a bv)
        | Application(a, b) -> Set.union (helper a bv) (helper b bv)
    helper e Set.empty
    
/// <summary>Gives the next available letter of the alphabet given a set of already used letters.</summary>
/// <param name="b">A set of already used chars.</param>
/// <returns>A fresh char.</returns>
let freshvar (b: Set<char>) : char =
    let letters = Set.ofList ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
    let fv = Set.toList (Set.difference letters b)
    match fv with
    | a::fv' -> a
    | [] ->
        printfn "No more letters in alphabet" 
        exit(1)
        
/// <summary>Alpha normalized a lambda expression so that every different bound variable is a unique letter.</summary>
/// <param name="e">An Expr.</param>
/// <param name="b">A set of already used chars.</param>
/// <param name="r">A map from char to char full of renaming rules.</param>
/// <returns>A char.</returns>
let rec alphanorm (e: Expr)(b: Set<char>)(r: Map<char, char>) : Expr*Set<char> = 
    match e with
    | Variable(c) -> 
        if(Map.containsKey c r) then 
            (Variable(Map.find c r), b) 
        else (Variable(c), b)
    | Abstraction(v, e) -> 
        if(Set.contains v b) then
            let v' = freshvar b
            let b' = Set.add v' b
            let r' = Map.add v v' r
            match (alphanorm e b' r') with
            |(e, b) -> (Abstraction(v', e), Set.union b b')
        else
            let b' = Set.add v b
            match alphanorm e b' r with
            |(e, b) -> (Abstraction(v, e), Set.union b b')
    | Application(e1, e2) -> 
        let t1 = (alphanorm e1 b r)
        let t2 =
            match t1 with
            |(e, b') -> (alphanorm e2 b' r)
        match (t1, t2) with
        |((e1, b1), (e2, b2)) -> (Application(e1, e2), Set.union b1 b2)
    
