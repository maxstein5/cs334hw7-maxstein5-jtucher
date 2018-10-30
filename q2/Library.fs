module CS334

type Tree<'a> =
| Leaf of 'a
| Node of Tree<'a> * Tree<'a>

/// <summary> 
/// The recursive function treduce takes a binary operation f and a tree t and combines all the leaves of t using the binary operation f.
/// </summary>

/// <param name= "f"> Function used to combine leaf values</param>
/// <param name= "t"> Tree to be combines</param>

/// <return> Returns the result of the combined tree of type output of f.</return>

let rec treduce (f:('a -> 'a -> 'a)) (t: Tree<'a>) =
    match t with
    | Leaf(a) -> a
    | Node(t1,t2) -> f(treduce f t1)(treduce f t2) 
