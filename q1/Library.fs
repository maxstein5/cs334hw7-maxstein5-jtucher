module CS334

type Tree<'a> =
| Leaf of 'a
| Node of Tree<'a> * Tree<'a>

/// <summary> 
///Maptree applies a function f to all leaves of a tree by either calling f on leaves or calling maptree recursively on the nodes of a tree. Type: ('a -> 'b) -> Tree<'a> -> Tree<'b>. The f function dictates the type of tree that is input and output. Since f can go from type 'a to type 'b, the maptree function can convert a tree from type 'a to 'b, not only from 'a to 'a. 
/// </summary>

/// <param name= "f"> Function to be applied to a tree's leaves</param>
/// <param name= "t"> Tree to have function applied to its leaves</param>

/// <return> Returns a new tree resulting from f applied to all leaves of t</return>

let maptree f t =
    let rec maptree' f t = 
        match t with
        | Leaf (a) -> Leaf(f a)
        | Node (t1,t2) -> Node(maptree' f t1, maptree' f t2)
    maptree' f t
