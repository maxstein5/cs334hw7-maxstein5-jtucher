open System
open CS334

[<EntryPoint>]
let main argv =
    let rec treestring t = 
        match t with
        |Leaf a -> "Leaf " + a.ToString()
        |Node (x, y) -> "Node (" + treestring x + ", " + treestring y + ")" 

    let f x = x + 1
    let t = Node(Node(Leaf 2, Leaf 3), Leaf 4)
    printf "%s\n" (treestring t)
    printf "%s\n" (treestring (maptree f t))

    let g x = "Max"
    let r = Node(Node(Leaf "Julia", Leaf "Julia"), Node(Leaf "Dan", Leaf "Dan"))
    printf "%s\n" (treestring r)
    printf "%s\n" (treestring (maptree g r))

    let h x = String.length x
    let v = Node(Node(Leaf "Map", Leaf "strings"), Node(Node(Leaf "to", Leaf "their"), Leaf "length"))
    printf "%s\n" (treestring v)
    printf "%s\n" (treestring (maptree h v))
    0

