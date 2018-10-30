open System
open CS334

[<EntryPoint>]
let main argv =
    let rec treestring t = 
        match t with
        |Leaf a -> "Leaf " + a.ToString()
        |Node (x, y) -> "Node (" + treestring x + ", " + treestring y + ")" 

    let f x y = x + y
    let t = Node(Node(Leaf 2, Leaf 3), Leaf 4)
    printf "%s\n" (treestring t)
    printf "%A\n" (treduce f t)

    let g x y = x + "Max" + y
    let r = Node(Node(Leaf "Julia", Leaf "Julia"), Node(Leaf "Dan", Leaf "Dan"))
    printf "%s\n" (treestring r)
    printf "%A\n" (treduce g r)

    let h x y = (x = y)
    let v = Node(Node(Leaf true, Leaf false), Node(Node(Leaf true, Leaf true), Leaf false))
    printf "%s\n" (treestring v)
    printf "%A\n" (treduce h v)
    0

