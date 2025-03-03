(*
Name: William Bryce
Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type dTree =
| Leaf of int
| Node of char * dTree * dTree

let tLeft = Node('w',
                    Node('x', Leaf(2), Leaf(5)), 
                    Leaf(8))

let tRight = Node('w', 
                    Node('x', Leaf(2), Leaf(5)), 
                    Node('y', Leaf(7), Leaf(5)))

let rec height : dTree -> int =
    fun t ->
    match t with
    | Leaf(d) -> 1
    | Node(d, lt, rt) -> 1 + max (height lt) (height rt)

let rec size : dTree -> int =
    fun t ->
    match t with
    | Leaf(d) -> 1
    | Node(d, lt, rt) -> 1 + (size lt) + (size rt)

let rec paths : dTree -> int list list =
    fun t ->
    match t with
    | Leaf(d) -> [[]]
    | Node(d, lt, rt) -> List.map (fun x -> 0::x) (paths lt) @ List.map (fun x -> 1::x) (paths rt)

let rec is_perfect : dTree -> bool =
    fun t ->
    match t with
    | Leaf(d) -> true
    | Node(d, lt, rt) -> 
        if (height lt) = (height rt)
        then (is_perfect lt) && (is_perfect rt)
        else false

let rec map : (char -> char) -> (int -> int) -> dTree -> dTree =
    fun f g t ->
    match t with
    | Leaf(d) -> Leaf(g d)
    | Node(d, lt, rt) -> Node(f d, (map f g lt), (map f g rt))