(*Harishan Amirthanathan
I pledge my honor that I have abided by the Stevens Honor System
*)

type dTree =
  | Leaf of int
  | Node of char * dTree * dTree

let tLeft =
  Node ('w',
    Node ('x', Leaf 2, Leaf 5),
    Leaf 8
  )

let tRight =
  Node ('w',
    Node ('x', Leaf 2, Leaf 5),
    Node ('y', Leaf 7, Leaf 5)
  )

let rec height (t: dTree) : int =
  match t with
  | Leaf _-> 1
  | Node(_,lt,rt) -> 1 + max(height lt) (height rt)

let rec size (t: dTree) : int =
  match t with
  | Leaf _-> 1
  | Node(_,lt,rt) -> 1 + size lt + size rt


let rec paths (t: dTree) : int list list =
  match t with
  | Leaf _-> [[]]
  | Node(_,lt,rt) -> 
    (List.map(fun p -> 0 :: p)(paths lt)) @ 
    (List.map(fun p -> 1 :: p)(paths rt))

let rec is_perfect (t: dTree) : bool =
  let rec depths t d =
    match t with
    | Leaf _ -> [d]
    | Node (_, lt, rt) -> (depths lt (d+1)) @ (depths rt (d+1))
  in
  let d_list = depths t 0 in
  List.for_all ((=) (List.hd d_list)) d_list

let rec map (n: char -> char) (e: int -> int) (t: dTree) : dTree = 
  match t with
  | Leaf x -> Leaf (e x)
  | Node(d,lt,rt) -> Node(n d,map n e lt,map n e rt)
  
let rec list_to_tree (lst: char list) : dTree =
  match lst with
  | [] -> Leaf 0
  | h :: t -> Node (h, list_to_tree t, list_to_tree t)

let rec replace_leaf_at (tree: dTree) (mapping: (int list * int) list) : dTree =
  let rec x (path: int list) (mapping: (int list * int) list) : int =
    match mapping with
    | [] -> 0  
    | (n, value) :: remaining ->
        if n = path then value
        else x path remaining
  in

  let rec traverse (t: dTree) (y: int list) : dTree =
    match t with
    | Leaf _ -> Leaf (x y mapping)  
    | Node (char, lt, rt) ->
        Node (char, traverse lt (y @ [0]), traverse rt (y @ [1]))
  in

  
  traverse tree []

let bf_to_dTree ((vars, graph): char list * (int list * int) list) : dTree =
  replace_leaf_at (list_to_tree vars) graph