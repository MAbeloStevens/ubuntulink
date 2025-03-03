(* 
Justin Carnemolla
I pledge my honor that I have abided by the Stevens Honor system 
*)

type dTree =
| Leaf of int
| Node of char * dTree * dTree

let tLeft = Node('w',
              Node('x',
                Leaf(2),
                Leaf(5)
              ),
              Leaf(8)
            )           
let tRight = Node('w',
              Node('x',
                Leaf(2),
                Leaf(5)
              ),
              Node('y',
                Leaf(7),
                Leaf(5)
              )
            )  

let rec height (t : dTree) : int =
  match t with
  | Leaf(_) -> 1
  | Node(_, l, r) -> 1 + (max (height l) (height r))

let rec size (t : dTree) : int =
  match t with
  | Leaf(_) -> 1
  | Node(_, l, r) -> 1 + size l + size r

let rec paths (t : dTree) : int list list = 
  match t with
  | Leaf(_) -> [[]]
  | Node(_, l, r) ->
      let left_paths = List.map (fun p -> 0::p) (paths l) in
      let right_paths = List.map (fun p -> 1::p) (paths r) in
      List.append left_paths right_paths

let rec is_perfect (t : dTree) : bool = 
  match t with
  | Leaf(_) -> true
  | Node(_,l,r) -> (is_perfect l && is_perfect r) && (height l = height r)

let rec map (f : char -> char) (g: int -> int) (t : dTree) : dTree =
  match t with
  | Leaf(x) -> Leaf(g x)
  | Node(x,l,r) -> Node(f x, map f g l, map f g r)

let rec list_to_tree (l : char list) : dTree = 
  match l with 
  | [] -> Leaf(0)
  | h::t -> Node(h,list_to_tree t, list_to_tree t)

let rec replace_leaf_at (t : dTree) (f : (int list * int) list) : dTree =
  let rec lookup (path : int list) (f : (int list * int) list) : int =
    match f with
    | [] -> 0
    | (p, v)::t -> if p = path then v else lookup path t
  in
  let rec replace (t : dTree) (path : int list) : dTree =
    match t with
    | Leaf(_) -> Leaf (lookup path f)
    | Node (c, l, r) -> 
        Node (c, replace l (List.append path [0]), replace r (List.append path [1]))
  in
  replace t []

let rec bf_to_dTree (l : char list * (int list * int) list) : dTree = 
  let (chars, graph) = l in
  replace_leaf_at (list_to_tree chars) graph
