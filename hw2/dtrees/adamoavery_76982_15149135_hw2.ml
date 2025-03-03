(* Avery Adamo *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)

type dTree =
| Leaf of int
| Node of char*dTree*dTree

let tLeft = Node('w', 
                  Node('x', 
                        Leaf(2), Leaf(5)), 
                  Leaf(8))

let tRight = Node('w', 
                   Node('x', 
                        Leaf(2), Leaf(5)), 
                   Node('y', 
                        Leaf(7), Leaf(5)))

let rec height : dTree -> int =
  fun t ->
  match t with
  | Leaf(n) -> 1
  | Node(c, l, r) -> 1 + max (height l) (height r)

let rec size : dTree -> int =
  fun t ->
  match t with 
  | Leaf(n) -> 1
  | Node(c, l, r) -> 1 + size l + size r

(* 0 -> left, 1 -> right *)
let rec paths : dTree -> int list list =
  fun t ->
  match t with
  | Leaf(n) -> [[]]
  | Node(c, l, r) -> (List.map (fun l -> 0 :: l) (paths l)) @ (List.map (fun l -> 1 :: l) (paths r))

let is_perfect : dTree -> bool = 
  fun t ->
  let path_lengths = List.map (fun l -> List.length l) (paths t) in
  match path_lengths with
  | [] -> true
  | h::t -> List.for_all (fun n -> n=h) t
  
let rec map : (char -> char) -> (int -> int) -> dTree -> dTree = 
  fun f g t ->
  match t with
  | Leaf(n) -> Leaf(g n)
  | Node(c, l, r) -> Node(f c, map f g l, map f g r)

let rec list_to_tree : char list -> dTree =
  fun l ->
  match l with
  | [] -> failwith "empty list"
  | [c] -> Node(c, Leaf(0), Leaf(0))
  | h::t -> Node(h, list_to_tree t, list_to_tree t)

let rec replace_leaf_at : dTree -> (int list * int) list -> dTree =
  fun tr l ->
  (* Finds and replaces one leaf using one specific path *)
  let rec find_replace_leaf : dTree -> int list -> int -> dTree =
    fun tr dl nm ->
    match tr with
    | Leaf(n) -> Leaf(nm)
    | Node(c, l, r) -> 
      match dl with
      | [] -> tr
      | h::t ->
        if h=0 then Node(c, find_replace_leaf l t nm, r) else Node(c, l, find_replace_leaf r t nm)
  in
  match l with
  | [] -> tr
  | (path, num)::t -> replace_leaf_at (find_replace_leaf tr path num) t
 
let rec bf_to_dtree : char list * (int list * int) list -> dTree = 
  fun l ->
  let tree = list_to_tree (fst l) in
  replace_leaf_at tree (snd l)
