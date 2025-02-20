(*
Quiz 2
Name1: James Barbi
Name2:
*)


type 'a bt = Empty | Node of 'a*'a bt*'a bt

(* Helper function that builds a leaf *)
let leaf n = Node(n,Empty,Empty)

(* Two sample binary trees.
The first one is a BST, but not the second one *)
let t1 : int bt = Node(12,
Node(7,Empty,leaf 10),
Node(24, leaf 14, Empty))

let t2 : int bt = Node(12,
leaf 7, Node(24, leaf 30, Empty))

(** returns smallest element in non-empty tree [t].
Fails if [t] is empty, it should fail.
Note: the tree [t] is not assumed to be a bst *)
let rec mint : 'a bt -> 'a =
fun t ->
  match t with
  | Empty -> failwith "tree is empty"
  | Node(v, Empty, Empty) -> v
  | Node(v, Empty, right) -> min v (mint right)
  | Node(v, left, Empty) -> min v (mint left)
  | Node(v, left, right) -> min v (min (mint left) (mint right))

(** returns largest element in non-empty tree [t].
Fails if [t] is empty, it should fail.
Note: the tree [t] is not assumed to be a bst *)
let rec maxt : 'a bt -> 'a =
fun t ->
  match t with
  | Empty -> failwith "tree is empty"
  | Node(v, Empty, Empty) -> v
  | Node(v, Empty, right) -> max v (maxt right)
  | Node(v, left, Empty) -> max v (maxt left)
  | Node(v, left, right) -> max v (max (maxt left) (maxt right))

(** [is_bst t] determines whether the binary tree [t] is a binary search tree *)
let rec is_bst : 'a bt -> bool =
fun t ->
  match t with
  | Empty -> true
  | Node(v, Empty, Empty) -> true
  | Node(v, Empty, right) -> (match right with
                              | Empty -> true
                              | Node(right_val, _, _) -> v < right_val && is_bst right)
  | Node(v, left, Empty) -> (match left with
                              | Empty -> true
                              | Node(left_val, _, _) -> v > left_val && is_bst left)
  | Node(v, left, right) -> (match right with
                              | Empty -> true
                              | Node(right_val, _, _) -> v < right_val && is_bst right)
                            &&
                            (match left with
                              | Empty -> true
                              | Node(left_val, _, _) -> v > left_val && is_bst left)

(** [add k t] adds key [k] to the bst [t].
Should fail with failwith if [k] is already in the tree.
Otherwise, returns updated tree *)
let rec add : 'a -> 'a bt -> 'a bt =
fun k t ->
  match t with
  | Empty -> leaf k
  | Node(v, left, right) -> 
    if k = v 
      then failwith "given value is in tree" 
    else if k<v 
      then Node(v, add k left, right)
    else 
      Node(v, left, add k right)

(** [rem k t] removes key [v] from the BST [t]
Should fail with failwith if [k] is not in tree
(Extra-credit) *)
let rec rem : 'a -> 'a bt -> 'a bt =
fun k t ->
failwith "implement"