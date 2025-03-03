(* Payton Bates - I pledge my honor that I have abided by the Stevens Honor System *)

type dTree =
| Leaf of int
| Node of char*dTree*dTree;;

let tLeft : dTree = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8));;
let tRight : dTree = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)));;

(* Returns the height of a tree by incrementing the max of its subtrees *)
let rec height (t : dTree) : int =
  match t with
  | Leaf(_) -> 1
  | Node(_, lt, rt) -> 1 + max (height lt) (height rt);;

(* Returns the size of a tree by incrementing the size of its subtrees *)
let rec size (t : dTree) : int =
  match t with
  | Leaf(_) -> 1
  | Node(_, lt, rt) -> 1 + size lt + size rt;;

(* Returns all paths to a tree's leaves by mapping 0 to left nodes and 1 to right nodes *)
let rec paths (t : dTree) : int list list =
  match t with
  | Leaf(_) -> [[]]
  | Node(_, lt, rt) ->
      List.map (fun x -> 0::x) (paths lt)
      @ List.map (fun x -> 1::x) (paths rt);;

(* Returns whether a tree is perfect by comparing the heights of subtrees *)
let rec is_perfect (t : dTree) : bool =
  match t with
  | Leaf(_) -> true
  | Node(_, lt, rt) -> 
      if height lt = height rt
      then is_perfect lt && is_perfect rt
      else false;;

(* Maps each node's character to the function f and each leaf to the function g *)
let rec map (f : char -> char) (g : int -> int) (t : dTree) : dTree =
	match t with
	| Leaf(x) -> Leaf(g x)
	| Node(x, lt, rt) -> Node(f x, map f g lt, map f g rt);;

(* Returns a dTree from a list of characters through recursion and maps each leaf to 0 *)
let rec list_to_tree (l : char list) : dTree =
	match l with
	| [] -> Leaf(0)
	| h::t -> Node(h, list_to_tree t, list_to_tree t);;

(* Replaces each leaf with the associated path's value in f *)
(* Keeps track of all paths through recursive calls to a helper function *)
let replace_leaf_at (t : dTree) (f : (int list * int) list) : dTree =
	let rec helper (tree : dTree) (path : int list) : dTree =
		match tree with
		| Leaf(_) -> Leaf(List.assoc path f) (* Life saver *)
		| Node(x, lt, rt) -> Node(x, helper lt (path @ [0]), helper rt (path @ [1]))
	in helper t [];;

(* Returns a dTree from parsing a list of characters and a list of paths *)
(* Uses list_to_tree to create a dTree from the list of characters *)
(* Uses replace_leaf_at to replace leaves with those from the list of paths *)
let bf_to_dTree (l : char list * (int list * int) list) : dTree =
	let (x, y) = l in replace_leaf_at (list_to_tree x) y;;