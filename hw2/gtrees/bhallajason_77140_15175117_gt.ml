(*
G Trees
Author: Jason Bhalla
I pledge my honor that I have abided by the Stevens Honor System.
2/23/25
*)

type 'a gt = Node of 'a*('a gt) list

(*
   
                33
                /\
              12  77
                / | \
               37 48 103
               |
               14

*)

let t : int gt =
  Node (33, [Node (12, []); Node (77, [Node (37, [Node (14, [])]); Node (48, []); Node (103, [])])])

let mk_leaf : 'a -> 'a gt =
  fun n ->
    Node(n, [])

(* ----------------------------------------------------------------- *)
(* Gets the height of the tree *)
let rec height : 'a gt -> int =
  fun (Node (_, l)) -> 1 + List.fold_left (fun i child -> max i (height child)) 0 l

(* ----------------------------------------------------------------- *)
(* Gets the number of nodes in the tree *)
let rec size : 'a gt -> int = 
  fun (Node (_, l)) -> 1 + List.fold_left (fun i child -> i + size child) 0 l
  (* Original attempt, but this counts the number of leaf nodes, not total nodes: fun (Node (_, l)) -> if (not Empty) then 1 else (List.fold_left(fun i child -> size child) 0 l) *)

(* ----------------------------------------------------------------- *)

(* Helper Function: Adds an integer to the front of each path *)
let rec add_to_path n paths =
  match paths with
  | [] -> []
  | h::t -> (n::h) :: add_to_path n t

let rec paths_from_children : int -> 'a gt list -> int list list =
  fun i children ->
    match children with
    | [] -> []
    | child::t -> add_to_path i (paths_to_leaves child) @ paths_from_children (i+1) t

(* Gets the paths from the root to the leaves *)
and paths_to_leaves : 'a gt -> int list list = (* Note: Since paths_to_leaves referenced in paths_from_children, I had to use 'and' instead of 'let rec' *)
  fun t ->
    match t with
    | Node (_, []) -> [[]]
    | Node (_, l) -> paths_from_children 0 l

(* ----------------------------------------------------------------- *)

(* Helper Function: Remove the duplicates from a list *)
let rec rem_dups l =
  match l with
  | [] -> []
  | h::t ->
    if List.mem h t
    then rem_dups t
    else h:: rem_dups t

(* Checks if the paths to leaves all have the same length, and if they do, it's leaf perfect *)
let is_leaf_perfect : 'a gt -> bool =
  fun t -> List.length (rem_dups (List.map List.length (paths_to_leaves t))) = 1

(* ----------------------------------------------------------------- *)
(* Preorder traversal *)
let rec preorder : 'a gt -> 'a list =
  fun (Node (a, l)) -> List.fold_left (@) ([a]) (List.map preorder l)
  (* Old attempt: | Node (a, h::t) -> (a :: preorder h) @ (List.fold_left (::) ([]) (List.map preorder h)) *)
        
(* ----------------------------------------------------------------- *)

(* Original:
Node (33, [Node (12, []); Node (77, [Node (37, [Node (14, [])]); Node (48, []); Node (103, [])])])
                33
                /\
              12  77
                / | \
               37 48 103
               |
               14
*)

(* Mirror:
Node (33, [Node (77, [Node (103, []); Node (48, []); Node (37, [Node (14, [])])]); Node (12, [])])
                33
                /\
              77  12
            / | \
          103 48 37
                 |
                14
*)

(* Mirrors the tree by switching the order of every child node *)
let rec mirror : 'a gt -> 'a gt =
  fun t ->
    match t with
    | Node (a, []) -> Node(a, [])
    | Node (a, l) -> Node(a, List.rev (List.map mirror l))
    
(* ----------------------------------------------------------------- *)
(* Map *)
let rec map : ('a -> 'b) -> 'a gt -> 'b gt =
  fun f t ->
    match t with
    | Node (a, l) -> Node(f a, List.map (map f) l)

(* ----------------------------------------------------------------- *)
(* Fold *)
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node (a, l)) ->
    f a (List.map (fold f) l)

(* ----------------------------------------------------------------- *)
(* Mirror with fold *)
let mirror' : 'a gt -> 'a gt =
  fun t ->
    fold (fun a rs -> Node(a, List.rev rs)) t

(* ----------------------------------------------------------------- *)
(* Gets the degree of the tree*)
let rec degree : 'a gt -> int =
  fun (Node (_, l)) ->
    max (List.length l) (List.fold_left (fun d child -> max d (degree child)) 0 l)
