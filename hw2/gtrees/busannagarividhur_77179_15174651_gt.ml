(*
Homework Assignment 2
Author: Vidhur Busannagari
Pledge: I pledge my honor that I have abided by the stevens honor system - VB
*)

(* General tree type with polymorphic values and variable number of children *)
type 'a gt = Node of 'a * ('a gt) list

(* Returns height of tree (longest path from root to leaf) *)
let rec height : 'a gt -> int = function
  | Node (_, []) -> 1
  | Node (_, children) -> 1 + List.fold_left (fun acc t -> max acc (height t)) 0 children

(* Returns total number of nodes in the tree *)
let rec size : 'a gt -> int = function
  | Node (_, []) -> 1
  | Node (_, children) -> 1 + List.fold_left (fun acc t -> acc + size t) 0 children

(* Returns all paths from root to leaves as lists of child indices *)
let rec paths_to_leaves (Node (_, children)) =
  match children with
  | [] -> [[]]
  | _  ->
    List.mapi (fun i child ->
      List.map (fun path -> i :: path) (paths_to_leaves child))
      children
    |> List.flatten

(* Checks if all leaves are at the same depth *)
let is_leaf_perfect t =
  let depths = List.map List.length (paths_to_leaves t) in
  List.for_all ((=) (List.hd depths)) depths

(* Returns list of node values in pre-order traversal *)
let rec preorder (Node (v, children)) =
  v :: List.flatten (List.map preorder children)

(* Returns tree with children lists reversed at each node *)
let rec mirror (Node (v, children)) =
  Node (v, List.rev (List.map mirror children))

(* Applies function f to each node value *)
let rec map f (Node (v, children)) =
  Node (f v, List.map (map f) children)

(* Recursion scheme for general trees *)
let rec fold f (Node (v, children)) =
  f v (List.map (fold f) children)

(* Alternative mirror implementation using fold *)
let mirror' t = 
  fold (fun v rs -> Node (v, List.rev rs)) t

(* Returns maximum number of children of any node *)
let rec degree (Node (_, children)) =
  max (List.length children) (List.fold_left (fun acc t -> max acc (degree t)) 0 children)



(* Testing Work
let t : int gt =
  Node (33 ,
        [ Node (12 ,[]) ;
          Node (77 ,
                [ Node (37 ,
                      [ Node (14 , [])]) ;
                  Node (48 , []) ;
                  Node (103 , [])])
        ])

let sum t =
  fold (fun i rs -> i + List.fold_left (+) 0 rs) t

let mem t e =
  fold (fun i rs -> i = e || List.exists ((=) true) rs) t

let mirror' t =
  fold (fun v rs -> Node (v, List.rev rs)) t
*)

