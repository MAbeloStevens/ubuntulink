(*******************************************************************************
 * Name        : gt.ml
 * Author      : Christopher Bernard
 * Pledge      : I pledge my honor that I have abided by the Stevens Honor System.
 ******************************************************************************)

type 'a gt = Node of 'a *('a gt ) list

let t : int gt =
  Node (33 ,
    [ Node (12 ,[]) ;
      Node (77 ,
      [ Node (37 ,
        [ Node (14 , [])]) ;
      Node (48 , []) ;
      Node (103 , [])])
    ])

let t2 : int gt =
  Node (22 ,[ 
    Node(1, [
      Node(10, []);
      Node(20, [])
    ]);
    Node(2, [
      Node(30, []);
      Node(40, [])
    ]);
    Node(3, [
      Node(50, []);
      Node(60, [])
    ])
  ])

let t3 : int gt = 
  Node (1, [])

(* returns the length of the longest path from root to leaf *)
let rec height : 'a gt -> int =
  fun tree ->
  match tree with
  | Node(_, []) -> 1
  | Node(_, l) -> 1 + List.fold_right (fun child currMax -> max (height child) currMax ) l 0

(* returns the number of nodes in a general tree *)
let rec size : 'a gt -> int =
  fun tree ->
  match tree with
  | Node(_, []) -> 1
  | Node(_, l) -> 1 + List.fold_right (fun child total -> (size child) + total) l 0

(* returns the integer paths to all leaves in a general tree *)
let rec paths_to_leaves : 'a gt -> int list list =
  fun tree ->
  let rec ptl_helper : 'a gt list -> int -> int list list =
    fun list i -> (* processes children, takes in an index field that increments for each child *)
    match list with
    | [] -> [] 
    | h::t -> 
      (List.map (fun p -> i :: p) (paths_to_leaves h)) @ (ptl_helper t (i+1))
  in match tree with
  | Node (d, []) -> [[]]
  | Node (d, l) -> ptl_helper l 0 (* call ptl_helper starting at 0 for the first child *)

(* returns a boolean representing whether or not all of the general tree's leaves are on the same level *)
let rec is_leaf_perfect : 'a gt -> bool =
  fun tree ->
    (* if the lengths of all paths to leaves are the same, then the tree is perfect *)
    let rec perfect_helper : int list list -> int -> bool = 
      fun list size ->
      match list with
      | [] -> true (* if list is empty then all lists have the same length *)
      | h::t -> 
        if (size = 0) (* checks if handling first iteration of helper function *)
        then perfect_helper list (List.length h)
        else ((List.length h)=size) && perfect_helper t (List.length h)
      in
    perfect_helper (paths_to_leaves tree) 0 (* calls helper function with result of paths_to_leaves *)

(* returns a preorder traversal by recursively prepending the data through folding the children into a list *)
let rec preorder : 'a gt -> 'a list =
  fun tree ->
  match tree with
  | Node(d, []) -> [d]
  | Node(d, l)  -> d :: List.fold_right (fun child list -> (preorder child) @ list) l []

(* returns a mirrored version of the given general tree *)  
let rec mirror: 'a gt -> 'a gt =
  fun tree ->
  match tree with
  | Node(d, l) -> Node(d, List.rev (List.map mirror l))

(* performs the given function on all nodes in a general tree *)
let rec map: ('a -> 'b) -> 'a gt -> 'b gt =
  fun func tree -> 
  match tree with
  | Node(d, []) -> Node(func d, [])
  | Node(d, l) -> Node(func d, List.map (map func) l)

(* performs the recursion scheme over a general tree *)
let rec fold: ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun func tree ->
  match tree with
  | Node(d, []) -> func d []
  | Node(d, l) -> func d (List.map (fold func) l)

(* mirrors the tree using fold instead of recursion *)
let mirror': 'a gt -> 'a gt =
  fun tree ->
  fold (fun i rs -> Node(i, List.rev rs)) tree

(* returns the maximum number of a children a node has *)
let degree: 'a gt -> int =
  fun tree ->
  fold (fun i rs -> max (List.length rs) (List.fold_right max rs 0)) tree

