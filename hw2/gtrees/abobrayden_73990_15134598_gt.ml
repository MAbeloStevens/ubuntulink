(*
General Trees : Homework #2
Author:  Brayden Abo
Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type 'a gt = Node of 'a *('a gt) list


let t : int gt =
  Node (33, [
    Node (12, []);
    Node (77, [
      Node (37, [
        Node (14, [])
      ]);
      Node (48, []);
      Node (103, [])
    ])
  ])

(* Helper function to determine if a node is a leaf. *)
let is_leaf node =
  match node with
  | Node (_, []) -> true  (* No children, it's a leaf *)
  | _ -> false;;

(* Returns the height of a tree *)
let rec height tree =
  if is_leaf tree then 1 
  else 
    match tree with 
    | Node(_, children) -> 
      1 + List.fold_left (fun num child -> max num (height child)) 0 children

(* Returns the size of a tree *)   
let rec size tree = 
  if is_leaf tree then 1 
  else
    match tree with 
    | Node(_, children) ->
      1 + List.fold_left (fun num child -> num + size child) 0 children

(* Helper function for paths_to_leaves, stores path and recursively adds appropriate indexes*)
let rec path_helper path tree =
  if is_leaf tree then [List.rev path] (* Base case: Reverse the path as we worked down -> up during recursion*)
  else
    (* Use mapi to create from index 0 to appropriate number of recursion calls *)
    match tree with
    | Node(_, children) ->
      List.flatten (List.mapi (fun index child -> path_helper (index :: path) child) children)

(* Returns a list of paths from the root to all leaves in the tree, utilizes above helper function *)
let rec paths_to_leaves tree =
  path_helper [] tree

(* Helper function for is_leaf_perfect, check if all heights of a list is equal *)
let equal_heights children =
  match children with
  | [] -> true  (* Empty list is true*)
  | first :: rest -> List.for_all (fun x -> x = first) rest;; (* Check if all values in list are equal *)

(* Returns a boolean indicating if a tree is leaf perfect *)
let rec is_leaf_perfect tree =
  if is_leaf tree then true 
  else 
    match tree with
    | Node(_, children) -> 
      (* Create a list of heights for a Node's children and apply equal_heights() and recursively call itself on the children *)
      let heights = List.map height children in
      (equal_heights heights) && (List.for_all is_leaf_perfect children);;

(* Returns preorder traversal of a general tree *)
let rec preorder tree =
  match tree with 
  | Node(x, []) -> [x] (* If no children, print value*)
  (* Root @ Left @ Right*)
  | Node(x, children) ->
    [x] @ List.flatten (List.map (fun child -> preorder child) children)



(* Mirrors the image of a general tree by reversing the lists in each child of a node*)
let rec mirror tree =
  match tree with
  | Node (x, children) -> Node (x, List.rev (List.map mirror children))

(* Produces a tree cosnisting of nodes from tree with function f applied to the node's data *)
let rec map f tree =
  match tree with 
  | Node(x, []) -> Node(f x, []) (* Base case -> Apply function to just the data in leaf node. *)
  | Node(x, children) -> Node(f x, List.map (map f) children)

(* Encodes the recursion scheme over general trees *)
let rec fold f (Node (x, children)) =
  f x (List.map (fold f) children)

(* Implmentation of mirror, using fold from excerise 8. *)
let mirror_fold tree =
  fold (fun x children -> Node (x, List.rev children)) tree

(* Returns maximum number of children in a tree, uses fold() from above. *)
let degree tree =
  fold (fun _ childs -> max (List.length childs)(List.fold_left max 0 childs)) tree

