(*
G Trees
Author: Aidan Cancelliere
*)

(* Define a tree type *)
type 'a gt = Node of 'a * ('a gt) list;;

(* Example tree *)
let t : int gt =
  Node (33 ,
        [Node (12,[]);
         Node (77,
               [Node (37,
                      [Node (14,[])]);
                Node (48,[]);
                Node (103,[])])
        ])

let rec height : 'a gt -> int =
  fun t ->
    match t with
    | Node(_, []) -> 1 (* Leaf, no children*)
    | Node(_, children) -> 
      let counter = 0 in (* Start with zero for comparison*)
      let max_child_height = 
        (* Get max height among children*)
        List.fold_left (fun acc child -> max acc (height child)) counter children
      in
      1 + max_child_height;; (* Add 1 for the current node*)

let rec size : 'a gt -> int = 
  fun t ->
  match t with
  | Node (_, children) ->
      (* Sum sizes of children *)
      1 + List.fold_left (fun acc child -> acc + size child) 0 children

let rec paths_to_leaves : 'a gt -> int list list =
  fun t ->
  match t with
  | Node (_, []) -> [[]] (* Leaf: one empty path*)
  | Node (_, children) ->
    let (_, paths) =
      List.fold_left (fun (i, acc) child ->
        let child_paths = paths_to_leaves child in (* Get child paths *)
        let paths_with_index = List.map (fun path -> i :: path) child_paths in
        (i + 1, acc @ paths_with_index) (* Increment index and add paths*)
      ) (0, []) children
    in
    paths
    

let is_leaf_perfect : 'a gt -> bool = 
  fun t ->
  (* Collects the depths of all leaves in the tree. *)
  let rec leaf_depths tree d =
    match tree with
    | Node (_, []) -> [d]   (* Leaf: return its depth in a list. *)
    | Node (_, children) ->
        (* Append depths from children*)
        List.fold_left (fun acc child -> acc @ (leaf_depths child (d + 1))) [] children
  in
  match leaf_depths t 1 with
  | [] -> true  (* Should not happen for non-empty trees *)
  | h :: t ->
      (* Map each depth in tail to a boolean: true if it equals h, false otherwise *)
      let comparisons = List.map (fun x -> x = h) t in
      (* Fold the list with && to check if all comparisons are true *)
      List.fold_left (&&) true comparisons

let rec preorder : 'a gt -> 'a list = 
  fun (Node (value, children)) ->
    (* For each child, recursively get its pre-order list and then flatten the list of lists *)
    value :: List.concat (List.map preorder children)
      
let rec mirror : 'a gt -> 'a gt =
  fun (Node (value, children)) ->
    (* Recursively mirror each child and then reverse order to complete mirror effect*)
    Node (value, List.rev (List.map mirror children))
    
let rec map : ('a -> 'b) -> 'a gt -> 'b gt =
  fun f (Node (value, children)) ->
    (* Apply f to the current value *)
    let new_value = f value in
    (* Recursively apply map to each child *)
    let new_children = List.map (map f) children in
    Node (new_value, new_children)

let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node (value, children)) ->
    (* Recursively fold each child to get a list of results *)
    let results = List.map (fold f) children in
    (* Combine the current node's value with the list of results from its children *)
    f value results
    
let mirror' : 'a gt -> 'a gt =
  fold (fun value children ->
    (* Reverse the list of results (mirrored children) for each node *)
    Node (value, List.rev children)
  )
    
  let rec degree : 'a gt -> int = 
    fun (Node (_, children)) ->
    let current_degree = List.length children in (* Count current children*)
    let child_degrees = List.map degree children in (* Get degree of each child*)
    let max_child_degree = List.fold_left max 0 child_degrees in (* Find max degree among children*)
    max current_degree max_child_degree (* Return the maximum value*)
  