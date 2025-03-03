(*
G Trees
Author: Sarah Basil
Date: 2/23/2025
*)

(* tree definition *)
type 'a gt = Node of 'a * ('a gt) list


(*the height of a general tree -> the length of the longest path 
from the root to a leaf, in terms of number of nodes.*)

let rec height : 'a gt -> int = fun t ->
  match t with
  | Node(_, []) ->
      (* leaf node has height = 1 
         (just node, and no children). *)
      1
  | Node(_, children) ->
      (* internal node has height:
         1 + the maximum height of any child. *)
      1 + List.fold_left
          (fun current_max child ->
             let child_h = height child in
             if child_h > current_max then child_h else current_max
          )
          0
          children



(*the size of a general tree -> total number of nodes in the tree.*)      

let rec size : 'a gt -> int = fun t ->
  match t with
  | Node(_, children) ->
      (* 1 for root node, plus the sum of sizes of each child *)
      1 + List.fold_left
          (fun acc child ->
              acc + size child
          )
          0
          children


(*Returns a list of all paths from the root to each leaf, 
where each path is a list of child‐indices used to descend 
from the root. If a node has n children, they are labeled 0,1,..
.,n-1. A path is a list of integers in that range.*)

let rec paths_to_leaves : 'a gt -> int list list =
fun t ->
  match t with
  | Node(_, []) ->
      (* leaf node => only one path: empty list, 
           no child-steps to arrive here. *)
      [ [] ]
  | Node(_, children) ->
      (* internal node with children --> collect
          all leaf-paths from each child, prefix  
          with the child's index i. Then concatenate. *)

      (* iterate over children with fold_left, keep 
          track of the child index 'i': *)
      let f (i, all_paths_so_far) child =
        let subpaths = paths_to_leaves child in
        (* prefixing each subpath with i:  i :: subpath *)
        let subpaths = List.map (fun p -> i :: p) subpaths in
        (* join them to our running total *)
        let new_paths = all_paths_so_far @ subpaths in
        (i + 1, new_paths)
      in

      (* fold_left starting at index=0, empty list of paths *)
      let (_, result_paths) =
        List.fold_left f (0, []) children
      in
      result_paths


(* [is_leaf_perfect t] returns true if all leaves in [t] are at
   the same depth, false otherwise. *)

let is_leaf_perfect : 'a gt -> bool =
    fun t ->

    (* helper to collect all leaves' depths *)
    let rec leaf_depths t current_depth =
        match t with
        | Node(_, []) ->
            (* it's leaf, so recording depth*)
            [ current_depth ]
        | Node(_, children) ->
            (* for each child --> gather leaf depths with depth+1,
                 concatenate all (will use List.concat) *)
            let depths_from_each_child =
                List.map (fun c -> leaf_depths c (current_depth + 1)) children
            in
            List.concat depths_from_each_child
    in

    (* gathering all leaf depths, starting at depth 0 for root *)
    let ds = leaf_depths t 0 in

    (* If ds is empty, tree might be empty 
    but assignment says the tree is non-empty. 
    So, will check if every depth in ds equals the first one. *)
    match ds with   
    | [] ->
        (* case if tree is empty, or no leaves. 
        Will assume non-empty*)
        true
    | first_depth :: _ ->
        (* will see if all are the same as 'first_depth', using List.for_all function*)
        List.for_all (fun d -> d = first_depth) ds


(* [preorder t] returns a list of the node values in 
   pre-order: root first, then children from left to right. *)

let rec preorder : 'a gt -> 'a list =
fun t ->
    match t with
    | Node(x, []) ->
        (* leaf -> return [x] *)
        [x]
    | Node(x, children) ->
        (* start with [x], ecursively gather 
            each child's preorder, append them all. *)
        let children_preorders =
        List.map preorder children  (* each child is a list of 'a *)
        in
        x :: (List.concat children_preorders)
  

(* [mirror t] returns a new general tree that is the "mirror 
image"of [t]. Each node's children are reversed, and 
transformation is applied recursively to the children 
as well. *)

let rec mirror : 'a gt -> 'a gt =
fun t ->
    match t with
    | Node(x, children) ->
        (* reverse the list of children (will try to use List.rev)
            then can map mirror to each reversed child. *)
        let reversed = List.rev children in
        Node(x, List.map mirror reversed)
  

(* map : ('a -> 'b) -> 'a gt -> 'b gt 
   [map f t] applies [f] to each node's data in [t],
   building a new tree of the same shape but with
   data replaced by (f data).
*)
let rec map (f : 'a -> 'b) (t : 'a gt) : 'b gt =
    match t with
    | Node(x, children) ->
        Node( f x, List.map (map f) children )

        
(* fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b
   [fold f t] recursively processes the tree [t].
   At each node (x, children), we fold the children
   to get a list of 'b results, then call f x on child_results.
*)
let rec fold (f : 'a -> 'b list -> 'b) (t : 'a gt) : 'b =
    match t with
    | Node(x, children) ->
        (* first will fold each child, building a list of 'b 
           results in the same order: *)
        let subresults = List.map (fold f) children in
        (* then combine them with x: *)
        f x subresults


(* mirror' : 'a gt -> 'a gt
   Again, rebuilds the tree in mirrored form, 
   but via fold. Pass function that:
   -> gets x (node data)
   -> gets child results (mirrored subtrees)
   -> returns Node(x, reversed child results)
*)

let mirror' (t : 'a gt) : 'a gt =
    fold (fun x mirrored_children ->
      (* mirrored_children is  list of 
         already-mirrored subtrees, in the *original* order 
         because we folded from left to right. 
         This means -> reverse that list to complete
         parent's mirror. *)
      Node(x, List.rev mirrored_children)
    ) t


(* [degree t] returns max number of children of any node in t. 
   If some node has 5 children, that's possible candidate for the 
   degree (as an example)*)

let rec degree : 'a gt -> int =
    fun t ->
    match t with
    | Node(_, children) ->
        let children_degs =
          List.map degree children
        in
        let max_subdeg =
          List.fold_left max 0 children_degs 
        in
        max (List.length children) max_subdeg 
        (*List.length children -> how many children the node has*)