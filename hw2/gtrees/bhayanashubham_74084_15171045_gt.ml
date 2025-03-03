(*Shubham Bhayana
  I pledge my honor that I have abided by the Stevens Honor System
  *)

type 'a gt = Node of 'a * ('a gt) list

let t : int gt = (*Sample Tree to test my code*)
  Node (33, [
    Node (12, []);
    Node (77, [
      Node (37, [Node (14, [])]);
      Node (48, []);
      Node (103, [])
    ])
  ])
 
  (* Given leaf function *)
  let mk_leaf : 'a -> 'a gt = 
    fun n ->
      Node(n, [])

  (* Gets the maximum height of the tree *)
  let rec height (t: 'a gt) : int = 
    match t with
      | Node (_, []) -> 1  
      | Node (_, subtrees) ->
      1 + List.fold_left (fun acc subtree -> max acc (height subtree)) 0 subtrees

  (* Gets the size of the tree *)
  let rec size (t: 'a gt) : int =
    match t with
      | Node (_, []) -> 1  
      | Node (_, subtrees) ->
      1 + List.fold_left (fun acc subtree -> acc + size subtree) 0 subtrees

  (* Gets all the different paths *)
  let rec paths_to_leaves (t: int gt) : int list list =
    match t with
    | Node(_, []) -> [[]] 
    | Node(_, subtrees) -> List.concat (List.mapi (fun i subtree -> List.map (fun path -> i :: path) (paths_to_leaves subtree)) subtrees);;

  (* Checks if all leaves are at the same depth *)
  let same_leaf_depths (t: 'a gt) : bool =
    match paths_to_leaves t with
    | [] -> true  
    | lst_depth :: rest -> List.for_all (fun depth -> List.length depth = List.length lst_depth) rest;;

  (* Returns a preorder traversal of the tree *)
  let rec preorder (t: 'a gt) : 'a list =
    match t with
    | Node (value, subtrees) ->
        value :: List.fold_right (fun subtree acc -> preorder subtree @ acc) subtrees []
  
  (* Mirrors the tree by reversing the order of subtrees *)
  let rec mirror (t: 'a gt) : 'a gt =
    match t with
    | Node (value, subtrees) -> Node (value, List.fold_left (fun acc subtree -> (mirror subtree) :: acc) [] subtrees)
  
  (* Applies function f to each node value in the tree *)
  let rec map f (t: 'a gt) : 'b gt = 
    match t with
    | Node (value, subtrees) -> Node (f value, List.map(fun subtree -> map f subtree) subtrees)

  (* Generalized fold function for tree traversal *)
  let rec fold (f: 'a -> 'b list -> 'b) (t: 'a gt) : 'b =
    match t with 
    | Node (value, subtrees) -> f value (List.map(fun subtree -> fold f subtree) subtrees)
    
  (* Alternate mirror function using fold instead of recursion *)
  let mirror' t =
    fold (fun value subtrees -> Node (value, List.fold_left (fun acc subtree -> subtree :: acc) [] subtrees)) t

  (* Computes the maximum degree (largest number of children at any node) *)
  let rec degree (t: 'a gt) : int =
    match t with
    | Node (_, []) -> 0  
    | Node (_, subtrees) -> max (List.length subtrees) (List.fold_left (fun acc subtree -> max acc (degree subtree)) 0 subtrees)
      

        
        
    
  
    

  
      
  