(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Mikkail Allen
    Name 2:
*)

type 'a node = { mutable data: 'a;
                 mutable left: 'a node option;
                 mutable right: 'a node option}

type 'a bt = { mutable root: 'a node option ;
               mutable size: int}

let t1 : int bt =
  { root = None;
    size = 0}

(*     
       33
      / \
     22  44
         /
        37
*)

let t2 : int bt =
  { root = Some { data = 33;
                  left = Some {data=22; left=None; right=None} ;
                  right = Some {data=44;
                                left = Some {data=37; left=None;
                                             right=None};
                                right = None}
                };
    size = 4
  }

(** [height t] returns the height of [t]. 
    Eg. [height t2 ==> 3]
*)
let height : 'a bt -> int =
  fun t ->
  let rec node_height = function
  	| None -> 0
  	| Some node -> 1 + max (node_height node.left) (node_height node.right)
  in
  node_height t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
  fun e t  ->
  let rec is_mem node =
  	match node with
        | None -> false
        | Some { data; left; right } -> 
        if e = data 
	then true
        else (is_mem left) || (is_mem right)
  in
  is_mem t.root

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
  let rec visit_node = function
      | None -> []
      | Some node -> 
          let left_result = visit_node node.left in
          let right_result = visit_node node.right in
          node.data :: (left_result @ right_result)
    in
    visit_node t.root 

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
  let rec apply_function = function
  	| None -> None
        | Some node -> 
        	Some {
                data = f node.data; 
            	left = apply_function node.left;
            	right = apply_function node.right;
          	}
  in
  t.root <- (apply_function t.root)

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
  failwith "implement"

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
  failwith "implement"


