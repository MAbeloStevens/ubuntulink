(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: James Barbi
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
let rec height : 'a bt -> int =
  fun t ->
  let rec height_node n =
    match n with
    | None -> 0
    | Some node -> 1 + max (height_node node.left) (height_node node.right)
  in height_node t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
  fun e t  ->
  let rec mem_node n =
    match n with
    | None -> false
    | Some node -> (node.data = e) || (mem_node node.left) || (mem_node node.right)
  in mem_node t.root

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
  let rec pre_order n =
    match n with
    | None -> []
    | Some node -> node.data :: (pre_order node.left @ pre_order node.right)
  in pre_order t.root

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
  let rec map_node n =
    match n with
    | None -> ()
    | Some node -> 
      node.data <- f node.data;
      map_node node.left;
      map_node node.right;
  in map_node t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
  let rec mirror_node n =
    match n with
    | None -> ()
    | Some node -> let temp = node.left in
                    node.left <- node.right;
                    node.right <- temp;
                    mirror_node node.left;
                    mirror_node node.right;
  in mirror_node t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
  let rec add_node n =
    match n with
    | None -> { data = e; left = None; right = None }
    | Some node -> 
      if e < node.data then
        node.left <- Some (add_node node.left)
      else
        node.right <- Some (add_node node.right);
      node
  in t.root <- Some (add_node t.root);
      t.size <- t.size + 1