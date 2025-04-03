(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Vidhur Busannagari
    Name 2: Aarya Radhan
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
let rec height_node (n : 'a node option) : int =
  match n with
  | None -> 0
  | Some node ->
    1 + max (height_node node.left) (height_node node.right)

let height : 'a bt -> int =
  fun t ->
    height_node t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let rec mem_node (e : 'a) (n : 'a node option) : bool =
  match n with
  | None -> false
  | Some node ->
    if node.data = e then true
    else mem_node e node.left || mem_node e node.right

let mem : 'a -> 'a bt -> bool =
  fun e t  ->
    mem_node e t.root
(**
check it
*)
  
(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let rec preorder_node (n : 'a node option) : 'a list =
  match n with
  | None -> []
  | Some node ->
    node.data :: preorder_node node.left @ preorder_node node.right

let preorder : 'a bt -> 'a list =
  fun t ->
    preorder_node t.root

(** [map f t] maps [f] to every element of [t].
*)
let rec map_node (f : 'a -> 'a) (n : 'a node option) : unit =
  match n with
  | None -> ()
  | Some node ->
    node.data <- f node.data;
    map_node f node.left;
    map_node f node.right

let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
    map_node f t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let rec mirror_node (n : 'a node option) : unit =
  match n with
  | None -> ()
  | Some node ->
    let temp = node.left in
    node.left <- node.right;
    node.right <- temp;
    mirror_node node.left;
    mirror_node node.right

let mirror : 'a bt -> unit =
  fun t ->
    mirror_node t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let rec add_node (e : 'a) (n : 'a node) : unit =
  if e < n.data then
    match n.left with
    | None -> n.left <- Some { data = e; left = None; right = None }
    | Some l -> add_node e l
  else
    match n.right with
    | None -> n.right <- Some { data = e; left = None; right = None }
    | Some r -> add_node e r

let add : 'a -> 'a bt -> unit =
  fun e t ->
    match t.root with
    | None ->
      t.root <- Some { data = e; left = None; right = None };
      t.size <- 1
    | Some root ->
      add_node e root;
      t.size <- t.size + 1


