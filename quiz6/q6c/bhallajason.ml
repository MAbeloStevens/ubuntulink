(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Jason Bhalla
    Name 2: Abu Sayiem
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
let rec height_n (n : 'a node option) : int = 
  match n with
  | None -> 0
  | Some node -> 
    1 + max (height_n node.left) (height_n node.right)

let height : 'a bt -> int =
  fun t ->
  height_n t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let rec mem_n e (n : 'a node option) : bool =
  match n with
  | None -> false
  | Some node ->
      let c = compare e node.data in
      if c = 0 then true
      else if c < 0 then mem_n e node.left
      else mem_n e node.right

let mem : 'a -> 'a bt -> bool =
  fun e t  ->
    mem_n e t.root

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let rec preorder_n (n : 'a node option) : 'a list = 
  match n with
  | None -> []
  | Some node ->
    node.data :: (preorder_n node.left @ preorder_n node.right)

let preorder : 'a bt -> 'a list =
  fun t ->
    preorder_n t.root



  
(** [map f t] maps [f] to every element of [t].
*)

(* Helper: Does the map on a node *)
let rec map_n : ('a -> 'a) -> 'a node option -> unit =
  fun f n ->
    match n with
    | None -> ()
    | Some node ->
      node.data <- f node.data;
      map_n f node.left;
      map_n f node.right

let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
  map_n f t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)

let rec mirror_n : 'a node option -> unit = 
  fun n ->
    match n with
    | None -> ()
    | Some node ->
      let tmp = node.left in
      node.left <- node.right;
      node.right <- tmp;
      mirror_n node.left;
      mirror_n node.right

let mirror : 'a bt -> unit =
  fun t ->
  mirror_n t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)

let rec add_n : 'a -> 'a node -> unit =
  fun e n ->
    if compare e n.data < 0 then
      match n.left with
      | None -> n.left <- Some {data = e; left = None; right = None}
      | Some l -> add_n e l
    else
      match n.right with
      | None -> n.right <- Some {data = e; left = None; right = None}
      | Some r -> add_n e r

let add : 'a -> 'a bt -> unit =
  fun e t ->
  match t.root with
  | None -> t.root <- Some {data = e; left = None; right = None}
  | Some n -> add_n e n;
  t.size <- t.size + 1


