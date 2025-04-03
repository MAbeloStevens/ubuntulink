(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Adrian Chacon
    Name 2: Gabriel Castillo
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
  let rec height_node node = 
    match node with
    | None -> 0
    | Some n ->
      1 + max(height_node n.left)(height_node n.right)
    in
    height_node t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
  fun e t  ->
  let rec mem_node node = 
    match node with
    | None -> false
    | Some n ->
      n.data = e || mem_node n.left || mem_node n.right
    in
    mem_node t.root
      

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
  let rec preorder_node node acc = 
    match node with
    | None -> acc
    | Some n ->
      n.data :: (preorder_node n.left (preorder_node n.right acc))
    in
    preorder_node t.root[]

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
  let rec map_node node = 
    match node with
    | None -> ()
    | Some n ->
      n.data <- f n.data;
      map_node n.left;
      map_node n.right;
    in
    map_node t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
  let rec mirror_node node =
    match node with
    | None -> ()
    | Some n ->
      let temp = n.left in
      n.left <- n.right;
      n.right <- temp;
      mirror_node n.left;
      mirror_node n.right;
    in 
    mirror_node t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
  failwith "implement"


