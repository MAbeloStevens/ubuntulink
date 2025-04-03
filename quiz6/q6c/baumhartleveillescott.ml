(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Scott Baumhart Leveille
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

(*     ee
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
let rec height_helper : 'a node option -> int = 
  fun n ->
    match n with
    | None -> 0
    | Some(n) -> 1 + max (height_helper n.right) (height_helper n.left)

let height : 'a bt -> int =
  fun t ->
    height_helper t.root



(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let rec mem_helper : 'a -> 'a node option -> bool = 
  fun e n ->
    failwith "implement"

let rec mem : 'a -> 'a bt -> bool =
  fun e t  ->
  mem_helper e t.root
  

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
  failwith "implement"

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
  failwith "implement"

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


