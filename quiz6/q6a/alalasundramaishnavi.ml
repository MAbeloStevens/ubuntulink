(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Aishnavi Alalasundram
    Name 2: Jingyi Guo
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
    let rec n_height no =
      match no with 
      | None -> 0
      | Some n -> 1 + max (n_height n.left) (n_height n.right)
    in
    n_height t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
  fun e t  ->
  let rec n_mem no =
    match no with
    | None -> false
    | Some n -> 
        if e = n.data then true
        else n_mem n.left || n_mem n.right
      in
      n_mem t.root

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
  let rec traverse no =
    match no with
    | None -> []
    | Some n -> n.data :: (traverse n.left @ traverse n.right)
  in
  traverse t.root

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
  let rec map_help no =
    match no with
    | None -> ()
    | Some n ->
        n.data <- f n.data;
        map_help n.left;
        map_help n.right
  in
  map_help t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
  let rec mirr_help no =
    match no with
    | None -> ()
    | Some n ->
        let cur = n.left in
        n.left <- n.right;
        n.right <- cur;
        mirr_help n.left;
        mirr_help n.right
  in
  mirr_help t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
  let rec add_first no =
    match no with 
    | None -> Some {data=e; left=None; right=None}
    | Some n ->
        if e < n.data then
          n.left <- add_first n.left
        else
          n.right <- add_first n.right;
        Some n
  in
  t.root <- add_first t.root;
  t.size <- t.size+1


