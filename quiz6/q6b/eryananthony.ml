(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Anthony Eryan
    Name 2:
*)

type 'a node = { mutable data: 'a;
                 mutable left: 'a node option;
                 mutable right: 'a node option}

type 'a bt = { mutable root: 'a node option;
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
                  left = Some {data=22; left=None; right=None};
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
    match t with
    | { root = None; size = 0 } -> 0
    | { root = Some node; size } ->
        let rec height_helper n =
          match n with
          | None -> 0
          | Some node -> 1 + max (height_helper node.left) (height_helper node.right)
        in
        height_helper t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t].
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
  fun e t -> 
    match t with
    | { root = None; size = 0 } -> false
    | { root = Some _; size = _ } ->
        let rec mem_helper n =
          match n with
          | None -> false
          | Some node ->
              if node.data = e then true
              else mem_helper node.left || mem_helper node.right
        in
        mem_helper t.root

(** [preorder t] returns a list with the preorder traversal of [t].
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
    match t with
    | { root = None; size = 0 } -> []
    | { root = Some node; size } ->
        let rec preorder_helper n =
          match n with
          | None -> []
          | Some node ->
              node.data :: (preorder_helper node.left @ preorder_helper node.right)
        in
        preorder_helper t.root

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t -> 
    match t with
    | { root = None; size = 0 } -> () (* empty tree, nothing to map *)
    | { root = Some node; size } ->
        let rec map_helper n =
          match n with
          | None -> ()
          | Some node ->
              (* apply f to the current node's data *)
              node.data <- f node.data;
              (* recursively map on left and right children *)
              map_helper node.left;
              map_helper node.right
        in
        map_helper t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t -> 
    match t with
    | { root = None; size = 0 } -> () (* empty tree, nothing to mirror *)
    | { root = Some _; size = _ } ->
        let rec mirror_helper n =
          match n with
          | None -> ()
          | Some node ->
              let temp = node.left in
              node.left <- node.right;
              node.right <- temp;
              mirror_helper node.left;
              mirror_helper node.right
        in
        mirror_helper t.root

(** [add e t] adds [e] to [t].
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
    match t.root with
    | None ->
        t.root <- Some { data = e; left = None; right = None };
        t.size <- t.size + 1
    | Some _ ->
        let rec add_helper n =
          match n with
          | None -> Some { data = e; left = None; right = None }
          | Some node ->
              if e < node.data then
                node.left <- add_helper node.left
              else if e > node.data then
                node.right <- add_helper node.right
              else
                (* Element already exists, but precondition states e is not in t *)
                failwith "Element already exists in the tree";
              Some node
        in
        ignore (add_helper t.root);
        t.size <- t.size + 1