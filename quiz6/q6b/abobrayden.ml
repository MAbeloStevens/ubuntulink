(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Brayden Abo
    Name 2: Ian Nevins
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
  let rec helper node =
    match node with
    | None -> 0
    | Some n -> 
        1 + max (helper n.left) (helper n.right)
  in helper t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
  fun e t  ->
  let rec helper node =
    match node with
    | None -> false
    | Some n -> 
        n.data = e || helper n.left || helper n.right
  in helper t.root

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
  let rec helper node x =
    match node with
    | None -> x
    | Some n -> 
        n.data :: (helper n.left (helper n.right x))
  in helper t.root []

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
  let rec helper node =
    match node with
    | None -> ()
    | Some n -> 
        n.data <- f n.data;
        helper n.left;
        helper n.right
  in helper t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).

*)
let mirror : 'a bt -> unit =
  fun t ->
  let rec swap node =
    match node with
    | None -> ()
    | Some n ->
        let temp = n.left in
        n.left <- n.right;
        n.right <- temp;
        swap n.left;
        swap n.right
  in swap t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
  let new_node = { data = e; left = None; right = None } in
  match t.root with
  | None -> 
      t.root <- Some new_node;
      t.size <- t.size + 1
  | Some _ ->
      let rec insert parent =
        if e < parent.data then
          match parent.left with
          | None -> parent.left <- Some new_node
          | Some left_child -> insert left_child
        else
          match parent.right with
          | None -> parent.right <- Some new_node
          | Some right_child -> insert right_child
      in
      insert (Option.get t.root);
      t.size <- t.size + 1


