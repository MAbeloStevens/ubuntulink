(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Justin Carnemolla
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
let rec height_helper (n : 'a node option) : int =
  match n with
  | None -> 0
  | Some node -> 1 + max (height_helper node.left) (height_helper node.right)

let height (t : 'a bt) : int =
  height_helper t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let rec mem_helper (e : 'a) (n : 'a node option) : bool =
  match n with
  | None -> false
  | Some node ->
      if node.data = e then true
      else mem_helper e node.left || mem_helper e node.right

let mem (e : 'a) (t : 'a bt) : bool =
  mem_helper e t.root

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let rec preorder_helper (n : 'a node option) : 'a list =
  match n with
  | None -> []
  | Some node -> node.data :: (List.append (preorder_helper node.left) (preorder_helper node.right))

let preorder (t : 'a bt) : 'a list =
  preorder_helper t.root

(** [map f t] maps [f] to every element of [t].
*)
let rec map_helper (f : 'a -> 'a) (n : 'a node option) : unit =
  match n with
  | None -> ()
  | Some node ->
      node.data <- f node.data;
      map_helper f node.left;
      map_helper f node.right

let map (f : 'a -> 'a) (t : 'a bt) : unit =
  map_helper f t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let rec mirror_helper (n : 'a node option) : unit =
  match n with
  | None -> ()
  | Some node ->
      let temp = node.left in
      node.left <- node.right;
      node.right <- temp;
      mirror_helper node.left;
      mirror_helper node.right

let mirror (t : 'a bt) : unit =
  mirror_helper t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let rec add_helper (e : 'a) (n : 'a node option) : 'a node option =
  match n with
  | None -> Some { data = e; left = None; right = None }
  | Some node ->
      if e < node.data then node.left <- add_helper e node.left
      else node.right <- add_helper e node.right;
      Some node

let add (e : 'a) (t : 'a bt) : unit =
  t.root <- add_helper e t.root;
  t.size <- t.size + 1


