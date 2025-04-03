(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Lucas Burdian
    Name 2: n/a
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
let rec height_of_node (n: 'a node option) : int =
  match n with
  | None -> 0
  | Some node ->
      1 + max (height_of_node node.left) (height_of_node node.right)

let height (t: 'a bt) : int =
  height_of_node t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let rec mem_node (e: 'a) (n: 'a node option) : bool =
  match n with
  | None -> false
  | Some node ->
      if node.data = e then true
      else mem_node e node.left || mem_node e node.right

let mem (e: 'a) (t: 'a bt) : bool =
  mem_node e t.root

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let rec preorder_node (n: 'a node option) : 'a list =
  match n with
  | None -> []
  | Some node ->
      node.data :: preorder_node node.left @ preorder_node node.right 

let preorder (t: 'a bt) : 'a list =
  preorder_node t.root


(** [map f t] maps [f] to every element of [t].
*)
let rec map_node (f: 'a -> 'b) (n: 'a node option) : 'b node option =
  match n with
  | None -> None
  | Some node ->
      Some {
        data = f node.data;
        left = map_node f node.left;
        right = map_node f node.right
      }

let map (f: 'a -> 'b) (t: 'a bt) : 'b bt =
  {
    root = map_node f t.root;
    size = t.size
  }


(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let rec mirror_node (n: 'a node option) : unit =
  match n with
  | None -> ()
  | Some node ->
      let temp = node.left in
      node.left <- node.right;
      node.right <- temp;
      mirror_node node.left; 
      mirror_node node.right

let mirror (t: 'a bt) : unit =
  mirror_node t.root


(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let rec insert_node (e: 'a) (n: 'a node option) : 'a node option =
  match n with
  | None -> Some { data = e; left = None; right = None }
  | Some node ->
      if e < node.data then
        node.left <- insert_node e node.left
      else
        node.right <- insert_node e node.right;
      n

let add (e: 'a) (t: 'a bt) : unit =
  t.root <- insert_node e t.root;
  t.size <- t.size + 1

