(*
Mutable state in OCaml
Quiz 6 - 2 April 202
Mutable fields in records: Binary Trees
Name 1: Nick Kogut
Name 2: Christopher Bernard
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
22 44
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
  let rec helper n =
    match n with
    | None -> 0
    | Some no -> 1 + max (helper no.left) (helper no.right)

  in helper t.root

    
(** [mem e t] returns a boolean indicating whether [e] is in [t].
Eg. [mem 23 t2 ==> false ]
Eg. [mem 33 t2 ==> true ]
*)

let mem : 'a -> 'a bt -> bool =
fun e t ->
  let rec helper e' t' = 
  match t' with
  | None -> false
  | Some no ->
    if no.data=e' then true else ((helper e' no.left) || (helper e' no.right))
  in helper e t.root
  



(** [preorder t] returns a list with the preorder traversal of [t].
Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
fun t ->
  let rec helper t' =
    match t' with
    | None -> []
    | Some h -> h.data :: (helper h.left) @ (helper h.right)

  in helper t.root

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
fun f t ->
  let rec helper f' t' =
    match t' with
    | None -> ()
    | Some n -> n.data <- (f' n.data); helper f' n.left; helper f' n.right

  in helper f t.root



(** [mirror t] updates [t] to its mirror image (left and right
children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
  let rec helper n =
    match n with
    | None -> ()
    | Some n ->
      let temp = n.left in
      let left = n.right in
      n.right <- temp;
      n.left <- left;
      helper (n.left);
      helper (n.right)
  in helper t.root

let add : 'a -> 'a bt -> unit =
fun e t ->
  let rec helper e' no =
    
    if e' < no.data then
      match no.left with
      | None -> no.left <- Some {data=e'; left=None; right=None}
      | Some n -> helper e' n
    else 
      match no.right with 
      | None -> no.right <- Some {data=e'; left=None; right=None}
      | Some n -> helper e' n
    in 
    match t.root with
    | None -> t.root <- Some {data=e; left=None; right=None}
    | Some n -> helper e n
    
