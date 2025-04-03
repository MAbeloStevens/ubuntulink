(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Jaran Binning
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
let rec height : 'a bt -> int =
  fun t ->
  let rec h n =
      match n with
      | None -> 0
      | Some {data = _; left = l; right = r} ->
        1 + max (h l) (h r)
    in
    h t.root
  

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let rec mem : 'a -> 'a bt -> bool =
  fun e t  ->
  let rec helper n =
      match n with
      | None -> false
      | Some {data = d; left = l; right = r} ->
        if e = d then true
        else if e < d then helper l
        else helper r
    in
    helper t.root
  

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
    let rec helper n =
      match n with
      | None -> []
      | Some {data = d; left = l; right = r} ->
        d :: (helper l @ helper r)
    in
    helper t.root

(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f l ->
  let rec helper f n =
      match n with
      | None -> ()
      | Some node ->
        begin
          node.data <- f node.data;
          helper f node.left;
          helper f node.right
        end
      in helper f l.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
  let rec helper n =
    match n with
    | None -> ()
    | Some node ->
      begin
      let temp = node.left in
      node.left <- node.right;
      node.right <- temp;
      helper node.left;
      helper node.right
      end
    in helper t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
  match t.root with
  | None -> 
    t.root <- Some {data = e; left = None; right = None};
    t.size <- t.size + 1


