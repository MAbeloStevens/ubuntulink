(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Gustavo Balaguera
    Name 2:
*)
(* I pledge my hononr that I have abided by the Stevens Honor System. *)


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
  let rec helper n =
    match n with
    | None -> 0
    | Some {data = _; left = l; right = r} ->
      let hl = helper l in
      let hr = helper r in
      1 + max hl hr
  in
  helper t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
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
  fun f t ->
    let rec map_ no =
      match no with
    | None -> ()
    | Some n ->
    begin
    n.data <- f n.data;
    map_ n.left;
    map_ n.right
    end
    in map_ t.root

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
    let rec mirror_ n =
      match n with
      | None -> ()
      | Some n ->
        let temp = n.left in
        n.left <- n.right;
        n.right <- temp;
        mirror_ n.left;
        mirror_ n.right
    in
    mirror_ t.root

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
    let rec add_ n =
      match n with
      | None -> ()
      | Some n ->
        if e < n.data then
          match n.left with
          | None -> t.size <- t.size + 1;
                    n.left <- Some {data = e; left = None; right = None}
          | Some _ -> add_ n.left
        else
          match n.right with
          | None -> t.size <- t.size + 1;
                    n.right <- Some {data = e; left = None; right = None}
          | Some _ -> add_ n.right
    in
    add_ t.root
