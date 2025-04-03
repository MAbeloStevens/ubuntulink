(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Sahana Ali
    Name 2: Nicole Mak
    I pledge my honor that I have abided by the Stevens Honor System. 
*)

type 'a node = { mutable data: 'a;
                 mutable left: 'a node option;
                 mutable right: 'a node option }

type 'a bt = { mutable root: 'a node option;
               mutable size: int }

let t1 : int bt =
  { root = None;
    size = 0 }

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
    let rec helper no =
      match no with
      | None -> 0
      | Some n -> 1 + max (helper n.left) (helper n.right)
    in
    helper t.root

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
  fun e t ->
    let rec helper no =
      match no with
      | None -> false
      | Some n ->
        if n.data = e then true
        else helper n.left || helper n.right
    in
    helper t.root


(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
    let rec helper no =
      match no with
      | None -> []
      | Some n -> n.data :: (helper n.left @ helper n.right)
    in
    helper t.root
  
(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
    let rec helper no =
      match no with
      | None -> ()
      | Some n ->
        n.data <- f n.data;  
        helper n.left;       
        helper n.right       
    in
    helper t.root
    

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
    let rec helper no =
      match no with
      | None -> ()
      | Some n ->
        let temp = n.left in
        n.left <- n.right;
        n.right <- temp;
        helper n.left;
        helper n.right
    in
    helper t.root
  

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
    let rec helper no =
      match no with
      | None -> ()  
      | Some n ->
        if e < n.data then
          match n.left with
          | None -> n.left <- Some { data = e; left = None; right = None } 
          | Some _ -> helper n.left  
        else if e > n.data then
          match n.right with
          | None -> n.right <- Some { data = e; left = None; right = None } 
          | Some _ -> helper n.right  
        else 
          ()  
    in
   
    match t.root with
    | None -> t.root <- Some { data = e; left = None; right = None }  
    | Some _ -> helper t.root 
