(* 
     Quiz 2

     Name1: Jaran Binning
     Name2:

 *)

type 'a bt = Empty |  Node of 'a*'a bt*'a bt

(* Helper function that builds a leaf *)
let leaf n = Node(n,Empty,Empty)

(* Two sample binary trees.
   The first one is a BST, but not the second one *)
let t1 : int bt = Node(12,
                       Node(7,Empty,leaf 10),
                       Node(24,
                            leaf 14,
                            Empty))

let t2 : int bt = Node(12,
                       leaf 7,
                       Node(24,
                            leaf 30,
                            Empty))

(** returns smallest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec mint : 'a bt -> 'a =
  fun t ->
  match t with 
  | Empty -> failwith "Empty tree"
  | Node(n, Empty, Empty) -> n
  | Node(n, Empty, r) -> min n (mint r)
  | Node(n, l, Empty) -> min n (mint l)
  | Node(n, l, r) -> min (min n (mint l)) (mint r)

  
(** returns largest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec maxt : 'a bt -> 'a =
  fun t ->
    match t with 
    | Empty -> failwith "Empty tree"
    | Node(n, Empty, Empty) -> n
    | Node(n, Empty, r) -> max n (maxt r)
    | Node(n, l, Empty) -> max n (maxt l)
    | Node(n, l, r) -> max (max n (maxt l)) (maxt r)


let get_node : 'a bt -> 'a=
  fun n -> 
  match n with
  | Node(v,_,_) -> v
  | Empty -> 0


(** [is_bst t] determines whether the binary tree [t] is a binary search tree *)
let rec is_bst : 'a bt -> bool =
  fun t ->
  match t with
  | Empty -> true
  | Node(v, Empty, Empty) -> true
  | Node(v, Empty, r) -> if v < get_node r then is_bst r else false
  | Node(v, l, Empty) -> if v > get_node l then is_bst l else false
  | Node(v,l,r) -> if (v > get_node l) && (v < get_node r) 
                    then is_bst l && is_bst r 
                    else false

(** [add k t] adds key [k] to the bst [t]. 
    Should fail with failwith if [k] is already in the tree.
    Otherwise, returns updated tree *)
let rec add : 'a -> 'a bt -> 'a bt =
  fun k t ->
  match t with
  | Empty -> leaf k
  | Node(v, l, r) -> if k = v then failwith "Key already in tree"
                     else if k < v then Node(v, add k l, r)
                     else Node(v, l, add k r)
           
(** [rem k t] removes key [v] from the BST [t] 
   Should fail with failwith if [k] is not in tree
   (Extra-credit) *)
let rec rem : 'a -> 'a bt -> 'a bt =
   fun k t ->
   match t with
   | Empty -> failwith "Key not in tree"
   | Node(v, l, r) -> if k = v then t else t

 





