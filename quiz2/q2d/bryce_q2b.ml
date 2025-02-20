(* 
     Quiz 2

     Name1: William Bryce
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
  | Empty -> failwith "mint: empty tree"
  | Node(d,Empty,Empty) -> d
  | Node(d,lt,Empty) -> min d (mint lt)
  | Node(d,Empty,rt) -> min d (mint rt)
  | Node(d,lt,rt) -> min d (min (mint lt) (mint rt))
  (*failwith "implement"*)
  
(** returns largest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec maxt : 'a bt -> 'a =
  fun t ->
  match t with
  | Empty -> failwith "maxt: empty tree"
  | Node(d,Empty,Empty) -> d
  | Node(d,lt,Empty) -> max d (maxt lt)
  | Node(d,Empty,rt) -> max d (maxt rt)
  | Node(d,lt,rt) -> max d (max (maxt lt) (maxt rt))
  (*failwith "implement"*)

(** [is_bst t] determines whether the binary tree [t] is a binary search tree *)
let rec is_bst : 'a bt -> bool =
  fun t ->
  match t with
  | Empty -> true
  | Node(d,Empty,Empty) -> true
  | Node(d,lt,Empty) -> (d > maxt lt) && (is_bst lt)
  | Node(d,Empty,rt) -> (d < mint rt) && (is_bst rt)
  | Node(d,lt,rt) -> (d > maxt lt) && (d < mint rt) && (is_bst lt) && (is_bst rt)
  (*failwith "implement"*)

(** [add k t] adds key [k] to the bst [t]. 
    Should fail with failwith if [k] is already in the tree.
    Otherwise, returns updated tree *)
(*let rec add : 'a -> 'a bt -> 'a bt =
  fun k t ->
  match t with
  | Empty -> leaf k
  | Node(d,Empty,Empty) -> 
    if k = d
    then failwith "add: key already in tree"
    else if k > d
    
  failwith "implement"*)
           
(** [rem k t] removes key [v] from the BST [t] 
   Should fail with failwith if [k] is not in tree
   (Extra-credit) *)
let rec rem : 'a -> 'a bt -> 'a bt =
   fun k t ->
   failwith "implement"
 





