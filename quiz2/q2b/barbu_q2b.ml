(* 
     Quiz 2

     Name1: Jacob Barbulescu
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


(** Returns a list of every node value in the tree *)
let rec getNodeValues tree = 
  match tree with
  | Empty -> []
  | Node(n, l ,r) -> n::(getNodeValues l)@(getNodeValues r)

(** Returns the minimum value of a list *)
let rec getMin list min =
  match list with 
  | [] -> min
  | h::t ->
    if h < min
      then getMin t h
      else getMin t min

(** Returns the max value of a list *)
let rec getMax list max =
  match list with 
  | [] -> max
  | h::t ->
    if h > max
      then getMax t h
      else getMax t max


(** returns smallest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec mint : 'a bt -> 'a =
  fun t ->
  match t with
  | Empty -> failwith "Empty tree"
  | Node(n, l ,r) -> getMin (getNodeValues t) n
  
(** returns largest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec maxt : 'a bt -> 'a =
  fun t ->
    match t with
    | Empty -> failwith "Empty tree"
    | Node(n, l ,r) -> getMax (getNodeValues t) n

(** Finds the min value of the bst. If empty, return passed in default+1 *)
let rec mintBST : 'a bt -> 'b -> 'a =
  fun t default ->
  match t with
  | Empty -> default+1
  | Node(n, l ,r) -> getMin (getNodeValues t) n
  
(** Finds the max value of the bst. If empty, return passed in default-1 *)
let rec maxtBST : 'a bt -> 'b -> 'a =
  fun t default ->
    match t with
    | Empty -> default-1
    | Node(n, l ,r) -> getMax (getNodeValues t) n

(** [is_bst t] determines whether the binary tree [t] is a binary search tree *)
let rec is_bst : 'a bt -> bool =
  fun t ->
  match t with
  | Empty -> true
  | Node(n, l ,r) -> if (maxtBST l n) < n && (mintBST r n) > n then (is_bst l) && (is_bst r) else false

(** [add k t] adds key [k] to the bst [t]. 
    Should fail with failwith if [k] is already in the tree.
    Otherwise, returns updated tree *)
let rec add : 'a -> 'a bt -> 'a bt =
  fun k t ->
  match t with
  | Empty -> (leaf k)
  | Node(n, l, r) -> if k = n then failwith "Key already present" else
      if k < n then Node(n, (add k l), r) else Node(n, l, (add k r))
           
(** [rem k t] removes key [v] from the BST [t] 
   Should fail with failwith if [k] is not in tree
   (Extra-credit) *)
(*let rec rem : 'a -> 'a bt -> 'a bt =
   fun k t ->
   match t with ->
   | Empty -> failwith "Key not in tree"
   | Node(n, l, r) -> if (k = n) then removeNode else
                        if (k < n) then Node(n, rem k l, r) else Node(n, l, rem k r)*)
 
(** the top node of a tree *)




