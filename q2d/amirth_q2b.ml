(* 
     Quiz 2

     Name1: Harishan Amirthanathan
     Name2: Jaden Fernandes

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
    | Empty -> failwith "maxt: empty tree"
    | Node(d,Empty,Empty) -> d
    | Node(d,lt,Empty) -> min d (mint lt)
    | Node(d,Empty,rt) -> min d (mint rt)
    | Node(d,lt,rt) -> min d (min (mint lt) (mint rt))
  
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
  

(** [is_bst t] determines whether the binary tree [t] is a binary search tree *)
let rec is_bst : 'a bt -> bool =
  fun t ->
    match t with
    | Empty -> true
    | Node(d,lt,rt) ->
       let left =
          match lt with
          | Empty -> true
          | _ -> maxt lt < d
        in
        let right =
          match rt with
          | Empty -> true
          | _ -> mint rt > d
        in
        left && right && is_bst lt && is_bst rt

(** [add k t] adds key [k] to the bst [t]. 
    Should fail with failwith if [k] is already in the tree.
    Otherwise, returns updated tree *)
let rec add : 'a -> 'a bt -> 'a bt =
  fun k t ->
    match t with
  | Empty -> Node(k, Empty, Empty)
  | Node(d, lt, rt) ->
      if k = d then
        failwith "Key already in tree"
      else if k < d then
        Node(d, add k lt, rt)
      else
        Node(d, lt, add k rt)
    
(** [rem k t] removes key [v] from the BST [t] 
   Should fail with failwith if [k] is not in tree
   (Extra-credit) *)
let rec rem : 'a -> 'a bt -> 'a bt =
   fun k t ->
    match t with
    | Empty -> failwith "element not found"
    | Node (d, lt, rt) ->
      if k < d then Node(d,rem k lt,rt)
      else if k > d then Node(d,lt,rem k rt)
      else 
        match lt,rt with
        | Empty,Empty -> Empty
        | Empty ,_ -> rt
        | _, Empty -> lt
        | _ -> 
          let min_right = mint rt in 
          Node(min_right,lt,rem min_right rt)


        
 





