(* 
     Quiz 2

     Name1: Sahana Ali
     Name2: Nicole Mak

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
 let rec mint = function
   | Empty -> failwith "Fail"
   | Node (t, Empty, Empty) -> t
   | Node (t, Empty, rt) -> min t (mint rt)
   | Node (t, lt, Empty) -> min t (mint lt)
   | Node (t, lt, rt) -> min t (min (mint lt) (mint rt))
   
 (** returns largest element in non-empty tree [t].
     Fails if [t] is empty, it should fail. 
     Note: the tree [t] is not assumed to be a bst *)
 let rec maxt = function
   | Empty -> failwith "Fail"
   | Node (t, Empty, Empty) -> t
   | Node (t, Empty, rt) -> max t (maxt rt)
   | Node (t, lt, Empty) -> max t (maxt lt)
   | Node (t, lt, rt) -> max t (max (maxt lt) (maxt rt))
 
 
 
 (** [is_bst t] determines whether the binary tree [t] is a binary search tree *)
 (*doesn't complie, however logic is there*)
(*let rec is_bst = function 
   match t with
   | Empty -> failwith "Fail"
   | Node(t, Empty, Empty)-> true
   | Node(t, lt, Empty) -> t > maxt lt && is_bst lt
   | Node(t, Empty, rt) -> t < mint rt && is_bst rt
   | Node(t, lt, rt) -> t > maxt lt && d < mint rt && is_bst lt
   && is_bst rt *)
 

(** [add k t] adds key [k] to the BST [t]. 
    Should fail with failwith if [k] is already in the tree.
    Otherwise, returns the updated tree *)
let rec add : 'a -> 'a bt -> 'a bt =
  fun k t ->
    match t with
    | Empty -> Node(k, Empty, Empty)  (* Insert new key as a leaf *)
    | Node(v, lt, rt) ->
        if k = v then failwith "Key already exists in the tree"
        else if k < v then Node(v, add k lt, rt)  (* Recur into left subtree *)
        else Node(v, lt, add k rt)  (* Recur into right subtree *)

            
 (** [rem k t] removes key [v] from the BST [t] 
    Should fail with failwith if [k] is not in tree
    (Extra-credit) *)
 let rec rem : 'a -> 'a bt -> 'a bt =
    fun k t ->
    failwith "Fail"
  
 
 
 
 
 
 