(* 
   Quiz 2 

   Name1: Aidan Cancelliere
   Name2: N/A
*)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let t1 = Node(30,Node(20,Node(10,Empty,Empty),Empty),Empty)
let t2 = Node(4,
              Node(3,
                   Node(2,
                        Node(1,Empty,Empty),
                        Empty),
                   Empty),
              Node(5,
                   Empty,
                   Node(6,
                        Empty,
                        Node(7,Empty,Empty))))

let t3 = Node(12,
              Node(7,Empty,Empty),
              Node(24,
                   Node(18,Empty,Empty),
                   Empty))

(** Implement [level t i] which returns a list with all the nodes in
   [t] at level [i]. You may assume that [i] is greater than 0.
   1 is the first level. 
   If the level is greater than the height of the tree, then 
   it should return the empty list.
   Eg.
# level t2 1 ==> [4]
# level t2 2 ==> [3; 5]
# level t2 3 ==> [2; 6] 
# level t2 33 ==> [] 
*)
let rec level t i =
   match t with
   | Empty -> []
   | Node (v, left, right) ->
      if i = 1 then [v] (* When i=1, we are at desired level *)
      else level left (i-1) @ level right (i-1) (* Else, decrement i and recursively search in both subtrees *)

(** Implement [levels t] which returns a list of lists, namely a list of the lists of nodes at each level. More precisely, the list at index i consists of all the items in [t] at level i+1.
   Eg. # levels t2 ==> [[4]; [3; 5]; [2; 6]; [1; 7]]
*)

(* A helper function to merge two lists of levels from left and right subtrees. *)
let rec helper_merge ls1 ls2 =
   match ls1, ls2 with
   | [], l | l, [] -> l (* If one of the lists is empty, return the other*)
   (* Combine the first level of each subtree by concatenating the lists, then recursively merge remaining*)
   | l1 :: ls1', l2 :: ls2' -> (l1 @ l2) :: helper_merge ls1' ls2'
                         
let rec levels t =
   match t with
   | Empty -> []
   | Node(v, left, right) ->
       [v] :: helper_merge (levels left) (levels right)

(** Implement [pbt h d] that generates a perfect binary tree of a given height whose nodes contain [d] as
    data. The height is [h] is an integer greater or equal to zero.
    Eg.
 pbt 3 "a" ==>
 Node ("a", Node ("a", Node ("a", Empty, Empty), Node ("a", Empty, Empty)),
 Node ("a", Node ("a", Empty, Empty), Node ("a", Empty, Empty)))
   Eg.
   # pbt 0 3 ==> Empty
*)
let rec pbt h d =
   if h = 0 then Empty
   else Node (d, pbt (h-1) d, pbt (h-1) d)
           
(** Implement [paths_to_leaves t] which returns a list with all the paths from the root to a leaf 
    Eg. 
    # paths_to_leaves t2 => [[0; 0; 0]; [1; 1; 1]] 
*)      
let rec paths_to_leaves t =
  match t with
  | Empty -> []
  | Node(d,Empty,Empty) ->  [[]]
  | Node(d,lt,rt) ->
      let left_paths = paths_to_leaves lt in
      let right_paths = paths_to_leaves rt in
      let left_paths2 = List.map (fun p -> 0 :: p) left_paths in
      let right_paths2 = List.map (fun p -> 1 :: p) right_paths in
      left_paths2 @ right_paths2
                         
(** Implement [paths t] which returns a list with all the paths from the root to any node. If the tree is empty, then paths returns the empty list.
    Eg. 
    # paths t2 => [[0; 0; 0]; [0; 0]; [0]; [1; 1; 1]; [1; 1];
    [1]; []]    
*)  


let rec paths t =
  match t with
  | Empty -> []
  | Node(d,Empty,Empty) -> [[]] (* A leaf, only path to itself is itself*)
  | Node (d, lt, rt) ->
   (* For the left subtree, add 0 to each path.
      For the right subtree, add 1 to each path.
      Then add the current node’s own path, which is [] *)
   let left_paths = List.map (fun p -> 0 :: p) (paths lt) in
   let right_paths = List.map (fun p -> 1 :: p) (paths rt) in
   left_paths @ right_paths @ [[]]
